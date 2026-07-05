"""
Pull road inventory data from KYTC and ODOT ArcGIS REST endpoints.
  - KYTC: posted speed limits (state + city streets) for Boone/Kenton/Campbell
  - KYTC: lane counts from existing LRS shapefile (state roads only)
  - ODOT: full road inventory for Hamilton County — speed, lanes, width,
           bike lane, function class, ADT
"""

import time
from pathlib import Path
import requests
import pandas as pd
import geopandas as gpd
from pyproj import Transformer

DATA = Path("data")
DATA.mkdir(exist_ok=True)

HEADERS = {"User-Agent": "Mozilla/5.0 (research; contact benedict.r.leonardi@gmail.com)"}
BBOX_4326 = {"xmin": -84.82, "ymin": 38.95, "xmax": -84.20, "ymax": 39.35}

t = Transformer.from_crs("EPSG:4326", "EPSG:3857", always_xy=True)
xmin, ymin = t.transform(BBOX_4326["xmin"], BBOX_4326["ymin"])
xmax, ymax = t.transform(BBOX_4326["xmax"], BBOX_4326["ymax"])
BBOX_3857 = {"xmin": xmin, "ymin": ymin, "xmax": xmax, "ymax": ymax}


def fetch_json_attributes(url, params, page_size=1000):
    """Page through ArcGIS f=json, return list of attribute dicts (no geometry)."""
    params = {**params, "f": "json", "returnGeometry": "false"}
    all_rows = []
    offset = 0
    while True:
        params["resultOffset"] = offset
        params["resultRecordCount"] = page_size
        r = requests.get(f"{url}/query", params=params, headers=HEADERS, timeout=60)
        r.raise_for_status()
        d = r.json()
        if "error" in d:
            raise RuntimeError(d["error"])
        features = d.get("features", [])
        all_rows.extend(f["attributes"] for f in features)
        print(f"  {len(all_rows)} records fetched...")
        if len(features) < page_size:
            break
        offset += page_size
        time.sleep(0.25)
    return all_rows


def fetch_geojson(url, params, page_size=1000):
    """Page through ArcGIS f=geojson, return GeoDataFrame."""
    params = {**params, "f": "geojson", "returnGeometry": "true", "outSR": "4326"}
    all_features = []
    offset = 0
    while True:
        params["resultOffset"] = offset
        params["resultRecordCount"] = page_size
        r = requests.get(f"{url}/query", params=params, headers=HEADERS, timeout=60)
        r.raise_for_status()
        d = r.json()
        if "error" in d:
            raise RuntimeError(d["error"])
        features = d.get("features", [])
        all_features.extend(features)
        print(f"  {len(all_features)} features fetched...")
        if len(features) < page_size:
            break
        offset += page_size
        time.sleep(0.25)
    if not all_features:
        return gpd.GeoDataFrame()
    fc = {"type": "FeatureCollection", "features": all_features}
    return gpd.GeoDataFrame.from_features(fc, crs="EPSG:4326")


# ── KYTC: Speed limits (attributes only — LRS table, no useful line geometry) ─
print("Fetching KYTC speed limits...")
KYTC_SPEED = "https://kygisserver.ky.gov/arcgis/rest/services/WGS84WM_Services/Ky_Speed_Limits_WGS84WM/MapServer/0"

rows = fetch_json_attributes(KYTC_SPEED, {
    "geometry": f"{BBOX_3857['xmin']},{BBOX_3857['ymin']},{BBOX_3857['xmax']},{BBOX_3857['ymax']}",
    "geometryType": "esriGeometryEnvelope",
    "inSR": "3857",
    "spatialRel": "esriSpatialRelIntersects",
    "outFields": "RT_UNIQUE,RT_DESCR,CNTY_NAME,D_GOV_LEVEL,GOV_LEVEL,SPEEDLIM,MILES,D_DISTRICT",
})

kytc_speed = pd.DataFrame(rows)
print(f"KYTC speed: {len(kytc_speed)} segments")
if not kytc_speed.empty:
    print(f"  counties: {kytc_speed['CNTY_NAME'].value_counts().to_dict()}")
    print(f"  speed range: {kytc_speed['SPEEDLIM'].min()}–{kytc_speed['SPEEDLIM'].max()} mph")
    kytc_speed.to_csv(DATA / "kytc_speed.csv", index=False)
    print("Saved: data/kytc_speed.csv")

# ── KYTC: Lane data from existing LRS shapefile ───────────────────────────────
print("\nLoading KYTC lane data from LRS shapefile...")
KYTC_LN = Path("../Street_Type_Divergence/data/raw/kytc_ln/LN.shp")

kytc_ln = gpd.read_file(KYTC_LN).to_crs("EPSG:4326")
kytc_nky = kytc_ln[kytc_ln["CNTY_NAME"].isin(["Boone", "Kenton", "Campbell"])].copy()
print(f"KYTC lanes (NKy): {len(kytc_nky)} state-road segments")
print(f"  lane range: {kytc_nky['LANES'].min()}–{kytc_nky['LANES'].max()}")
print(f"  lane width range: {kytc_nky['LANEWID'].min()}–{kytc_nky['LANEWID'].max()} ft")
kytc_nky.to_file(DATA / "kytc_lanes_nky.gpkg", driver="GPKG")
print("Saved: data/kytc_lanes_nky.gpkg")

# ── ODOT: Road inventory (Hamilton County) ────────────────────────────────────
print("\nFetching ODOT road inventory (Hamilton County)...")
ODOT_INV = "https://tims.dot.state.oh.us/ags/rest/services/Roadway_Information/Road_Inventory/MapServer/0"

odot = fetch_geojson(ODOT_INV, {
    "geometry": f"{BBOX_4326['xmin']},{BBOX_4326['ymin']},{BBOX_4326['xmax']},{BBOX_4326['ymax']}",
    "geometryType": "esriGeometryEnvelope",
    "inSR": "4326",
    "spatialRel": "esriSpatialRelIntersects",
    "where": "COUNTY_CD='HAM'",
    "outFields": ",".join([
        "STREET_NAME", "STREET_SUFFIX_CD",
        "COUNTY_CD", "FUNCTION_CLASS_CD",
        "LANES_NBR", "LANE_WIDTH_NBR", "ROADWAY_WIDTH", "THROUGH_LANES",
        "SPEED_LIMIT_NBR",
        "DIVIDED_HWY_IND", "MEDIAN_TYPE_CD", "MEDIAN_WIDTH_NBR",
        "BIKE_LANE_TYPE_CD", "BIKE_LANE_WIDTH_NBR",
        "SHOULDER_TYPE_CD", "SHOULDER_TOTAL_WIDTH_LT", "SHOULDER_TOTAL_WIDTH_RT",
        "SURFACE_TYPE_LEFT_CD",
        "ADT_TOTAL_NBR", "ADT_YEAR_NBR",
        "JURISDICTION_CD", "ACCESS_CONTROL_CD",
        "SEGMENT_LENGTH_NBR",
    ]),
})

print(f"ODOT road inventory: {len(odot)} segments")
if not odot.empty:
    print(f"  speed range: {odot['SPEED_LIMIT_NBR'].min()}–{odot['SPEED_LIMIT_NBR'].max()} mph")
    print(f"  lanes range: {odot['LANES_NBR'].min()}–{odot['LANES_NBR'].max()}")
    print(f"  bike lane types: {odot['BIKE_LANE_TYPE_CD'].value_counts().to_dict()}")
    odot.to_file(DATA / "odot_inventory.gpkg", driver="GPKG")
    print("Saved: data/odot_inventory.gpkg")

print("\nDone.")
