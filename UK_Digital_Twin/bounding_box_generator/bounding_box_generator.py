from shapely.geometry import shape, GeometryCollection
import json 
import geopandas as gpd # for coordinate system change
import matplotlib.pyplot as plt

def return_bounding_box(file):
	'''
	returns (minx,miny,maxx,maxy)
	'''
	with open(file) as f:
		all = json.load(f)
	features = all["features"]
	crs = all["crs"]["properties"]["name"]
	geo_df = gpd.GeoDataFrame.from_features(features,crs=crs)
	geo_df = geo_df.to_crs("EPSG:4326")

	return geo_df.total_bounds

box = return_bounding_box('bounding_box_generator/Flood_Map_for_Planning_Rivers_and_Sea_Flood_Zone_3.json')
print(box)