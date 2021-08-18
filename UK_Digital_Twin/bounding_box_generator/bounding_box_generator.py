from shapely.geometry import shape, GeometryCollection
import json 
import geopandas as gpd # for coordinate system change
import matplotlib.pyplot as plt


def open_features(file):
	with open(file) as f:
		all = json.load(f)
	features = all["features"]
	try:
		crs = all["crs"]["properties"]["name"]
	except:
		# in the absense of crs info assume WGS84
		crs = 'EPSG:4326'
	return features, crs

def return_bounding_box(features,crs):
	'''
	changes coordinates to wgs84 and
	returns (minx,miny,maxx,maxy)
	'''
	geo_df = gpd.GeoDataFrame.from_features(features,crs=crs)
	geo_df = geo_df.to_crs("EPSG:4326")
	return geo_df.total_bounds

def get_flood_features(features):
	flood_property = "Fluvial / Tidal Models"
	flood_features = []
	for i in range(len(features)):
		if features[i]['properties']['type'] == flood_property:
			flood_features.append(features[i])
	return flood_features

def feature_intersection(feature_polygons,feature_points,crs1,crs2):
	geo_df_1 = gpd.GeoDataFrame.from_features(feature_polygons,crs=crs1)
	geo_df_2 = gpd.GeoDataFrame.from_features(feature_points,crs=crs2)
	if crs1 != 'EPSG:4326':
		geo_df_1 = geo_df_1.to_crs("EPSG:4326")
	if crs2 != 'EPSG:4326':
		geo_df_2 = geo_df_2.to_crs("EPSG:4326")
	overlap  = gpd.overlay(geo_df_2,geo_df_1)
	overlap.to_file('intersection.geojson',driver="GeoJSON")
	return 

def coordinate_conversion(file):
	features,crs = open_features(file)
	geo_df = gpd.GeoSeries.from_features(features,crs=crs)
	geo_df = geo_df.to_crs("EPSG:4326")
	geo_df.to_file(file,driver="GeoJSON")
	return 



features ,crs = open_features('Flood_Map_for_Planning_Rivers_and_Sea_Flood_Zone_3.json')
flood_features = get_flood_features(features)
offtake_features,crs_offtakes = open_features('pipe_network.geojson')

feature_intersection(flood_features,offtake_features,crs,crs_offtakes)


box = return_bounding_box(flood_features,crs)
