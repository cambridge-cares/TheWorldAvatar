import json
import geopandas as gpd # for coordinate system change
import sys


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

def feature_intersection_extended(feature_polygons,feature_points,crs1,crs2, file_name):
	geo_df_1 = gpd.GeoDataFrame.from_features(feature_polygons,crs=crs1)
	geo_df_2 = gpd.GeoDataFrame.from_features(feature_points,crs=crs2)
	if crs1 != 'EPSG:4326':
		geo_df_1 = geo_df_1.to_crs("EPSG:4326")
	if crs2 != 'EPSG:4326':
		geo_df_2 = geo_df_2.to_crs("EPSG:4326")
	overlap  = gpd.overlay(geo_df_2,geo_df_1)
	try:
		overlap.to_file(file_name+'_intersection.geojson',driver="GeoJSON")
	except ValueError as err:
		print('The following value error occurred: ', err)
	return


def coordinate_conversion(file):
	features,crs = open_features(file)
	geo_df = gpd.GeoDataFrame.from_features(features,crs=crs)
	geo_df = geo_df.to_crs("EPSG:4326")
	geo_df.to_file(file,driver="GeoJSON")
	return 


if __name__ == '__main__':
	if len(sys.argv) == 1:
		print('For HELP, run> crop_map_intersector -h')
	if len(sys.argv) == 2 and str(sys.argv[1]) in '-h':
		print(
			'To run the intersector, provide a command as follows:>crop_map_intersector.py [OUTPUT_FILE_START_ID] [STARTING_FEATURE_NUMBER] [UPPER_LIMIT]\n')
		print(
			'An example comm>crop_map_intersector 147 43801 103800')
	if len(sys.argv) == 4:
		features ,crs = open_features('Flood_Map_for_Planning_Rivers_and_Sea_Flood_Zone_3.json')
		# flood_features = get_flood_features(features)
		crop_map_features,crs_crop_maps = open_features('Crop_Map.geojson')
		features_crop_map = []
		fileid = int(sys.argv[1])
		counter = 0
		for offtake_feature in crop_map_features:
			counter = counter + 1
			if counter<int(sys.argv[2]):
				continue
			features_crop_map.append(offtake_feature)
			if counter%300 == 0:
				print('Processing crop map objects ...')
				feature_intersection_extended(features,features_crop_map,crs,crs_crop_maps, 'crop_map_data/'+str(fileid))
				print('Processing crop map objects ..., so far intersected ', counter)
				fileid = fileid + 1
				features_crop_map = []
			if counter>=int(sys.argv[3]):
				break
		if counter%300 != 0:
			print('Processing crop map objects ...')
			feature_intersection_extended(features,features_crop_map,crs,crs_crop_maps, 'crop_map_data/'+str(fileid))
			print('Processing crop map objects ..., so far intersected ', counter)
			fileid = fileid + 1
			features_crop_map = []


		box = return_bounding_box(features,crs)
