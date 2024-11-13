import geopandas as gpd
import numpy as np
import pandas as pd


def other_tags(row, geometry_type):
    pairs = row.split('","')
    
    if geometry_type  == 'polygon':
        info_columns = ["building", "amenity", "leisure", "tourism", "office", "shop", "landuse", 
                        "addr:city", "addr:country", "addr:housenumber", "addr:street", "addr:postcode", "building:levels", 
                        "building:material", "roof:material", "name"]
    
    elif geometry_type == 'point':
        info_columns = ["building", "amenity", "leisure", "tourism", "office", "shop", 
                        "addr:city", "addr:country", "addr:housenumber", "addr:street", "addr:postcode", "name"]

    final_columns = info_columns.copy()
    final_columns.append("osm_id")
    if geometry_type == 'polygon':
        final_columns.append("osm_way_id")
    final_columns.append("geometry")
    
    result_dict = {}
    
    for c in info_columns:
        result_dict[c] = np.nan
    
    for pair in pairs:
        pair = pair.replace('"', '')
        key, value = pair.split('=>')
        
        
        if key in final_columns and value != "yes":
            result_dict[key] = value
    
    return result_dict

def write(geojson_path, final_path, geometry_type):

    # Read GeoJSON file into a GeoDataFrame
    gdf = gpd.read_file(geojson_path)
    
    # polygons
    if geometry_type  == 'polygon':
        info_columns = ["building", "amenity", "leisure", "tourism", "office", "shop", "landuse", 
                        "addr:city", "addr:country", "addr:housenumber", "addr:street", "addr:postcode", "building:levels", 
                        "building:material", "roof:material", "name"]
    
    elif geometry_type == 'point':
        info_columns = ["building", "amenity", "leisure", "tourism", "office", "shop", 
                        "addr:city", "addr:country", "addr:housenumber", "addr:street", "addr:postcode", "name"]

    final_columns = info_columns.copy()
    final_columns.append("osm_id")
    if geometry_type == 'polygon':
        final_columns.append("osm_way_id")
    final_columns.append("geometry")

    
    # filter out entries where no other information is available
    final_gdf = gdf.dropna(subset=["other_tags"])
    
    # filter out entries where other_tags does not contain the information desired
    final_gdf = final_gdf[final_gdf["other_tags"].str.contains('|'.join(final_columns).replace('landuse|','').replace("|name",''))]
    
    result_df = pd.DataFrame(final_gdf['other_tags'].apply(other_tags, args=(geometry_type,)).tolist())
    
    result_df.reset_index(drop=True, inplace=True)
    final_gdf.reset_index(drop=True, inplace=True)
    
    final_gdf = pd.concat([final_gdf, result_df], axis=1)
    
    if geometry_type == 'polygon':
        mask = final_gdf["landuse"].isna()
        final_gdf = final_gdf[mask]

    final_gdf = final_gdf[final_columns]
    
    final_gdf = final_gdf.dropna(subset=info_columns, how='all')
    
    final_gdf.to_file(final_path, driver="GML")
    
# input GML path
input_file = 

# output GML path
output_file = 

# point or polygon type
geometry_type = 

write(input_file, output_file, geometry_type)



