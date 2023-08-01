import numpy as np
from sklearn.cluster import KMeans
import rasterio
from rasterio.features import shapes
import geopandas as gpd
import logging, os, uuid, json

def raster_to_clusters(input_file, n_clusters):
    with rasterio.open("./GeoSegmentAgent/raster_input/" + input_file) as src:
        # read the raster data
        data = src.read()

        # reshape the data to a 1D array
        reshaped_data = np.reshape(data, -1).reshape(-1, 1)

        # fit and predict clusters
        kmeans = KMeans(n_clusters=n_clusters, random_state=0).fit(reshaped_data)
        labels = kmeans.labels_

        # calculate cluster averages
        averages = [np.mean(reshaped_data[labels == i]) for i in range(n_clusters)]

        # reshape labels back to original shape
        labels_reshaped = np.reshape(labels, (src.height, src.width))

        # write the labels to a new raster
        with rasterio.open('clusters.tif', 'w', driver='GTiff',
                           height=src.height, width=src.width, count=1,
                           dtype='int32',
                           crs=src.crs, transform=src.transform) as dst:
            dst.write(labels_reshaped, 1)

        return averages, labels_reshaped

def raster_to_geojson(input_file_labels, averages, output_file, value_to_object_dict):
    with rasterio.open(input_file_labels) as src_labels:
        # read the raster data
        data_labels = src_labels.read(1)

        # create shapes (polygons) from the raster
        results = (
            {'properties': {'cluster_index': int(v),
                            'meanOfCluster': averages[int(v)],
                            'Polygon': 'http://www.opengis.net/ont/gml#Polygon/' + str(int(v)) + str(uuid.uuid4()),
                            'Feature': 'http://www.opengis.net/ont/geosparql#Feature/' + str(int(v)) + str(uuid.uuid4()),
                            'Segmentation':'https://www.theworldavatar.com/kg/Segmentation/' + str(int(v)) + str(uuid.uuid4()),
                            'SegmentationModel':'KMeansClusteringModel',
                            'representsRealWorldObject': 'placeholder',
                            }, 'geometry': s}
            for (s, v), _
            in zip(shapes(data_labels, mask=None, transform=src_labels.transform), np.ndindex(data_labels.shape)))

        if value_to_object_dict != None:
            # Converts the input parameter string to a list of dict
            # E.g., '[{"10":"tree"}, {"20":"building"}, {"30":"road"}]' to a list of dict
            results = add_real_world_object(list(results), value_to_object_dict)

        # write the shapes to a geojson file
        logging.error(results)  # <--- check here what is in the results
        geoms = list(results)
        gpd_polygonized_raster = gpd.GeoDataFrame.from_features(geoms, crs=src_labels.crs)
        gpd_polygonized_raster.to_file(output_file, driver='GeoJSON')


def add_real_world_object(results, value_to_object_dict):
    for result in results:
        mean_of_cluster = result['properties']['meanOfCluster']
        closest_key = min(value_to_object_dict.keys(), key=lambda k: abs(k - mean_of_cluster))
        result['properties']['representsRealWorldObject'] = value_to_object_dict[closest_key]
    return results

