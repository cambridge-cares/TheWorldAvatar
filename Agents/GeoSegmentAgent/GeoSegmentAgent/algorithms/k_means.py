import numpy as np
from sklearn.cluster import KMeans
import rasterio
from rasterio.features import shapes
import geopandas as gpd
import logging, os

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

def raster_to_geojson(input_file_labels, averages, output_file):
    with rasterio.open(input_file_labels) as src_labels:
        # read the raster data
        data_labels = src_labels.read(1)

        # create shapes (polygons) from the raster
        results = (
            {'properties': {'cluster_index': int(v), 'raster_val': averages[int(v)]}, 'geometry': s}
            for (s, v), _
            in zip(shapes(data_labels, mask=None, transform=src_labels.transform), np.ndindex(data_labels.shape)))

        # write the shapes to a geojson file
        geoms = list(results)
        gpd_polygonized_raster = gpd.GeoDataFrame.from_features(geoms, crs=src_labels.crs)
        gpd_polygonized_raster.to_file(output_file, driver='GeoJSON')


