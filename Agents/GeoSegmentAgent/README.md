# GeoSegment Agent

This agent is designed to run segmentation algorithms to segment raster data input and create a virtual knowledge graph using Ontop and OBDA mapping.

As of its latest development, the GeoSegment Agent is able to perform the following tasks:
1. Executes value-based segmentation on raster data using the k-means clustering algorithm. 
2. Performs semantic segmentation using the Semantic Anything Model (SAM) from Meta.

## 1. Data Preparation
The agent requires raster data input in GeoTIFF format. The raster data input can be obtained from a variety of sources, such as satellite imagery, aerial photography, or drone imagery. Ensure that the data is accessible to the docker container by placing it into `raster_input` folder.
The raster data must be pre-processed and converted to the GeoTIFF.

## 2. Build the Image

To build the Docker image for the GeoSegment Agent, follow these steps:

1. Navigate to the directory containing the Dockerfile for the GeoSegment Agent.
2. Execute the following command to build the Docker image:

```
docker build -t geosegment_agent:1.0.0 .
```

This will use the Dockerfile to build an image named `geosegment_agent` tagged with `1.0.0`.

### Note: The GeoSegment Agent can only run within a Docker stack.
The stack is spun up by [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager), which is beyond the scope of this README.

A successful setup will result in 9 containers (or more):
- Default containers
    - Stack Manager (exits when spins up all other containers)
    - Blazegraph
    - Nginx
    - Postgis
    - Adminer
    - Ontop
    - Gdal
    - Geoserver
- GeoSegmentAgent

## 3. Run the Agent

Once the GeoSegment Agent is successfully spun up within the Docker stack, you can send requests to convert raster data into a virtual knowledge graph. 

### K-Means Clustering Parameters

- `n_clusters`: The number of clusters to form and the number of centroids to generate.
- `file`: The filename of the GeoTIFF format raster data (that is available within the container).
- `tablename`: The name of the table to store segmented data in the PostGIS database.
- `value_to_object_dict`: (Optional) A dictionary that maps raster values to object labels (e.g., `{10: "tree", 20: "building"}`).

Example API call to perform k-means clustering:

```
curl -X GET http://localhost:5000/k-means-cluster-segmentation?n_clusters=3&file=my_raster_data.tif&tablename=my_table
```

### Semantic Segmentation Parameters (Using SAM Model)

- `minLon`, `minLat`, `maxLon`, `maxLat`: Coordinates for the bounding box to segment within the raster data.
- `tablename`: The name of the table in the PostGIS database to store the segmentation results.

Example API call to perform SAM semantic segmentation:

```
curl -X GET "http://localhost:5000/SAM-semantic-segmentation?minLon=xxx&minLat=yyy&maxLon=zzz&maxLat=www&tablename=my_table"
```
### Update OBDA Mapping
Finally, upload the obda file to the Ontop container by running the following command:

```
curl -X http://localhost:3838/geosegment-agent/update-obda
```

Ensure that the GeoTIFF files, table names, and other parameters are specified correctly for your particular segmentation task.

By correctly specifying these input parameters, you can achieve different segmentation results based on the utilized segmentation algorithm (either k-means clustering or SAM semantic segmentation), the characteristics of the input raster data, and your desired output in terms of labeled segments.