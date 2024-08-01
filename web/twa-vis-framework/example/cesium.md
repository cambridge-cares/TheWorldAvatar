# Example CesiumJS Visualisation

This example visualisation has been put together to demonstrate the intended use of the centralised TWA Visualisation Framework (TWA-VF). This framework has been designed to make it easier for users not experienced with Typescript (or the mapping libraries) to quickly & easily put together a new TWA visualisation. It is intended for developers to use this example visualisation to gain an understanding of the TWA-VF before attempting to create their own visualisation; to do that, this example can be copied and used as a starting point.

It is recommended that you read the through the [TWA-VF Overview](../docs/overview.md) and [Working with CesiumJS](../docs/cesium.md) sections of the documentation before playing around with this example.

<br/>

## Sample Data

A small amount of sample data has been committed to demonstrate the power of the TWA-VF to visualise different data types. Please do not make changes to the sample data without consulting the original developer. At the time of writing, the sample data sets include:

- **New York**:
  - Tiled 3D buildings, loaded from a remote CMCL server.
  - 2D river data from a WMS endpoint provided by Cornell University.
  - Sample tile sets to demonstrate clipping planes.
  - No metadata or timeseries present in this data set.

It's also worth noting that with this example visualisation, no triplestore data or FeatureInfoAgent is used, hence there is no support for dynamic metadata and timeseries data is unavailable. This is something that we plan to work on in future.

<br/>

## Images

<br/>
<p align="center">
 <img src="../docs/img/sample-nyc-1.JPG" alt="City scale data set." width="75%"/>
</p>
<p align="center">
 <em>City scale data set.</em><br/><br/><br/>
</p>

<br/>
<p align="center">
 <img src="../docs/img/sample-nyc-2.JPG" alt="2D data from WMS endpoint." width="75%"/>
</p>
<p align="center">
 <em>2D data from WMS endpoint.</em><br/><br/><br/>
</p>

<br/>
<p align="center">
 <img src="../docs/img/sample-nyc-3.JPG" alt="Example clipping plane." width="75%"/>
</p>
<p align="center">
 <em>Example clipping plane.</em><br/><br/><br/>
</p>

