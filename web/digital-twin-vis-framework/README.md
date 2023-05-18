# Digital Twin Visualisation Framework

<img align="right" width="200" height="200" src="./docs/img/dtvf-logo-padded.svg">

A central framework for Digital Twin Visualisations (the Digital Twin Visualisation Framework, or DTVF) has been created so that in most cases, the process of creating a new visualisation to display pre-generated data is as simple as possible. The goal is that a developer that is inexperienced with Typescript (or the JavaScript libraries we're using) can get a reasonable visualisation of the data by simply ensuring the data meets a set format and providing some basic configuration files.

The current, working version of the DTVF is **3.3.4**.

## Contents

Documentation for the DTVF is spread across a number of files, and is broken down into the following sections.

* [Overview](./docs/overview.md)
  * _Provides a high level overview on the DTVF._
* [Development](./library/README.md)
  * _Information for developers of the DTVF._
* [Working with Mapbox](./docs/mapbox.md)
  * Generating layers and handling interactions with [Mapbox](https://www.mapbox.com/).
* [Working with Cesium](./docs/cesium.md)
  * Generated layers and handling interactions with [CesiumJS](https://cesium.com/platform/cesiumjs/).
* [Advanced features](./docs/advanced.md)
  * Enabling and configuring more advanced features.
* [Tutorials](./docs/tutorials.md)
  * Tutorials for creating new visualisations. 
* [Troubleshooting](./docs/troubleshooting.md)
  * _Solutions to common issues and questions._ 

## Examples

<p align="center">
 <img src="./example-mapbox-vis/readme-example.JPG" alt="Example of 2D data on a Mapbox visualisation" width="75%"/>
</p>
<p align="center">
 <em>Example of a 2D data on a Mapbox visualisation.</em><br/><br/><br/>
</p>
<p align="center">
 <img src="./example-cesium-vis/readme-example.JPG" alt="Example of 3D data on a CesiumJS visualisation" width="75%"/>
</p>
<p align="center">
 <em>Example of a 3D data on a CesiumJS visualisation.</em>
</p>