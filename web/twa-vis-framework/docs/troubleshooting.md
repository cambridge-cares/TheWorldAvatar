# Troubleshooting & FAQs

This page offers some solutions for common issues, and answers to common questions, that may arise when using the TWA-VF. These are listed here (on the GitHub wiki), rather than within committed README files as they may be useful for all uses of the framework, regardless of mapping provider, or pertain to multiple versions of the code.

Potential issues/questions relating to all uses of the framework, regardless of map provider, are listed first; solutions that only pertain to individual map providers are listed in further sections.

<br/>

## Generic

This section is relevant to all uses of the TWA-VF, regardless of the selected mapping provider.

| Issue/Question       | Solution/Answer    |
| ----------- | ----------- |
| Do my data files have to be hosted online? | Both mapping providers require a valid URL to load data files, this does mean that they have to be accessible online. However, data files can be included within the visualisation container (which uses Apache to host a web server) so that they can be accessed via a URL relative to the visualisation's hosted directory (i.e. "/data/tileset.json"). |
| Visualisation not updating after changes | Try clearing ([or disabling](https://www.webinstinct.com/faq/how-to-disable-browser-cache#:~:text=When%20you're%20in%20Google,close%20out%20of%20Developer%20Tools.)) your browser cache before reloading the page. |
| Visualisation not updating after changes | If running the visualisation within a Docker container, you may need to rebuild the Docker image and run a new container to see recent file changes. |
| No data is shown | If no data is shown _and_ no layer tree is built then this suggests that one (or more) of the user defined JSON files is invalid. Please use an external validator tool (or website) to ensure that the JSON is valid. |
| Hovering over a feature does nothing | The framework supports mouse hovering effects if the input data contains certain metadata fields. To show a small description box the geospatial data needs to contain a `name` and `description` field. |

<br/>

## Mapbox

| Issue/Question       | Solution/Answer    |
| ----------- | ----------- |
| What URL do I use to load content from GeoServer? | Check the [Building the WMS URL](./mapbox.md#building-the-wms-url) section of the Mapbox documentation. |
| How do I setup data-driven styling? | Using Mapbox's expressions system within layer styles. Try the tutorial [here](https://docs.mapbox.com/help/tutorials/mapbox-gl-js-expressions/). |
| Getting an "Unimplemented Type 4" error | This is normally thrown by Mapbox when it cannot pull data from a WMS endpoint correctly, double check your URL. |
| I keep seeing the same data regardless of the location I'm viewing | Double check that your WMS URLs are correctly using the placeholder string for the `bbox` parameter (e.g. "bbox={bbox-epsg-3857}"), using literal values here tells Mapbox to use the same tile of data for the entire map. |
| My data's projection looks off | Mapbox displays data using the EPSG 3857 projection, this is the CRS that should be used in your WMS URLs (think of the URL as how to display the data, not how the data was originally uploaded). |

<br/>

## CesiumJS

This section is relevant to issues/questions that only arise when developing CesiumJS visualisation with the framework.

| Issue/Question       | Solution/Answer    |
| ----------- | ----------- |
| Getting a 401 (Unauthorized) error | If the URL causing the error is an attempt to use a CesiumJS API key, then this is expected. The desire here is that we _want_ to ensure that we're not using a CesiumJS Ion key. |
| 3D data is not showing | Depending on the format, some data may specify object locations relative to a position defined outside the data itself. If not set, this can cause the 3D data not to appear. Within the `source` node of the visualisation configuration, please ensure the `position` (an array of longitude, latitude, and height) is set. |
| Features appear to be floating | For many data formats (such as 3D tile sets), CesiumJS does not provide an easy option to change the height of its data. Official advice appears to be that developers need to ensure the elevation of their data is correct (factoring in the specific terrain model that will be used), _when the data file is generated_. |
| How can I style my 2D data? | Unlike Mapbox, CesiumJS does not provide any functionality for styling 2D data on the client side. Instead, styling of any data provided by WMS endpoints must be done within Geoserver. For more information on server-side styling, read the page [here](https://docs.geoserver.org/stable/en/user/styling/index.html). | 
| How can I style my 3D data? | This can be done directly within the model files being loaded, or by use of the CesiumJS style expressions, read more about this on the [Advanced CesiumJS features](./advanced-cesium.md) page. | 
| Visualisation is very unresponsive/slow to render | There's two possibilities here, either you're plotting too much data at once, or you need to invest in a better GPU; unfortunately, CesiumJS has a high GPU overhead. | 
| Can I display 2D vector data? | CesiumJS does not provide an easy way to inject and display 2D vector data (particularly from WMS or WFS endpoints). GeoServer can host the vector data, but when used CesiumJS will rasterise it as a single, flat PNG before visualisation. Whilst it may be possible in future (with significant development), this is not something offered by the framework at the time of writing. |
| Getting a 422 (Unknown) error | This can sometimes happen when CesiumJS contacts the Mapbox styles API, especially at close-up zoom levels. Unless this is causing any visual issues, it should be safe to ignore. |