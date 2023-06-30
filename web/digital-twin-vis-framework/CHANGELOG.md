[//]: # (Note that version headers need to start with "# " characters to be picked up by some automated scripts)

# 3.5.0
### Features:
* Added ability to specify client-side styling & filtering options for Cesium 3D tilesets.
* Updated sample NYC data used in example Cesium visualisation.
### Bug Fixes:
* Restored missing JS file used to build legends in example visualisations.
* Minor CSS tweaks to UI components.

# 3.4.0
### Features:
* New clipping planes feature to allow `horizontal` slicing of 3D tile sets.
* Added new sample data to example Cesium visualisation.
### Bug Fixes:
* Reduced size of sample NYC data loaded into example Cesium visualisation.
* Added missing `moment.js` library to example visualisations.
* Moved Pylon visualisation out of DTVF directory.
* Minor CSS tweaks to UI components.
* Minor formatting tweaks to automated release email.

# 3.3.5
### Features:
* _No features present._
### Bug Fixes:
* Fixed issue when using numerical digits in searching feature.

# 3.3.4
### Features:
* Can now make use of SDF icons in Mapbox visualisations.
* Create a base Docker image for visualisations, with pre-configured bind mount for content.
* Added files required for integration with The Stack.
### Bug Fixes:
* Improved formatting of JSON response from FeatureInfoAgent.
* CSS tweaks for pop-up content when hovering over assets.
* Added character set specification to JS import.
* Fixed issue with visibility of metadata and timeseries panels.

# 3.3.3
### Features:
* _No features present._
### Bug Fixes:
* Additional logging to FIA and a minor error check when awaiting the response in the DTVF.
* Changed error handling in agent calling code to remove reliance on Promise objects.

# 3.3.2
### Features:
* _No features present._
### Bug Fixes:
* If set, each asset's `endpoint` parameter is now sent to the FeatureInfoAgent.

# 3.3.1
### Features:
* _No features present._
### Bug Fixes:
* Fixed issue in Cesium visualiations that prevented a selected asset's `stack` parameter from being determined.

# 3.3.0
### Features:
* Added search functionality to Mapbox visualisations.
* Added separate page for "Visualisation Help".
### Bug Fixes:
* Fixed null pointers occurring when selecting features (Mapbox).
* Fix identification of original layer when selecting features (Cesium).
* Fixed issues with Attribution visibility when maximising the side panel.

# 3.2.1
### Features:
* _No features present._
### Bug Fixes:
* Added additional null checks for selected assets with missing `properties` nodes.

# 3.2.0
### Features:
* Added ability to use custom terrain providers in Cesium visualisations.
* Added ability to set globe opacity in Cesium visualisations.
### Bug Fixes:
* Fixed issue with null exceptions when attempting to zoom to specific features.
* Fixed type in example Mapbox visualisation.

# 3.1.0
### Features:
* Added new imagery layer for Cesium visualisations (Satellite without labels).
* Added ability to define default expansion states in the layer tree.
* Added Ability to define layer order globally (i.e. irrespective of grouping).
* Can now use PAGE_UP and PAGE_DOWN buttons to zoom in Cesium visualisations.
* Can now click the asset name in the side panel to fly to it in Cesium visualisations.
* Refined camera options.
### Bug Fixes:
* Fixed scroll bar sizing in the Layers tree.
* Default visibility of layers in Cesium visualisations now functions.
* Fixed issues with toggling visibility for KML layers in Cesium visualisations.
* Allowed specification of positions for 3D tile data sources.
* Fixed hover effects for features containing properties with null values.