[//]: # (Note that version headers need to start with "# " characters to be picked up by some automated scripts)

#4.5.0
### Bug Fixes
* Removed linear-gradient effect in sidebar due to scrollbar getting affected
* Increased hover height of selected sidebar tab
* Fixed content container overlapping with tabs in sidebar
* Removed grey outlines in hovered and selected sidebar tabs

#4.5.0
### Features
Three control buttons are now included by default. These are:

* Scale Control (bottom right indicator of current map scale in metres and kilometres)
* Geolocator (a button in the bottom right to display current location of the user)
* Zoom, pitch and compass controls in the top right
  
  Ability to turn off with flags in `settings.json`. see [advanced mapbox documentation](../docs/advanced-mapbox.md)

# 4.4.0
### Features
* New colours in side panel including CMCL blue and more modern grey on off-white text
* Add animated wave above footer in sidebar
* Rich link hover effects in side panel

### Bug Fixes
* fixed scroll bars appearing automatically in layer tree


# 4.3.0
### Features
* Initial support for "parallel-world" scenarios (not yet supported by TWA backend).
* Added optional "credo" flag to use different format URLs for FeatureInfoAgent calls.
### Bug Fixes:
* Fixed checks for object properties before passing layers/sources to Mapbox API.
* Click events now correctly filter out non-interactable layers.

# 4.2.0
### Features
* Added an "interactions" layer parameter to set level of allowed mouse interaction ("all"|"click-only"|"hover-only"|"none", defaults to "all").
* Added the ability to use "[SELECTED-IRI]" and "[HOVERED-IRI]" placeholders in Mapbox expressions (Cesium TBD).
* Added system to set callbacks for feature selection, unselection, and layer tree change events.
### Bug Fixes:
* Fixed minor issue with loading metadata from selected raster layers in Cesium.

# 4.1.2
### Features
* _No features present._
### Bug Fixes:
* TWA-VF files no longer copied into Docker volume directory.
* TWA imports now available at "./lib/" route in addition to "./twa-vf".
* Example visualisations now run in a TWA stack.
* Documentation updates.

# 4.1.1
### Features
* _No features present._
### Bug Fixes:
* No longer append null units to time series tables.
* No longer show charts for time series where dependent values are strings.
* Time series table should now grow to fill container vertically.

# 4.1.0
### Features
* Added Cesium source option to enable/disable back face culling in 3D tile sets.
### Bug Fixes:
* Popup components are now handled centrally by the framework.
* Fixed placement of popup components over 2D WMS features in Cesium.
* Selecting the same base imagery twice no longer removes all data.
* Changing the base imagery now preserves the layer visibility.
* Minor tweaks to Mapbox tutorial.

# 4.0.0
### Features
* Renamed from "Digital Twin Visualisation Framework" to "TWA Visualisation Framework".
* Docker image name will now be "ghcr.io/cambridge-cares/twa-vf".
* Example visualisations updated to import JS and CSS files from "./twa-vf/" directory.
### Bug Fixes:
* _No bug fixes represent._
  
# 3.9.0
### Features
* Can now display a button to open an associated analytics dashboard.
### Bug Fixes:
* Minor CSS tweaks to filtering controls.
  
# 3.8.3
### Features
* _No features present._
### Bug Fixes:
* Separated and moved docker config files to better support GitHub actions.

# 3.8.2
### Features
* _No features present._
### Bug Fixes:
* Removed markdown ignores in GitHub action, not compatible with existing config.

# 3.8.1
### Features
* _No features present._
### Bug Fixes:
* Updated GitHub workflow syntax for automated release job.

# 3.8.0
### Features
* Docker image (dtvf-base-image) now contains DTVF files rather than relying on remote CDN.
* Overhaul of user and development documentation.
* Addition of step-by-step tutorial to create a Mapbox visualisation using the DTVF.
### Bug Fixes:
* Added character encoding statement in example visualisation's index file, can fix some rare encoding issues.
  
# 3.7.0
### Features:
* Mapbox username and token can now be read from external files/docker secrets.
### Bug Fixes:
* _No bug fixes present._

# 3.6.0
### Features:
* Location of configuration file can now be passed into DTVF.
### Bug Fixes:
* _No bug fixes present._
 
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
* Fixed issue in Cesium visualisations that prevented a selected asset's `stack` parameter from being determined.

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