
# TWA-VF Change Log

[//]: # (Note that version headers need to start with "## " characters to be picked up by some automated scripts)

## 5.3.0

### Features

* Time slider placeholders in retrieved scenario's data.json are temporarily fixed to 1
* Added order and clickable configuration and functionality from TWA-VF 4
* Added ability to perform subqueries for feature information in parts

### Bug Fixes

* Map events and data are now registered and removed when changing scenarios
* Added a placeholder for the feature selector's dropdown options so that the first option can now be selected as well
* Redirects correctly to the map page when landing page is disabled

## 5.2.0

### Features

* Embedding of dashboard using the "dashboard" parameter in settings.json
* Multiple data.json can be ingested using the new data-setting.json
* Improve dark mode design for some features
* Addition of "grouping" parameter within data.json to allow alternate views of the same source layer
* Addition of "hover" parameter within data.json to create hovering effect for the specified layer if enabled
* Added feature selector to manage multiple closely-positioned features

### Bug Fixes

* Fix missing metadata display
* Fix error when layers use expressions to retrieve their icon image or line colors
* Fix the parsing of RDF literals returned by Feature Info Agent

## 5.1.1

### Bug Fixes

* Minor config file fixes for development environment

## 5.1.0

### Features

* Added a developer tutorial for deploying the platform
* Added a help center for web platform usage
* Added a style guide for developers
* Redesign landing page buttons for coherency and added mapbox controls

## 5.0.2

### Bug Fixes

* Removed dependecy on vulnerable `requests` package

## 5.0.1

### Features

* _No features present._

### Bug Fixes

* As intended, map layers retain current state even as users switch away from the layer tree
* As intended, map layers will reset to default when switching pages

## 5.0.0

### Features

* Release of The World Avatar Visualisation Platform
* New directory for this framework at /web/twa-vis-platform
* Old version is available and supported during the transition

### Bug Fixes

* _No bug fixes present.
