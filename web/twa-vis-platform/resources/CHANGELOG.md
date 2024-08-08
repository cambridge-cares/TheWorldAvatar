
# TWA-VF Change Log

[//]: # (Note that version headers need to start with "## " characters to be picked up by some automated scripts)

## 5.4.0

### Features

* Viz app can now be secured behind a Keycloak authentication server by configuring some environment variables. See main documentation of the project 1.1
* Dependency management and build process optimised by using pnpm over npm for the project.
* Docker build optimised with multi stage building and module caching
* Rebuild should only occur if a change in a config file is detected
* Some dependency updating and trimming down
* NB there no longer two places to mount config files and media. All config files should go in `web/twa-vis-platform/code/public` and NOT `web/twa-vis-platform/uploads`

## 5.3.3

### Bug Fixes

* Fix tooltips in ribbon

## 5.3.2

### Bug Fixes

* Fix bug where sidepanel info did not show the correctly time indexed data
* Side panel now re-queries when dimension is changed
* Minor styling changes on scenario selector to fix weird icon scaling
* Better layer tree and ribbon display on mobile

## 5.3.1

### Features

* Scenario selection button now displays current scenario

### Bug Fixes and Improvements

* Several major updates in dependencies
* Dev dependencies no longer shipped with production, reducing number of node packages installed inside the container by ~66%

## 5.3.0

### Features

* Mostly CReDo focused features including:
* Dimension slider displayed in scenario if dimensions are returned from central stack
* Added order and clickable configuration and functionality from TWA-VF 4
* Added ability to perform subqueries for feature information in parts
* RTK query for central stack
* Redesigned controls ribbon
* Customisable landing page image
* Customisable toolbar logo
* Improvements to info and layer tree

### Bug Fixes

* Map events and data are now registered and removed when changing scenarios
* Added a placeholder for the feature selector's dropdown options so that the first option can now be selected as well
* Redirects correctly to the map page when landing page is disabled
* Improved page routing with more robust relative paths in routes
* Various issues and improvements listed in [#1246](https://github.com/cambridge-cares/TheWorldAvatar/issues/1246)

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
