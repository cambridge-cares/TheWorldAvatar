
# TWA-VF Change Log

[//]: # (Note that version headers need to start with "## " characters to be picked up by some automated scripts)

## 5.12.0

- Add ability to include JWT token in header for MapBox source requests.

## 5.11.1

### Bug Fixes

- Fixed links to optional pages behind nginx

## 5.11.0

## Improvements

- Simplify, secure and optimise MapBox credential management by loading in as environment variables instead of exposing via public API and using HTTP to read them
- Add a "layerTreeIconOverride" option to mapbox layers for cases where automatic icon is not wanted
- Update several packages, including a breaking version change of mapbox gl-js
- Update node to last LTS of v22, with a view to move to v23 when webpack dependencies can be resolved

### Bug Fixes

- Fix tooltip on ribbon remaining engaged on dropdown menu
- Change default from 3D maps to 2D maps
- Correctly find landing page icon
- Properly sanitize icons passed to mapbox with ASSET_PREFIX

## 5.10.0

### Features

- Added customisable search capabilities for the map visualisation, inclusive of metadata and time series
- Improve the search form design and user interaction
- Added geocoding capabilities
- Added documentation of these functions

### Bug Fixes

- Fix bad state calls in map container
- Fix some of the errors logged in the console

## 5.9.2

### Improvements

- Updated Keycloak connector to latest version v26

### Bug Fixes

- Resolved bug causing unexpectesd logouts from Keycloak due to next prefetching.

## 5.9.1

### Features

- Added CMCL logo to default icons library

## 5.9.0

### Bug Fixes

- Read in `ASSET_PREFIX` environment variable and write it to next.js asset prefix and basepath to fix static resources behind double nginx

## 5.8.1

### Bug Fixes

- Fix production build issues

## 5.8.0

### Features

- Added sorting, filters and other functions to the registry table
- Improve user interactions and experience in the registry table
- Improve user interactions with the schedule form sections and the form in general

## 5.7.0

### Bug Fixes

- Random logouts when keycloak authentication is enabled
- Role based authorisation was broken behind docker

### Changes

- Updated various package dependencies to latest versions

## 5.6.0

### Features

- Extension of the form UI for search capabilities
- Addition of initial search capabilities for the map visualisation* Addition of time slots for the form schedule section
- Modify the registry to work with the new vis backend agent
- Consolidation of the urls of assets as constants in one file

### Bug Fixes

- Fix the form's css

## 5.5.0

### Features

- Addition of the registry pages, enabling users to view, create, edit, and remove records from the knowledge graph
- Addition of scheduling capabilities for the specific registry pages
- Users can deploy multiple navbar logos
- Improvements to the handling and structure of configuration settings and pages
- Improvements to the documentation and tutorial

### Bug Fixes

- Fix the development mode in a Docker container
- Improve user experience if modules are disabled by redirecting back to homepage

## 5.4.0

### Features

- Viz app can now be secured behind a Keycloak authentication server by configuring some environment variables. See main documentation of the project 1.1
- Dependency management and build process optimised by using pnpm over npm for the project.
- Docker build optimised with multi stage building and module caching
- Rebuild should only occur if a change in a config file is detected
- Some dependency updating and trimming down
- NB there no longer two places to mount config files and media. All config files should go in `web/twa-vis-platform/code/public` and NOT `web/twa-vis-platform/uploads`

## 5.3.3

### Bug Fixes

- Fix tooltips in ribbon

## 5.3.2

### Bug Fixes

- Fix bug where sidepanel info did not show the correctly time indexed data
- Side panel now re-queries when dimension is changed
- Minor styling changes on scenario selector to fix weird icon scaling
- Better layer tree and ribbon display on mobile

## 5.3.1

### Features

- Scenario selection button now displays current scenario

### Bug Fixes and Improvements

- Several major updates in dependencies
- Dev dependencies no longer shipped with production, reducing number of node packages installed inside the container by ~66%

## 5.3.0

### Features

- Mostly CReDo focused features including:
- Dimension slider displayed in scenario if dimensions are returned from central stack
- Added order and clickable configuration and functionality from TWA-VF 4
- Added ability to perform subqueries for feature information in parts
- RTK query for central stack
- Redesigned controls ribbon
- Customisable landing page image
- Customisable toolbar logo
- Improvements to info and layer tree

### Bug Fixes

- Map events and data are now registered and removed when changing scenarios
- Added a placeholder for the feature selector's dropdown options so that the first option can now be selected as well
- Redirects correctly to the map page when landing page is disabled
- Improved page routing with more robust relative paths in routes
- Various issues and improvements listed in [#1246](https://github.com/cambridge-cares/TheWorldAvatar/issues/1246)

## 5.2.0

### Features

- Embedding of dashboard using the "dashboard" parameter in settings.json
- Multiple data.json can be ingested using the new data-setting.json
- Improve dark mode design for some features
- Addition of "grouping" parameter within data.json to allow alternate views of the same source layer
- Addition of "hover" parameter within data.json to create hovering effect for the specified layer if enabled
- Added feature selector to manage multiple closely-positioned features

### Bug Fixes

- Fix missing metadata display
- Fix error when layers use expressions to retrieve their icon image or line colors
- Fix the parsing of RDF literals returned by Feature Info Agent

## 5.1.1

### Bug Fixes

- Minor config file fixes for development environment

## 5.1.0

### Features

- Added a developer tutorial for deploying the platform
- Added a help center for web platform usage
- Added a style guide for developers
- Redesign landing page buttons for coherency and added mapbox controls

## 5.0.2

### Bug Fixes

- Removed dependecy on vulnerable `requests` package

## 5.0.1

### Features

- _No features present._

### Bug Fixes

- As intended, map layers retain current state even as users switch away from the layer tree
- As intended, map layers will reset to default when switching pages

## 5.0.0

### Features

- Release of The World Avatar Visualisation Platform
- New directory for this framework at /web/twa-vis-platform
- Old version is available and supported during the transition

### Bug Fixes

- _No bug fixes present.
