[//]: # (Note that version headers need to start with "# " characters to be picked up by some automated scripts)

# 3.0.1
### Features:
* _No features present._
### Bug Fixes:
* No longer cache enforced endpoint so is changeable from query to query.
* Minor code improvements to help in maintenance.

# 3.0.0
### Features:
* Added ability to set the reference time for time series queries.
* Added ability to change the limit unit for time series bounding.
* Entire class hierarchy is now queried, any point in the tree can be registered in the configuration.
* Added sample data, ontology, map, queries (etc.) to produce a testing stack.
* Added ability to inject URLs for all Blazegraph and Ontop endpoints into queries.
* Added "/refresh" route to force re-discovery of stack endpoints and re-read configurations.
* Updated configuration format to better support future changes.
* Updated documentation.
* Docker image now pushes with multiple tags.
### Bug Fixes:
* Updated class determination query to no longer require inheritance from OWL class.
* Major refactoring.