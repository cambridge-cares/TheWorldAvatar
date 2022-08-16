# TheWorldAvatar - Python Utils

This Python package contains a number of logging utilities that may be useful to any Python-based project within The World Avatar (TWA) ecosystem. At the time of writing, this project builds an isolated package named `agentlogging` that users can import in their own code. In the future, this package may be bundled with the Python wrapper for the JPS Base Library so that only one dependency is required.

## Functions

The library currently contains the following functionality:

* Logging
..* The `log` subpackage contains scripts to initialise and generate loggers that should be used in all TWA codes. For more details, see the [Logging](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Logging) page on the Wiki.