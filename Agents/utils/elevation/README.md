# Elevation Utils

This directory contains a Python script to determine the elevation of a given lattitude-longitude coordinate using the MapBox RGB Elevation tileset.

*Note:* As each elevation calculation requires a call to a MapBox API (with the below key),	be aware that is may use up your monthly API calls quota. For CMCL, this means that if we need to run the elevation script on many points, we may need to become premium MapBox members.

At the time of writing this script is not packaged into any Python module, nor are any setup files/scripts present. In future, these can be added so that Agents can easily make use of the script.