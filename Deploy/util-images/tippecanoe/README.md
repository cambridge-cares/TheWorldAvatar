# Tippecanoe

This Image contains an installation of Tippecanoe, allowing developers to convert GeoJSON files to MBTiles. As tippecannoe does not support Windows, and can be tricky to set up on Linux, this Image has been created to provide a transient environment to quickly run conversions.

For more information on Tippecanoe, see their GitHub page [here](https://github.com/mapbox/tippecanoe/tree/master).

	
## Building the Image

Once any requirements have been addressed, the Image can be build using the following methods.

+ To build the image:
  + `docker build --rm --no-cache -t docker.cmclinnovations.com/tippecanoe:latest .`
+ To generate a container (i.e. run the image):
  + `docker run -d --restart always -v "$(pwd)":/usr/local/tippecanoe --name "tippecanoe" -it docker.cmclinnovations.com/tippecanoe:latest`
+ To push the image to the CMCL registry:
  + `docker image push docker.cmclinnovations.com/tippecanoe:latest`
  
  
## Notes

The following command was used to generate the crop map MBTile files for the land use visualisation:
`tippecanoe -o output.mbtiles input.geojson`