From the dataset provided by the Landesvermessungsamt, from the folder `/61 Stadtplanung 3/DGM/DGM_2018_1m/`, open files named `dgm1_32???_54??_1_rp.xyz` in QGIS, re-export them as GeoTIFF, and copy the resulting tif files into the present folder.

The same could almost certainly be achieved by passing the right arguments to `gdal_translate`, however at the time of writing, this is the only known work-around for the problem of (horizontally) [flipped rasters](https://gis.stackexchange.com/questions/365082/geoserver-flips-my-geotiffs).
