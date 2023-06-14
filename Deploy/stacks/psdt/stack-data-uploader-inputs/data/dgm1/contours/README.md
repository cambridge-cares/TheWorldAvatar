Open the files from the `elevation` folder in QGIS. Then, [extract vector contours](https://gis.stackexchange.com/questions/385086/how-can-i-vectorize-contour-lines-from-a-dtm-in-qgis) via "Raster -> extract -> contour". NB The same can be achieved by `gdal_contour`.

Optionally, [smooth](https://gis.stackexchange.com/questions/346049/making-elevation-contours-of-raster-smoother-using-qgis) the contours as follows. In the Processing Toolbox, type `smooth` into search bar, double click `SAGA Line Smoothing` as method, select Improved SIA model, 20 iterations, sigma 3.0. Save the layer as shape file, and copy the file into the present folder.

NB To [label](https://opensourceoptions.com/blog/how-to-create-contour-lines-and-labels-with-qgis/) the contours in QGIS, select the `Labels` tab from the Layer Styling panel and choose `Single Labels` from the drop-down. The `Value` field of the labels tab specifies which column to use for labeling. Select the `ELEV` column for the Value.
