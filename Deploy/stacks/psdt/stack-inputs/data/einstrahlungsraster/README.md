From the Solarkataster dataset, within the folder `/Einstrahlungsraster/RP04/`, the file
`gRP04_KWh_Yr_Shd.tif`
is the relevant one. It covers the city of Pirmasens as well as surroundings. You may want to crop it using e.g. QGIS or gdal_translate if it is too big to handle. Note that the file as it is fails to import into the stack with a cryptic error (not related to the size of the file). Opening it in QGIS and re-exporting the layer as GeoTIFF resolves this issue. Passing the right argument to `gdal_translate` presumably could also achieve this, but it is unclear whatargument is required.
