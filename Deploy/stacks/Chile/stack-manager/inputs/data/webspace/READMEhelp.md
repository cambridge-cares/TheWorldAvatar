# Useful links



https://docs.mapbox.com/style-spec/reference/expressions/

https://www.postgis.net/docs/

https://postgis.net/docs/ST_Transform.html

https://postgis.net/docs/RT_reference.html

https://epsg.io/32719

https://postgis.net/docs/reference.html

https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md

https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-data-uploader/README.md

https://github.com/cambridge-cares/TheWorldAvatar/blob/main/web/twa-vis-framework/docs/mapbox.md

https://slideplayer.com/slide/7417666/

https://manifold.net/doc/mfd9/sql_example__custom_contour_intervals.html

https://nronnei.github.io/blog/2017/03/creating-rasters-from-scratch-in-postgis-pt3/

https://www.w3schools.com/SQL/sql_update.asp

https://docs.oracle.com/en/database/oracle/oracle-database/18/geors/raster-algebra-and-analytics.html#GUID-C75744C9-FA04-4391-96F2-59EF2EA212FF

https://gis.stackexchange.com/questions/76092/coordinate-values-are-out-of-range-180-90-180-90-for-geography-type


## Useful actions


PRESS CONTROL + SHIFT + I  to open the developer tools and check for more errors

If capital letters are causing trouble in postgis, then use '''''' symbol

To make it more portable you should be able to replace the "http://146.190.86.59:3851/" part with just "../"

[16/10/2023 13:53] George Brownbridge
I think you can use (ST_Dump(...)).* to expand all of the columns, this should work for all functions that return a table.


