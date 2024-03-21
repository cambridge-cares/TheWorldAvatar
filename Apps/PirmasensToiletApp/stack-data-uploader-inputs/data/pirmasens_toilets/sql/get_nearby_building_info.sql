select addr_housenumber as housenumber, 
addr_housename as housename, 
addr_street as street, 
addr_place as place, 
addr_suburb as suburb, 
addr_city as city, 
addr_country as country, 
addr_postcode as postcode,
ST_SetSRID(polygons."geometryProperty", 4326) as "geometryProperty"
from "public"."polygons" as polygons
ORDER BY ST_Distance(ST_SetSRID(polygons."geometryProperty", 4326), ST_SetSRID(ST_MakePoint(%lon%, %lat%), 4326))
LIMIT 1