ALTER TABLE acraregisteredcompanyaddress
ADD COLUMN if not exists wkb_geometry geometry(Point,4326);
UPDATE acraregisteredcompanyaddress 
SET wkb_geometry = sgpostcode.wkb_geometry
FROM sgpostcode 
WHERE acraregisteredcompanyaddress.reg_postal_code = sgpostcode.postal_code;
DELETE FROM acraregisteredcompanyaddress
WHERE wkb_geometry IS NULL;
