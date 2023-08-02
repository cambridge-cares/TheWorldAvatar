ALTER TABLE grid_primary_site
ADD COLUMN geom geometry(Point, 4326);

UPDATE grid_primary_site
SET geom = 
    CASE
        WHEN "Spatial Coordinates" ~ '^-?[0-9]+(\.[0-9]+)?,\s*-?[0-9]+(\.[0-9]+)?$' THEN
            ST_SetSRID(ST_MakePoint(
                CAST(split_part("Spatial Coordinates", ',', 2) AS double precision),
                CAST(split_part("Spatial Coordinates", ',', 1) AS double precision)
            ), 4326)
        ELSE
            NULL
    END;
