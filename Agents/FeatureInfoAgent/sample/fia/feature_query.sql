WITH buffered_line AS (
    SELECT 
        ST_Buffer(ST_Transform(ST_GeomFromText('[LINE_WKT]', 4326), 24500), 100) AS geom
)

SELECT 
    iri 
FROM 
    buildings_layer b, 
    buffered_line l
WHERE 
    ST_Intersects(b.geom, l.geom)