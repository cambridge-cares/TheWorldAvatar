DROP TABLE IF EXISTS dukes_2023_pop;

WITH circles AS (
    SELECT "Site Name", "Site Type" as type, "iri", (ST_Buffer(wkb_geometry::geography, 1000)::geometry(Polygon, 4326)) AS geom
    FROM dukes_2023
);

CREATE TABLE dukes_2023_pop AS (
    SELECT "Site Name", "type", "iri", COALESCE((ST_SummaryStatsAgg(ST_CLIP(rast, geom), 1, TRUE)).sum, 0) AS pop, (array_agg(geom))[1]::geometry(Polygon, 4326) AS circle
    FROM population, circles
    WHERE ST_Intersects(population.rast, geom)
    GROUP BY "Site Name", "type", "iri"
);