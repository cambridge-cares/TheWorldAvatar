CREATE MATERIALIZED VIEW flooded_buildings AS
SELECT materialized_buildings.building_id,
materialized_buildings.building_height,
materialized_buildings.geometry,
materialized_buildings.uuid,
materialized_buildings.iri
FROM citydb.materialized_buildings,
sealevel_rise_5m
WHERE st_intersects(materialized_buildings.geometry, st_transform(sealevel_rise_5m.geom, 24500));
