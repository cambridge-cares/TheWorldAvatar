SET search_path TO public,citydb;
UPDATE "building"
SET "measured_height" = "h" FROM
(SELECT COALESCE("building"."id", "thematic_surface"."building_id") AS "bid",MAX(public.ST_ZMax("geometry"))-MIN(public.ST_ZMin("geometry")) AS "h" FROM "surface_geometry"
FULL JOIN "building" ON "surface_geometry"."root_id" = COALESCE("building"."lod4_multi_surface_id","building"."lod3_multi_surface_id","building"."lod2_multi_surface_id","building"."lod1_multi_surface_id","building"."lod4_solid_id","building"."lod3_solid_id","building"."lod2_solid_id","building"."lod1_solid_id")
FULL JOIN "thematic_surface" ON "surface_geometry"."root_id" = COALESCE("thematic_surface"."lod4_multi_surface_id","thematic_surface"."lod3_multi_surface_id","thematic_surface"."lod2_multi_surface_id")
WHERE "geometry" IS NOT NULL GROUP BY "building"."id", "thematic_surface"."building_id"
) AS "x" WHERE "building"."measured_height" IS NULL AND "x"."bid"="building"."id";