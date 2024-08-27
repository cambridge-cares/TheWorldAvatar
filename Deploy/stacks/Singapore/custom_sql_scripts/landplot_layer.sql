DROP MATERIALIZED VIEW IF EXISTS landplot_layer;

CREATE MATERIALIZED VIEW landplot_layer AS
WITH uuid_cityobject_id AS (
SELECT strval, cityobject_id 
FROM citydb.cityobject_genericattrib
WHERE attrname='uuid'), 

gfa_cityobject_id AS (
SELECT realval, cityobject_id 
FROM citydb.cityobject_genericattrib
WHERE attrname='GFA'),

uuid_to_gfa AS (
SELECT strval AS building_uuid, realval AS gfa
FROM gfa_cityobject_id
LEFT JOIN uuid_cityobject_id on uuid_cityobject_id.cityobject_id = gfa_cityobject_id.cityobject_id),

gfa_sum AS (
SELECT ogc_fid, sum(gfa) AS calc_gfa
FROM landplot_buildings
JOIN uuid_to_gfa ON uuid_to_gfa.building_uuid = landplot_buildings.building_uuid
GROUP BY ogc_fid
),

ref_gfa AS (
SELECT ogc_fid, CASE WHEN "GPR" ~ '^\d+(\.\d+)?$' 
        THEN ("GPR"::double precision * public.ST_Area("lod1Geometry", true))
		ELSE 0
        END AS ref_gfa
FROM public.landplot
)

SELECT CONCAT('https://www.theworldavatar.com/kg/landplot/', landplot.ogc_fid) AS iri, "LU_DESC", "lod1Geometry" AS geom, ref_gfa, calc_gfa
FROM landplot
LEFT JOIN gfa_sum ON landplot.ogc_fid = gfa_sum.ogc_fid
LEFT JOIN ref_gfa ON ref_gfa.ogc_fid = landplot.ogc_fid;