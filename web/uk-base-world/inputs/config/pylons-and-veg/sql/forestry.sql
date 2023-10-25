SELECT
    CONCAT(
        'http://www.theworldavatar.com/ontology/ontoforest/OntoForest.owl#forestrymap/cell/',
        forestry.ogc_fid
    ) as iri,
    forestry.ogc_fid as ogc_fid,
    CONCAT(forestry."IFT_IOA", ' (', "OBJECTID_1", ')') as name,
    "CATEGORY" as category,
    wkb_geometry,
    colour
FROM
    forestry,
    forestry_colours
WHERE
    forestry."IFT_IOA" = forestry_colours."ift_ioa"