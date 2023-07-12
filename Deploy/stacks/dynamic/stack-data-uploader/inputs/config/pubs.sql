SELECT
    ogc_fid,
    name,
    geom,
    'http://theworldavatar.com/ontology/pubontology.owl#PublicHouse/' || ogc_fid as iri,
    'http://traffic-blazegraph:8080/blazegraph/namespace/tboxcsv/sparql' AS endpoint
FROM
    open_pubs