SELECT
    ogc_fid,
    date as name,
    geom,
    'http://www.w3.org/2012/7/ra3.owl#RoadAccident/' || ogc_fid as iri
    
FROM
    road_incidents