update
    isochrone_aggregated
set
    geom = ST_CollectionExtract(geom, 3);

update
    isochrone_aggregated
set
    geom = ST_ChaikinSmoothing(geom, 3, false);