SELECT ST_AsText(ST_Buffer(geography(gps.geom), 100)) as buffered_geom
FROM public.gps_test_case_2_data AS gps