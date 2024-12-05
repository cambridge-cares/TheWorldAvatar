SELECT 
        ST_AsText(ST_Buffer(ST_MakeLine(gps.geom::geometry ORDER BY gps."UTC TIME")::geography, 100)) AS buffered_geom -- Buffer the line in geography units (meters)
    FROM 
        public.gps_test_case_1_data AS gps