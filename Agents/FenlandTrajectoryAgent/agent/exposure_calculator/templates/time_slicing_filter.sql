DO $$
DECLARE 
    column_exists BOOLEAN;
    sql_query TEXT;
BEGIN
    SELECT EXISTS (
        SELECT 1 
        FROM pg_catalog.pg_attribute 
        WHERE attrelid = '{feature_table}'::regclass 
          AND attname = 'RatingDate'
          AND NOT attisdropped
    ) INTO column_exists;

    sql_query := E'
        WITH BufferedLine AS (
            SELECT ST_Buffer(
                     ST_MakeLine(gps."column7"::geometry ORDER BY gps."time")::geography, 
                     {exposure_radius}
                   ) AS buffered_geom
            FROM "{gps_table}" gps
        ),
        IntersectedObjects AS (
            SELECT 
                frs."Name"::text AS entity_name,
                frs."Address"::text AS address,
                ST_AsText(frs.geom) AS entity_geom,
                frs.geom,
                frs.*  
            FROM "{feature_table}" frs
            JOIN BufferedLine bl
              ON ST_Intersects(bl.buffered_geom, frs.geom::geography)
        )';

    IF column_exists THEN
        sql_query := sql_query || E'
            , TrajectoryTimes AS (
                SELECT MIN("time") AS traj_start, MAX("time") AS traj_end
                FROM "{gps_table}"
            ),
            SubBufferedLine AS (
                SELECT ST_Buffer(
                         ST_MakeLine(gps."column7"::geometry ORDER BY gps."time")::geography, 
                         {exposure_radius}
                       ) AS sub_buffered_geom
                FROM "{gps_table}" gps
                JOIN IntersectedObjects frs
                  ON gps."time" > frs."RatingDate"
                GROUP BY frs."RatingDate"
            ),
            FilteredExposure AS (
                SELECT entity_name, address, entity_geom,
                    CASE 
                        WHEN "RatingDate" > (SELECT traj_end FROM TrajectoryTimes) THEN 0
                        WHEN "RatingDate" > (SELECT traj_start FROM TrajectoryTimes)
                             AND "RatingDate" <= (SELECT traj_end FROM TrajectoryTimes) THEN (
                            SELECT CASE WHEN COUNT(*) > 0 THEN 1 ELSE 0 END
                            FROM SubBufferedLine sb
                            WHERE ST_Intersects(sb.sub_buffered_geom, IntersectedObjects.geom::geography)
                        )
                        ELSE 1
                    END AS exposure_count
                FROM IntersectedObjects
                CROSS JOIN TrajectoryTimes
            )
            SELECT * FROM FilteredExposure
        ';
    ELSE
        sql_query := sql_query || E'
            SELECT entity_name, address, entity_geom,
                   COUNT(entity_name)::integer AS exposure_count
            FROM IntersectedObjects
            GROUP BY entity_name, address, entity_geom
        ';
    END IF;

    EXECUTE '
    CREATE OR REPLACE FUNCTION get_exposure_results()
    RETURNS TABLE (
        entity_name text,
        address text,
        entity_geom text,
        exposure_count integer
    )
    AS $$
    BEGIN
        RETURN QUERY EXECUTE ' || quote_literal(sql_query) || ';
    END;
    $$ LANGUAGE plpgsql;
    ';
END $$;
