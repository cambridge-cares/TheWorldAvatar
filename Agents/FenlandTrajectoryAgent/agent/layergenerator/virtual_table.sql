CREATE OR REPLACE FUNCTION getLocationTable(pointiri VARCHAR, speediri VARCHAR, altitudeiri VARCHAR, bearingiri VARCHAR)
RETURNS TABLE (time timestamptz, geom geometry, speed double precision, altitude double precision, bearing double precision)
AS $$
DECLARE
    tableName TEXT;
BEGIN
    tableName := getTableName(pointiri);
    RETURN QUERY EXECUTE 
        format('SELECT time, %I AS geom, %I AS speed, %I AS altitude, %I AS bearing FROM %I', 
               getColumnName(pointiri), getColumnName(speediri), getColumnName(altitudeiri), getColumnName(bearingiri), tableName);
END
$$ LANGUAGE plpgsql;