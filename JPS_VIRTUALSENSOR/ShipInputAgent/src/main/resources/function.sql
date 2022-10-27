CREATE OR REPLACE FUNCTION public.get_geometry_table("table" TEXT, "columnName" TEXT)
RETURNS TABLE ("time" bigint, "value" geometry) AS
$$
BEGIN
RETURN QUERY EXECUTE 'SELECT "time", "' || "columnName" || '" AS value FROM "' || "table" || '"';
END;
$$ LANGUAGE plpgsql;