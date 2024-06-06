package uk.ac.cam.cares.jps.agent.osmagent.usage;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

/**
 * UsageShareCalculator contains 3 parts that run using SQL query
 * 1) assignUsageShare - For each building_iri, assign Propertyusage_iri,
 * calculate and assign usageShare to each OSM item.
 * 2) updatePropertyUsageStatement - For each building_iri, that has two OSM
 * items, check if they are the same ontoBuilt type,
 * if they are the same ontoBuilt type, return the first instance as the
 * propertyusage_iri
 * 3) updateLandUse - For building_iri which are untagged to any osm_tags, as a
 * default fallback,
 * SQL query is run check and tag buildings with buildings_iri with
 * propertyusage_iri and usageshare according to dlm_landuse.csv
 */

public class UsageShareCalculator {
        private RemoteRDBStoreClient rdbStoreClient;

        /**
         * @param database database URL
         * @param user username to database
         * @param password password to database
         */
        public UsageShareCalculator(String database, String user, String password) {
                this.rdbStoreClient = new RemoteRDBStoreClient(database, user, password);
        }

        public void createUsageIRI(String usageTable) {
                String createIRI = "UPDATE %s SET propertyusage_iri = ontobuilt || '_' || uuid_generate_v4()::text";

                rdbStoreClient.executeUpdate(String.format(createIRI, usageTable));

                String consistentIRI = "WITH temp AS (SELECT building_iri, ontobuilt, propertyusage_iri AS consistent FROM %s GROUPY BY building_iri, ontobuilt)\n" +
                        "UPDATE %s u SET propertyusage_iri = consistent FROM temp t WHERE u.building_iri = t.building_iri AND t.ontobuilt = u.ontobuilt";

                rdbStoreClient.executeUpdate(String.format(consistentIRI, usageTable));
        }

        public void usageShareCount(String view, String usageTable, String pointTable, String polygonTable) {
                String sql = "CREATE MATERIALIZED VIEW %s AS\n" +
                        "WITH counts AS (SELECT building_iri, ontobuilt, propertyusage_iri,  COUNT(*) AS c FROM %s GROUP BY building_iri, ontobuilt, propertyusage_iri),\n" +
                        "total AS (SELECT building_iri, SUM(c) AS total FROM counts GROUP BY building_iri),\n" +
                        "names AS (SELECT building_iri, ontobuilt, name FROM %s WHERE building_iri IS NOT NULL\n" +
                        "UNION SELECT building_iri, ontobuilt, name FROM %s WHERE building_iri IS NOT NULL)\n" +
                        "SELECT c.building_iri, c.ontobuilt, c.propertyusage_iri, (c.count::DOUBLE PRECISION/t.total) AS usageshare, n.name\n" +
                        "FROM counts AS c\n" +
                        "JOIN total AS t ON c.building_iri = t.building_iri\n" +
                        "LEFT JOIN names AS n on c.building_iri = n.building_iri AND c.ontobuilt = n.ontobuilt";

                rdbStoreClient.executeUpdate(String.format(sql, view, usageTable, pointTable, polygonTable, usageTable));
        }


        public void usageShareArea(String view, String usageTable) {
                String sql = "CREATE MATERIALIZED VIEW %s AS\n" +
                        "WITH counts AS (SELECT building_iri, ontobuilt, propetyusage_iri, COUNT(*) AS c,\n" +
                        "SUM(CASE WHEN source = 'polygon' THEN area ELSE 0 END) AS area_sum FROM %s\n" +
                        "GROUP BY building_iri, ontobuilt, propetyusage_iri),\n" +
                        "total AS (SELECT building_iri, SUM(c) AS total_c, SUM(area_sum) AS total_area FROM counts GROUP BY building_iri),\n" +
                        "names AS (SELECT building_iri, ontobuilt, name FROM %s WHERE building_iri IS NOT NULL\n" +
                        "UNION SELECT building_iri, ontobuilt, name FROM %s WHERE building_iri IS NOT NULL)\n" +
                        "SELECT c.building_iri, c.ontobuilt, c.propetyusage_iri, n.name, CASE\n" +
                        "WHEN t.total_area = 0 THEN c.c/t.total_c\n" +
                        "ELSE c.area_sum/t.total_area END AS usageshare\n" +
                        "FROM counts AS c\n" +
                        "JOIN total AS t\n" +
                        "ON c.building_iri = t.building_iri\n" +
                        "LEFT JOIN names AS n on c.building_iri = n.building_iri AND c.ontobuilt = n.ontobuilt";

                rdbStoreClient.executeUpdate(String.format(sql, view, usageTable, usageTable));
        }

        public void addMaterializedView(String usageTable, String addressTable, String schema, String osmSchema) {
                String materializedView ="-- Drop the materialized view if it exists\n" +
                        "DROP MATERIALIZED VIEW IF EXISTS "+schema+".buildinginfo_osm;\n" +
                        "\n" +
                        "-- Create a new materialized view named \"buildinginfo_osm\" in the \"buildinginfo\" schema\n" +
                        "CREATE MATERIALIZED VIEW "+schema+".buildinginfo_osm AS\n" +
                        "WITH cte AS (SELECT DISTINCT COALESCE(u.building_iri, a.building_iri) AS building_iri,\n" +
                        "u.propertyusage_iri, u.ontobuilt, u.usageshare,\n" +
                        "a.address_iri, a.country, a.city, a.street, a.house_number, a.postcode\n" +
                        "FROM "+usageTable+" AS u\n" +
                        "FULL OUTER JOIN "+addressTable+" AS a ON u.building_iri = a.building_iri)\n" +
                        "\n" +
                        "SELECT DISTINCT c.*, COALESCE(p.name, o.name) AS name\n" +
                        "FROM cte AS c\n" +
                        "LEFT JOIN "+osmSchema+".points AS p ON c.building_iri = p.building_iri\n" +
                        "LEFT JOIN "+osmSchema+".polygons AS o ON c.building_iri = o.building_iri;";


                String materializedView_geoserver= "-- Drop the materialized view if it exists\n" +
                        "DROP MATERIALIZED VIEW IF EXISTS "+schema+".buildingusage_geoserver;\n" +
                        "\n" +
                        "CREATE MATERIALIZED VIEW "+schema+".buildingusage_geoserver AS\n" +
                        "WITH uuid_table AS (\n" +
                        "    SELECT strval AS uuid, cityobject_id\n" +
                        "    FROM citydb.cityobject_genericattrib\n" +
                        "    WHERE attrname = 'uuid'\n" +
                        "), iri_table AS (\n" +
                        "    SELECT urival AS iri, cityobject_id\n" +
                        "    FROM citydb.cityobject_genericattrib\n" +
                        "    WHERE attrname = 'iri'\n" +
                        "), usageTable AS (\n" +
                        "    SELECT building_iri AS iri, propertyusage_iri, ontobuilt, usageshare\n" +
                        "    FROM "+schema+".usage\n" +
                        "), pointsTable AS (\n" +
                        "    SELECT building_iri AS iri, name\n" +
                        "    FROM "+osmSchema+".points\n" +
                        "), polygonsTable AS (\n" +
                        "    SELECT building_iri AS iri, name\n" +
                        "    FROM "+osmSchema+".polygons\n" +
                        ")\n" +
                        "SELECT DISTINCT\n" +
                        "    b.id AS building_id,\n" +
                        "    CASE\n" +
                        "        WHEN COALESCE(pointsTable.name, polygonsTable.name) IS NOT NULL\n" +
                        "        THEN COALESCE(pointsTable.name, polygonsTable.name)\n" +
                        "        ELSE CONCAT('Building ', uuid_table.cityobject_id)\n" +
                        "    END AS name,\n" +
                        "    COALESCE(measured_height, 100.0) AS building_height,\n" +
                        "    public.ST_Transform(geometry, 4326) AS geom,\n" +
                        "    uuid,\n" +
                        "    iri_table.iri,\n" +
                        "    propertyusage_iri,\n" +
                        "    ontobuilt,\n" +
                        "    usageshare\n" +
                        "FROM\n" +
                        "    citydb.building b\n" +
                        "JOIN\n" +
                        "    citydb.surface_geometry sg ON sg.root_id = b.lod0_footprint_id\n" +
                        "JOIN\n" +
                        "    uuid_table ON b.id = uuid_table.cityobject_id\n" +
                        "JOIN\n" +
                        "    iri_table ON b.id = iri_table.cityobject_id\n" +
                        "LEFT JOIN\n" +
                        "    pointsTable ON uuid_table.uuid = pointsTable.iri\n" +
                        "LEFT JOIN\n" +
                        "    polygonsTable ON uuid_table.uuid = polygonsTable.iri\n" +
                        "LEFT JOIN\n" +
                        "    usageTable ON uuid_table.uuid = usageTable.iri\n" +
                        "WHERE\n" +
                        "    sg.geometry IS NOT NULL\n" +
                        "    AND COALESCE(measured_height, 100.0) != '0';\n" +
                        "CREATE INDEX usage_index ON "+schema+".buildingusage_geoserver (ontobuilt);\n" + 
                        "CREATE INDEX geometry_index ON "+schema+".buildingusage_geoserver USING GIST (geom);";


                rdbStoreClient.executeUpdate(materializedView);
                rdbStoreClient.executeUpdate(materializedView_geoserver);
        }
}
