package uk.ac.cam.cares.jps.agent.osmagent.usage;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.util.UUID;

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
                String createIRI = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";\n" +
                        "UPDATE %s SET propertyusage_iri = ontobuilt || \'_\' || uuid_generate_v4()::text";

                rdbStoreClient.executeUpdate(String.format(createIRI, usageTable));

                String consistentIRI = "WITH temp AS (SELECT building_iri, ontobuilt, MIN(propertyusage_iri) AS consistent FROM %s GROUP BY building_iri, ontobuilt)\n" +
                        "UPDATE %s u SET propertyusage_iri = consistent FROM temp t WHERE u.building_iri = t.building_iri AND t.ontobuilt = u.ontobuilt";

                rdbStoreClient.executeUpdate(String.format(consistentIRI, usageTable, usageTable));
        }

        public void usageShareCount(String view, String usageTable, String pointTable, String polygonTable) {
                String sql = "DROP MATERIALIZED VIEW IF EXISTS %s CASCADE;\n" +
                        "CREATE MATERIALIZED VIEW %s AS\n" +
                        "WITH counts AS (SELECT building_iri, ontobuilt, propertyusage_iri,  COUNT(*) AS c FROM %s GROUP BY building_iri, ontobuilt, propertyusage_iri),\n" +
                        "total AS (SELECT building_iri, SUM(c) AS total FROM counts GROUP BY building_iri),\n" +
                        "names AS (SELECT building_iri, ontobuilt, name FROM %s WHERE building_iri IS NOT NULL\n" +
                        "UNION SELECT building_iri, ontobuilt, name FROM %s WHERE building_iri IS NOT NULL)\n" +
                        "SELECT c.building_iri, c.ontobuilt, c.propertyusage_iri, (c.c::DOUBLE PRECISION/t.total) AS usageshare, n.name\n" +
                        "FROM counts AS c\n" +
                        "JOIN total AS t ON c.building_iri = t.building_iri\n" +
                        "LEFT JOIN names AS n on c.building_iri = n.building_iri AND c.ontobuilt = n.ontobuilt;\n" +
                        "CREATE INDEX usage_count_index ON %s (building_iri);";

                rdbStoreClient.executeUpdate(String.format(sql, view, view, usageTable, pointTable, polygonTable, view));
        }


        public void usageShareArea(String view, String usageTable, String pointTable, String polygonTable) {
                String sql = "DROP MATERIALIZED VIEW IF EXISTS %s CASCADE;\n" +
                        "CREATE MATERIALIZED VIEW %s AS\n" +
                        "WITH counts AS (SELECT building_iri, ontobuilt, propertyusage_iri, COUNT(*) AS c,\n" +
                        "SUM(CASE WHEN source = \'osm_polygons\' THEN area ELSE 0 END) AS area_sum FROM %s\n" +
                        "GROUP BY building_iri, ontobuilt, propertyusage_iri),\n" +
                        "total AS (SELECT building_iri, SUM(c) AS total_c, SUM(area_sum) AS total_area FROM counts GROUP BY building_iri),\n" +
                        "names AS (SELECT building_iri, ontobuilt, name FROM %s WHERE building_iri IS NOT NULL\n" +
                        "UNION SELECT building_iri, ontobuilt, name FROM %s WHERE building_iri IS NOT NULL),\n" +
                        "intermediate AS (SELECT c.building_iri, c.ontobuilt, c.propertyusage_iri, n.name, CASE\n" +
                        "WHEN t.total_area = 0 THEN c.c/t.total_c\n" +
                        "ELSE c.area_sum/t.total_area END AS usageshare\n" +
                        "FROM counts AS c\n" +
                        "JOIN total AS t\n" +
                        "ON c.building_iri = t.building_iri\n" +
                        "LEFT JOIN names AS n on c.building_iri = n.building_iri AND c.ontobuilt = n.ontobuilt)\n" +
                        "SELECT * FROM intermediate WHERE usageshare <> 0;\n" +
                        "CREATE INDEX usage_area_index ON %s (building_iri);";

                rdbStoreClient.executeUpdate(String.format(sql, view, view, usageTable, pointTable, polygonTable, view));
        }

        public void addGeoserverView(String view, String usageView, String pointTable, String polygonTable) {
                String materializedView_geoserver= "-- Drop the materialized view if it exists\n" +
                        "DROP MATERIALIZED VIEW IF EXISTS %s;\n" +
                        "CREATE MATERIALIZED VIEW %s AS\n" +
                        "WITH osm AS (SELECT building_iri, area, name FROM %s WHERE building_iri IS NOT NULL\n" +
                        "UNION SELECT building_iri, area, name FROM %s WHERE building_iri IS NOT NULL),\n" +
                        "names AS (SELECT o.building_iri, o.name FROM osm AS o JOIN (" +
                        "SELECT building_iri,  MAX(COALESCE(area, 0)) AS m FROM OSM GROUP BY building_iri) AS max\n" +
                        "ON o.building_iri = max.building_iri AND COALESCE(o.area, 0) = max.m),\n" +
                        "geometry AS (SELECT cga.strval AS uuid, public.ST_Collect(public.ST_Transform(sg.geometry, 4326)) AS geometry, AVG(b.measured_height) AS building_height\n" +
                        "FROM citydb.building AS b\n" +
                        "INNER JOIN citydb.cityobject_genericattrib AS cga ON b.id = cga.cityobject_id\n" +
                        "INNER JOIN citydb.surface_geometry AS sg ON b.lod0_footprint_id = sg.parent_id\n" +
                        "WHERE sg.geometry IS NOT NULL AND cga.attrname = \'uuid\'\n" +
                        "GROUP BY cga.strval)\n" +
                        "SELECT u.building_iri AS uuid, CONCAT('https://www.theworldavatar.com/kg/Building/', u.building_iri) AS iri, " +
                        "u.ontobuilt, u.usageshare, n.name, g.geometry, g.building_height\n" +
                        "FROM %s AS u\n" +
                        "LEFT JOIN geometry AS g ON u.building_iri = g.uuid\n" +
                        "LEFT JOIN names AS n ON u.building_iri = n.building_iri;\n" +
                        "CREATE INDEX \"%s\" ON %s (ontobuilt);\n" +
                        "CREATE INDEX \"%s\" ON %s USING GIST (geometry);";

                String usage_index = "usage_index_" + UUID.randomUUID();
                String geo_index = "geo_index_" + UUID.randomUUID();

                rdbStoreClient.executeUpdate(String.format(materializedView_geoserver, view, view, pointTable, polygonTable, usageView, usage_index, view, geo_index, view));
        }
}
