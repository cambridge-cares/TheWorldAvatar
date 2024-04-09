package uk.ac.cam.cares.jps.agent.osmagent.usage;

import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.agent.osmagent.FileReader;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.io.*;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
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

        /**
         * Assigns OntoBuiltEnv:PropertyUsage IRI and usage share for building IRIs in usageTable
         * @param usageTable centralised table to store usage information
         */
        public void updateUsageShare(String usageTable) {
                String add_uuid_ossp_Extension = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";";

                // assign usageshare and propertyusage_iri
                String assignUsageShare =
                        "UPDATE " + usageTable + " AS p\n" +
                                "SET UsageShare = c.instances / c.total_instances::float,\n" +
                                "    propertyusage_iri = c.ontobuilt || '_' || uuid_generate_v4()::text\n"
                                +
                                "FROM (\n" +
                                "    SELECT building_iri,\n" +
                                "           ontobuilt,\n" +
                                "           COUNT(*) AS instances,\n" +
                                "           SUM(COUNT(*)) OVER (PARTITION BY building_iri) AS total_instances\n"
                                +
                                "    FROM " + usageTable + " \n" +
                                "    GROUP BY building_iri, ontobuilt\n" +
                                ") AS c\n" +
                                "WHERE p.building_iri = c.building_iri\n" +
                                "  AND p.ontobuilt = c.ontobuilt;";

                // ensures that for the same building_iri with the same ontobuilt, propertyusage_iri is the same
                String updatePropertyUsageStatement =
                        "UPDATE " + usageTable + " AS p\n" +
                                "SET propertyusage_iri = subquery.min_propertyusage_iri\n" +
                                "FROM (\n" +
                                "    SELECT p.building_iri, p.ontobuilt, MIN(propertyusage_iri) AS min_propertyusage_iri\n"
                                +
                                "    FROM " + usageTable + " AS p\n" +
                                "    INNER JOIN (\n" +
                                "        SELECT building_iri, ontobuilt\n" +
                                "        FROM " + usageTable + " \n" +
                                "            WHERE ontobuilt IS NOT NULL AND building_iri IS NOT NULL\n"
                                +
                                "    ) AS cd ON p.building_iri = cd.building_iri AND p.ontobuilt = cd.ontobuilt\n"
                                +
                                "    GROUP BY p.building_iri, p.ontobuilt\n" +
                                "    HAVING COUNT(*) > 1\n" +
                                ") AS subquery\n" +
                                "WHERE p.building_iri = subquery.building_iri\n" +
                                "    AND p.ontobuilt = subquery.ontobuilt;";

                // execute the SQL statement
                rdbStoreClient.executeUpdate(add_uuid_ossp_Extension);
                rdbStoreClient.executeUpdate(assignUsageShare);
                rdbStoreClient.executeUpdate(updatePropertyUsageStatement);
                System.out.println("UsageShare calculated and propertyUsage assigned.");

        }

        public void addMaterializedView(String usageTable, String osmSchema) {
                String materializedView ="-- Drop the materialized view if it exists\n" +
                        "DROP MATERIALIZED VIEW IF EXISTS usage.buildingusage_osm;\n" +
                        "\n" +
                        "-- Create a new materialized view named \"buildingusage_osm\" in the \"usage\" schema\n" +
                        "CREATE MATERIALIZED VIEW usage.buildingusage_osm AS\n" +
                        "SELECT DISTINCT u.*, COALESCE(p.name, o.name) AS name\n" +
                        "FROM "+usageTable+" AS u\n" +
                        "LEFT JOIN "+osmSchema+".points AS p ON u.building_iri = p.building_iri\n" +
                        "LEFT JOIN "+osmSchema+".polygons AS o ON u.building_iri = o.building_iri;";

                String materializedView_geoserver= "-- Drop the materialized view if it exists\n" +
                        "DROP MATERIALIZED VIEW IF EXISTS usage.buildingusage_geoserver;\n" +
                        "\n" +
                        "CREATE MATERIALIZED VIEW usage.buildingusage_geoserver AS\n" +
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
                        "    FROM usage.usage\n" +
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
                        "CREATE INDEX usage_index ON usage.buildingusage_geoserver (ontobuilt);\n" + 
                        "CREATE INDEX geometry_index ON usage.buildingusage_geoserver USING GIST (geom);";


                rdbStoreClient.executeUpdate(materializedView);
                rdbStoreClient.executeUpdate(materializedView_geoserver);
        }
}
