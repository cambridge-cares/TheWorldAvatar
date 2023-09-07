package uk.ac.cam.cares.jps.agent.osmagent.usage;

import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import uk.ac.cam.cares.jps.agent.osmagent.OSMAgent;
import uk.ac.cam.cares.jps.agent.osmagent.geometry.object.GeoObject;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.io.IOException;
import java.io.InputStreamReader;

public class UsageShareCalculator {
        private RemoteRDBStoreClient rdbStoreClient;
        public UsageShareCalculator(String database, String user, String password) {
                this.rdbStoreClient = new RemoteRDBStoreClient(database, user, password);

        }

        /**
         * UsageShareCalculator contains 3 parts that run using SQL query
         * 1) assignUsageShare - For each building_iri, assign Propertyusage_iri,
         * calculate and assign usageShare to each OSM item.
         * 2) updatePropertyUsageStatement - For each building_iri, that has two OSM
         * items, check if they are the same ontoBuilt type,
         * if they are the same ontoBuilt type, return the first instance as the
         * propertyusage_iri
         * 3) landUseMatcher - For building_iri which are untagged to any osm_tags, as a
         * default fallback,
         * SQL query is run check and tag buildings with buildings_iri with
         * propertyusage_iri and usageshare as according to the osm_landuse.csv
         *
         * @param database
         * @param user
         * @param password
         */
        public void updateUsageShare(String usageTable) {
                String add_uuid_ossp_Extension = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";";

                String assignUsageShare =
                                // SQL to update polygons table
                                "UPDATE " + usageTable + " AS p\n" +
                                                "SET UsageShare = c.instances / c.total_instances::float,\n" +
                                                "    propertyusage_iri = 'https://www.theworldavatar.com/kg/' || c.ontobuilt || '_' || uuid_generate_v4()::text\n"
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

                String updatePropertyUsageStatement =
                                // SQL to update polygons table
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

                // Execute the SQL statement

                rdbStoreClient.executeUpdate(add_uuid_ossp_Extension);
                rdbStoreClient.executeUpdate(assignUsageShare);
                rdbStoreClient.executeUpdate(updatePropertyUsageStatement);
                System.out.println("UsageShare calculated and propertyUsage assigned.");

        }

        public void updateLandUse(String usageTable, String landUseTable) throws IOException {
                InputStreamReader inputStreamReader = new InputStreamReader(
                                UsageMatcher.class.getResourceAsStream("/dlm_landuse.csv"));
                CSVReader csvReader = new CSVReaderBuilder(inputStreamReader).withSkipLines(1).build();
                String[] line;

                while ((line = csvReader.readNext()) != null) {
                        String ontobuilt = line[3];
                        String key = line[0];
                        String value = line[1];

                        String updateLandUse = "INSERT INTO " + usageTable + " (building_iri, ontobuilt) \n" +
                                "SELECT q2.iri, \'" + ontobuilt + "\' FROM \n" +
                                "(SELECT q.urival AS iri, q.geometry AS geo, q.srid AS srid FROM \n" +
                                "(" + GeoObject.getQuery() + ") AS q \n" +
                                "LEFT JOIN " + usageTable + " u ON q.urival = u.building_iri \n" +
                                "WHERE u.building_iri IS NULL) AS q2 \n" +
                                "WHERE ST_Intersects(q2.geo, ST_Transform(\n" +
                                "(SELECT ST_Collect(wkb_geometry) AS g FROM " + landUseTable + " \n" +
                                "WHERE \"" + key + "\" = \'" + value +"\'), q2.srid))";

                        rdbStoreClient.executeUpdate(updateLandUse);
                        System.out.println(
                                        "Untagged buildings with building_iri are assigned for " + key + " with value:"
                                                        + value + " under the ontobuiltenv:" + ontobuilt
                                                        + " category.");


                }

                System.out.println(
                                "Untagged building has been assigned an ontobuilt type according to the corresponding landuse.");
                csvReader.close();

        }
}
