package uk.ac.cam.cares.jps.agent.osmagent.usage;

import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import uk.ac.cam.cares.jps.agent.osmagent.OSMAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;

public class UsageShareCalculator {

    /**
     * UsageShareCalculator contains 3 parts that run using SQL query
     * 1) assignUsageShare - For each building_iri, assign Propertyusage_iri, calculate and assign usageShare to each OSM item.
     * 2) updatePropertyUsageStatement - For each building_iri, that has two OSM items, check if they are the same ontoBuilt type,
     * if they are the same ontoBuilt type, return the first instance as the propertyusage_iri
     * 3) landUseMatcher - For building_iri which are untagged to any osm_tags, as a default fallback,
     * SQL query is run check and tag buildings with buildings_iri with propertyusage_iri and usageshare as according to the osm_landuse.csv
     *
     * @param database
     * @param user
     * @param password
     */
    public static void updateUsageShare(String database, String user, String password) {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(database, user, password);

        String points = OSMAgent.POINT_TABLE;
        String polygons = OSMAgent.POLYGON_TABLE;

        String add_uuid_ossp_Extension = "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";";

        String assignUsageShare = "UPDATE " + polygons + " AS p\n" +
                "SET UsageShare = c.instances / c.total_instances::float,\n" +
                "    propertyusage_iri = 'https://www.theworldavatar.com/kg/' || c.OntoBuilt || '_' || uuid_generate_v4()::text\n"
                +
                "FROM (\n" +
                "    SELECT building_iri,\n" +
                "           OntoBuilt,\n" +
                "           COUNT(*) AS instances,\n" +
                "           SUM(COUNT(*)) OVER (PARTITION BY building_iri) AS total_instances\n" +
                "    FROM (\n" +
                "        SELECT building_iri, OntoBuilt FROM " + polygons + "\n" +
                "        WHERE OntoBuilt IS NOT NULL AND building_iri IS NOT NULL\n" +
                "        UNION ALL\n" +
                "        SELECT building_iri, OntoBuilt FROM " + points + "\n" +
                "        WHERE OntoBuilt IS NOT NULL AND building_iri IS NOT NULL\n" +
                "    ) AS combined_data\n" +
                "    GROUP BY building_iri, OntoBuilt\n" +
                ") AS c\n" +
                "WHERE p.building_iri = c.building_iri\n" +
                "  AND p.OntoBuilt = c.OntoBuilt;\n" +
                "\n" +
                "-- Update the \"points\" table\n" +
                "UPDATE " + points + " AS pt\n" +
                "SET UsageShare = c.instances / c.total_instances::float,\n" +
                "    propertyusage_iri = 'https://www.theworldavatar.com/kg/' || c.OntoBuilt || '_' || uuid_generate_v4()::text\n"
                +
                "FROM (\n" +
                "    SELECT building_iri,\n" +
                "           OntoBuilt,\n" +
                "           COUNT(*) AS instances,\n" +
                "           SUM(COUNT(*)) OVER (PARTITION BY building_iri) AS total_instances\n" +
                "    FROM (\n" +
                "        SELECT building_iri, OntoBuilt FROM " + polygons + "\n" +
                "        WHERE OntoBuilt IS NOT NULL AND building_iri IS NOT NULL\n" +
                "        UNION ALL\n" +
                "        SELECT building_iri, OntoBuilt FROM " + points + "\n" +
                "        WHERE OntoBuilt IS NOT NULL AND building_iri IS NOT NULL\n" +
                "    ) AS combined_data\n" +
                "    GROUP BY building_iri, OntoBuilt\n" +
                ") AS c\n" +
                "WHERE pt.building_iri = c.building_iri\n" +
                "  AND pt.OntoBuilt = c.OntoBuilt;";


        String updatePropertyUsageStatement = "-- Both table\n" +
                "UPDATE " + polygons + " AS p\n" +
                "SET propertyusage_iri = subquery.min_propertyusage_iri\n" +
                "FROM (\n" +
                "    SELECT p.building_iri, p.ontobuilt, MIN(propertyusage_iri) AS min_propertyusage_iri\n" +
                "    FROM " + polygons + " AS p\n" +
                "    INNER JOIN (\n" +
                "        SELECT building_iri, ontobuilt\n" +
                "        FROM (\n" +
                "            SELECT building_iri, OntoBuilt FROM " + polygons + "\n" +
                "            WHERE OntoBuilt IS NOT NULL AND building_iri IS NOT NULL\n" +
                "            UNION ALL\n" +
                "            SELECT building_iri, OntoBuilt FROM " + points + "\n" +
                "            WHERE OntoBuilt IS NOT NULL AND building_iri IS NOT NULL\n" +
                "        ) AS combined_data\n" +
                "    ) AS cd ON p.building_iri = cd.building_iri AND p.ontobuilt = cd.ontobuilt\n" +
                "    GROUP BY p.building_iri, p.ontobuilt\n" +
                "    HAVING COUNT(*) > 1\n" +
                ") AS subquery\n" +
                "WHERE p.building_iri = subquery.building_iri\n" +
                "    AND p.ontobuilt = subquery.ontobuilt;\n" +
                "\n" +
                "\n" +
                "-- Both table\n" +
                "UPDATE " + points + " AS p\n" +
                "SET propertyusage_iri = subquery.min_propertyusage_iri\n" +
                "FROM (\n" +
                "    SELECT p.building_iri, p.ontobuilt, MIN(propertyusage_iri) AS min_propertyusage_iri\n" +
                "    FROM " + points + " AS p\n" +
                "    INNER JOIN (\n" +
                "        SELECT building_iri, ontobuilt\n" +
                "        FROM (\n" +
                "            SELECT building_iri, OntoBuilt FROM " + polygons + "\n" +
                "            WHERE OntoBuilt IS NOT NULL AND building_iri IS NOT NULL\n" +
                "            UNION ALL\n" +
                "            SELECT building_iri, OntoBuilt FROM " + points + "\n" +
                "            WHERE OntoBuilt IS NOT NULL AND building_iri IS NOT NULL\n" +
                "        ) AS combined_data\n" +
                "    ) AS cd ON p.building_iri = cd.building_iri AND p.ontobuilt = cd.ontobuilt\n" +
                "    GROUP BY p.building_iri, p.ontobuilt\n" +
                "    HAVING COUNT(*) > 1\n" +
                ") AS subquery\n" +
                "WHERE p.building_iri = subquery.building_iri\n" +
                "    AND p.ontobuilt = subquery.ontobuilt;";

        String updateLanduse="";

        // Execute the SQL statement

        rdbStoreClient.executeUpdate(add_uuid_ossp_Extension);
        rdbStoreClient.executeUpdate(assignUsageShare);
        rdbStoreClient.executeUpdate(updatePropertyUsageStatement);
        System.out.println("UsageShare calculated and propertyUsage assigned.");

    }

    public static void updateLandUse (String database, String user, String password) throws IOException {

        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(database, user, password);

        String points = OSMAgent.POINT_TABLE;
        String polygons = OSMAgent.POLYGON_TABLE;

        InputStreamReader inputStreamReader = new InputStreamReader(
                UsageMatcher.class.getResourceAsStream("/dlm_landuse.csv"));
        CSVReader csvReader = new CSVReaderBuilder(inputStreamReader).withSkipLines(1).build();
        String[] line;

        while ((line = csvReader.readNext()) != null) {
            String ontobuilt = line[3];
            String key = line[0];
            String value = line[1];

            String updateLandusePoints="UPDATE public."+points+" AS p\n" +
                    "SET ontobuilt = '"+ontobuilt+"',\n" +
                    "propertyusage_iri = 'https://www.theworldavatar.com/kg/'||'"+ontobuilt+"'||'_' || uuid_generate_v4()::text,\n" +
                    "usageShare =1\n" +
                    "\n" +
                    "FROM public.dlmsie02f AS d\n" +
                    "WHERE p.building_iri IS NOT NULL\n" +
                    "  AND p.ontobuilt IS NULL\n" +
                    "  AND ST_Intersects(p.\"geometryProperty\", \n" +
                    "      ST_Transform((SELECT ST_Collect(wkb_geometry) \n" +
                    "                    FROM public.dlmsie02f \n" +
                    "                    WHERE \""+key+"\" = '"+value+"'), 4326)\n" +
                    "      );";

            String updateLandusePolygons="UPDATE public."+polygons+" AS p\n" +
                    "SET ontobuilt = '"+ontobuilt+"',\n" +
                    "propertyusage_iri = 'https://www.theworldavatar.com/kg/'||'"+ontobuilt+"'||'_' || uuid_generate_v4()::text,\n" +
                    "usageShare =1\n" +
                    "\n" +
                    "FROM public.dlmsie02f AS d\n" +
                    "WHERE p.building_iri IS NOT NULL\n" +
                    "  AND p.ontobuilt IS NULL\n" +
                    "  AND ST_Intersects(p.\"geometryProperty\", \n" +
                    "      ST_Transform((SELECT ST_Collect(wkb_geometry) \n" +
                    "                    FROM public.dlmsie02f \n" +
                    "                    WHERE \""+key+"\" = '"+value+"'), 4326)\n" +
                    "      );";

            rdbStoreClient.executeUpdate(updateLandusePoints);
            rdbStoreClient.executeUpdate(updateLandusePolygons);
        }

        csvReader.close();



    }
}
