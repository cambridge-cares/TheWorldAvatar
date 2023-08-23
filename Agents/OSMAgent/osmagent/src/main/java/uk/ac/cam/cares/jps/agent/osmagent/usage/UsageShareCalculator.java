package uk.ac.cam.cares.jps.agent.osmagent.usage;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;

public class UsageShareCalculator {

    public static void updateUsageShare(Connection conn, List<String> tableNames) {

        String points = tableNames.get(1);
        String polygons = tableNames.get(0);

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

        // Execute the SQL statement
        try (Statement statement = conn.createStatement()) {

            statement.executeUpdate(add_uuid_ossp_Extension);
            statement.executeUpdate(assignUsageShare);
            statement.executeUpdate(updatePropertyUsageStatement);
            System.out.println("UsageShare calculated and propertyUsage assigned.");

        } catch (SQLException e) {
            e.printStackTrace();
        }

    }
}
