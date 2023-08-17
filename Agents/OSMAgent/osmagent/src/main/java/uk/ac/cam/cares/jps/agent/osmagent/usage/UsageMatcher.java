package uk.ac.cam.cares.jps.agent.osmagent.usage;

import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;

public class UsageMatcher {
    public void updateOntoBuilt(Connection conn) {
        List<String> tableNames = List.of("points", "polygons");

        for (String tableName : tableNames) {

            try {

                InputStreamReader inputStreamReader = new InputStreamReader(
                        UsageMatcher.class.getResourceAsStream("/osmbase.csv"));
                CSVReader csvReader = new CSVReaderBuilder(inputStreamReader).withSkipLines(1).build();
                String[] line;

                Map<String, StringBuilder> hashmap = new HashMap<>();

                while ((line = csvReader.readNext()) != null) {
                    String ontobuilt = line[3];
                    String key = line[0];
                    String value = line[1];

                    if (!ontobuilt.equals("NA") && !ontobuilt.isBlank()) {

                        if (!hashmap.containsKey(ontobuilt)) {
                            hashmap.put(ontobuilt, new StringBuilder(
                                    "UPDATE " + tableName + " SET ontobuilt = '" + ontobuilt + "' WHERE "));
                        }

                        if (!(tableName.equals("points") && key.equals("leisure"))) {
                            StringBuilder sqlStatement = hashmap.get(ontobuilt);
                            sqlStatement.append(key).append(" = '").append(value).append("' OR ");
                        }
                    }
                }

                csvReader.close();

                for (Map.Entry<String, StringBuilder> entry : hashmap.entrySet()) {
                    String key = entry.getKey();
                    String sql = entry.getValue().toString();
                    // Remove the trailing "' OR " from the SQL statement
                    sql = sql.substring(0, sql.length() - 4);
                    if (key.equals("RetailFacility")) {
                        sql = sql.concat(" OR shop is NOT NULL");
                    } else if ((tableName.equals("polygons") && key.equals("Office"))) {
                        sql = sql.concat(" OR office IS NOT NULL");
                    }

                    sql = sql.concat(";");
                    System.out.println(sql);

                    // Execute the SQL statement
                    try (Statement statement = conn.createStatement()) {
                        statement.executeUpdate(sql);
                    } catch (SQLException e) {
                        e.printStackTrace();
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

}
