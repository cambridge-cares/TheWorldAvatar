package uk.ac.cam.cares.jps.agent.osmagent.usage;

import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;

public class UsageMatcher {



    public static void main(String[] args) {
        List<String> tableNames = List.of("points","polygons");

        for (String tableName : tableNames) {

            try {
                CSVReader csvReader = new CSVReaderBuilder(new FileReader("src/main/resources/osmbase.csv")).withSkipLines(1).build();
                String[] line;

                Map<String, StringBuilder> hashmap = new HashMap<>();

                while ((line = csvReader.readNext()) != null) {
                    String ontobuilt = line[3];
                    String key = line[0];
                    String value = line[1];

                    if (!hashmap.containsKey(ontobuilt)) {
                        hashmap.put(ontobuilt, new StringBuilder("UPDATE " + tableName + " SET ontobuilt = '" + ontobuilt + "' WHERE "));
                    }

                    if (!(tableName.equals("points") && key.equals( "leisure"))) {
                        StringBuilder sqlStatement = hashmap.get(ontobuilt);
                        sqlStatement.append(key).append(" = '").append(value).append("' OR ");
                    }


                }

                csvReader.close();

                for (Map.Entry<String, StringBuilder> entry : hashmap.entrySet()) {
                    String sql = entry.getValue().toString();
                    // Remove the trailing "' OR " from the SQL statement
                    sql = sql.substring(0, sql.length() - 4);
                    sql = sql.concat(";");
                    System.out.println(sql);
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

}
