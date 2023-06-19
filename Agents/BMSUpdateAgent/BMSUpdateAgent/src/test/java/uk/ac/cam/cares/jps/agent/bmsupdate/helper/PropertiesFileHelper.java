package uk.ac.cam.cares.jps.agent.bmsupdate.helper;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class PropertiesFileHelper {
    public static void writeToTempFolder(String filepath, List<String> properties) throws IOException {
        FileWriter writer = new FileWriter(filepath, false);
        for (String s : properties) {
            writer.write(s + "\n");
        }
        writer.close();
    }

    public static List<String> readClientPropertiesTemplateFile(String espHomeHost, String espUpdateHost, String blazegraphQueryHost, String blazegraphUpdateHost) {
        String path = "src/test/resources/";

        File file = new File(path + "testPropertyFile");
        String templatePath = file.getAbsolutePath();

        List<String> results = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(templatePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                // not adding the property line if the host value is null. This is set up to test missing property.
                if (line.contains("[ESP_HOST]") && espHomeHost != null) {
                    results.add(line.replace("[ESP_HOST]", espHomeHost));
                } else if (line.contains("[ESP_UPDATE_HOST]") && espUpdateHost != null) {
                    results.add(line.replace("[ESP_UPDATE_HOST]", espUpdateHost));
                } else if (line.contains("[BLAZEGRAPH_QUERY_HOST]") && blazegraphQueryHost != null) {
                    results.add(line.replace("[BLAZEGRAPH_QUERY_HOST]", blazegraphQueryHost));
                } else if (line.contains("[BLAZEGRAPH_UPDATE_HOST]") && blazegraphUpdateHost != null) {
                    results.add(line.replace("[BLAZEGRAPH_UPDATE_HOST]", blazegraphUpdateHost));
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return results;
    }
}
