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

    public static List<String> readClientPropertiesTemplateFile(String espHomeHost, String espUpdateHost, String sparqlQueryHost, String sparqlUpdateHost, String sparqlUser, String sparqlPassword, String dbUrl, String dbUser, String dbPassword, String wacnetApiPath) {
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
                } else if (line.contains("[SPARQL_QUERY_HOST]") && sparqlQueryHost != null) {
                    results.add(line.replace("[SPARQL_QUERY_HOST]", sparqlQueryHost));
                } else if (line.contains("[SPARQL_UPDATE_HOST]") && sparqlUpdateHost != null) {
                    results.add(line.replace("[SPARQL_UPDATE_HOST]", sparqlUpdateHost));
                } else if (line.contains("[SPARQL_USER]") && sparqlUser != null) {
                    results.add(line.replace("[SPARQL_USER]", sparqlUser));
                } else if (line.contains("[SPARQL_PASSWORD]") && sparqlPassword != null) {
                    results.add(line.replace("[SPARQL_PASSWORD]", sparqlPassword));
                } else if (line.contains("[DB_URL]") && dbUrl != null) {
                    results.add(line.replace("[DB_URL]", dbUrl));
                } else if (line.contains("[DB_USER]") && dbUser != null) {
                    results.add(line.replace("[DB_USER]", dbUser));
                } else if (line.contains("[DB_PASSWORD]") && dbPassword != null) {
                    results.add(line.replace("[DB_PASSWORD]", dbPassword));
                } else if (line.contains("[WACNET_API_PATH]") && wacnetApiPath != null) {
                    results.add(line.replace("[WACNET_API_PATH]", wacnetApiPath));
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return results;
    }

    public static List<String> readSetClientPropertiesTemplateFile(String espHomeHost, String espUpdateHost, String sparqlQueryHost, String sparqlUpdateHost) {
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
                } else if (line.contains("[SPARQL_QUERY_HOST]") && sparqlQueryHost != null) {
                    results.add(line.replace("[SPARQL_QUERY_HOST]", sparqlQueryHost));
                } else if (line.contains("[SPARQL_UPDATE_HOST]") && sparqlUpdateHost != null) {
                    results.add(line.replace("[SPARQL_UPDATE_HOST]", sparqlUpdateHost));
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return results;
    }


    public static List<String> readSparqlClientPropertiesTemplateFile(String sparqlQueryHost, String sparqlUpdateHost, String sparqlUser, String sparqlPassword) {
        String path = "src/test/resources/";

        File file = new File(path + "testPropertyFile");
        String templatePath = file.getAbsolutePath();

        List<String> results = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(templatePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                // not adding the property line if the host value is null. This is set up to test missing property.
                if (line.contains("[SPARQL_QUERY_HOST]") && sparqlQueryHost != null) {
                    results.add(line.replace("[SPARQL_QUERY_HOST]", sparqlQueryHost));
                } else if (line.contains("[SPARQL_UPDATE_HOST]") && sparqlUpdateHost != null) {
                    results.add(line.replace("[SPARQL_UPDATE_HOST]", sparqlUpdateHost));
                } else if (line.contains("[SPARQL_USER]") && sparqlUser != null) {
                    results.add(line.replace("[SPARQL_USER]", sparqlUser));
                } else if (line.contains("[SPARQL_PASSWORD]") && sparqlPassword != null) {
                    results.add(line.replace("[SPARQL_PASSWORD]", sparqlPassword));
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return results;
    }

    public static List<String> readWriteClientPropertiesTemplateFile(String sparqlQueryHost, String sparqlUpdateHost, String sparqlUser, String sparqlPassword, String dbUrl, String dbUser, String dbPassword) {
        String path = "src/test/resources/";

        File file = new File(path + "testPropertyFile");
        String templatePath = file.getAbsolutePath();

        List<String> results = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(templatePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                // not adding the property line if the host value is null. This is set up to test missing property.
                if (line.contains("[SPARQL_QUERY_HOST]") && sparqlQueryHost != null) {
                    results.add(line.replace("[SPARQL_QUERY_HOST]", sparqlQueryHost));
                } else if (line.contains("[SPARQL_UPDATE_HOST]") && sparqlUpdateHost != null) {
                    results.add(line.replace("[SPARQL_UPDATE_HOST]", sparqlUpdateHost));
                } else if (line.contains("[SPARQL_USER]") && sparqlUser != null) {
                    results.add(line.replace("[SPARQL_USER]", sparqlUser));
                } else if (line.contains("[SPARQL_PASSWORD]") && sparqlPassword != null) {
                    results.add(line.replace("[SPARQL_PASSWORD]", sparqlPassword));
                } else if (line.contains("[DB_URL]") && dbUrl != null) {
                    results.add(line.replace("[DB_URL]", dbUrl));
                } else if (line.contains("[DB_USER]") && dbUser != null) {
                    results.add(line.replace("[DB_USER]", dbUser));
                } else if (line.contains("[DB_PASSWORD]") && dbPassword != null) {
                    results.add(line.replace("[DB_PASSWORD]", dbPassword));
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return results;
    }
}
