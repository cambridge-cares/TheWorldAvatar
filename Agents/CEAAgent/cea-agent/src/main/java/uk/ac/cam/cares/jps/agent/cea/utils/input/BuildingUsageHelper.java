package uk.ac.cam.cares.jps.agent.cea.utils.input;

import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.query.Query;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class BuildingUsageHelper {
    
    /**
     * Process query result of building usage
     * 
     * @param queryResultArray query result
     * @return the usages and their corresponding weighting
     */
    private static Map<String, Double> processBuildingUsageData(JSONArray queryResultArray) {
        Map<String, Double> result = new HashMap<>();
        Map<String, Double> temp = new HashMap<>();
        String usage;
        if (queryResultArray.isEmpty()) {
            usage = toCEAConvention("default");
            result.put(usage, 1.00);
        } else if (queryResultArray.length() == 1) {
            usage = queryResultArray.getJSONObject(0).get("BuildingUsage").toString()
                    .split(OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))[1].split(">")[0]
                    .toUpperCase();
            usage = toCEAConvention(usage);
            result.put(usage, 1.00);
        } else {
            for (int i = 0; i < queryResultArray.length(); i++) {
                usage = queryResultArray.getJSONObject(i).get("BuildingUsage").toString()
                        .split(OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))[1].split(">")[0]
                        .toUpperCase();
                usage = toCEAConvention(usage);

                if (temp.containsKey(usage)) {
                    temp.put(usage, temp.get(usage) + queryResultArray.getJSONObject(i).getDouble("UsageShare"));
                } else {
                    temp.put(usage, queryResultArray.getJSONObject(i).getDouble("UsageShare"));
                }
            }

            // get the top 3 usages
            result = temp.entrySet().stream()
                    .sorted(Map.Entry.comparingByValue(Comparator.reverseOrder()))
                    .limit(3)
                    .collect(Collectors.toMap(
                            Map.Entry::getKey, Map.Entry::getValue, (e1, e2) -> e1, LinkedHashMap::new));

            // normalise the usage weights in result so that they sum up to 1
            Double sum = 0.00;

            for (Double val : result.values()) {
                sum += val;
            }

            for (Map.Entry<String, Double> entry : result.entrySet()) {
                result.put(entry.getKey(), entry.getValue() / sum);
            }
        }

        return result;
    }

    /**
     * Retrieves the usages of a list of building and each usage's corresponding weight, and
     * returns the usages and their weight as a map
     * 
     * @param uriStringList list of building IRI
     * @param endpoint  SPARQL endpoint
     * @return the usages and their corresponding weighting
     */

    public static List<Map<String, Double>> bulkGetBuildingUsages(List<String> uriStringList, String endpoint) {

        Query query = bulkGetBuildingUsageQuery(uriStringList);

        JSONArray queryResultArray;

        RemoteStoreClient remoteStoreClient = new RemoteStoreClient(endpoint);

        try {
            queryResultArray = remoteStoreClient.executeQuery(query.toString());
        } catch (Exception e) {
            System.out.println("No building usage retrieved, agent will run CEA with CEA's default building usage.");
            queryResultArray = new JSONArray();
        }

        // create HashMap to group results per building IRI

        Map<String, JSONArray> queryMap = new HashMap<>();

        // Traverse JSONArray and group by building IRI
        for (int i = 0; i < queryResultArray.length(); i++) {
            JSONObject row = queryResultArray.getJSONObject(i);
            String buildingIRI = row.getString("building");
            queryMap.computeIfAbsent(buildingIRI, k -> new JSONArray()).put(row);
        }

        List<Map<String, Double>> listBuildingUsageMap = new ArrayList<>();

        for (String uri : uriStringList) {
            JSONArray resultJSONArray = queryMap.getOrDefault(uri, new JSONArray());
            listBuildingUsageMap.add(processBuildingUsageData(resultJSONArray));
        }

        return listBuildingUsageMap;

    }

    /**
     * Builds a SPARQL query for a list of URI to retrieve the building usages and
     * the building usage share with OntoBuiltEnv concepts
     * 
     * @param uriStringList list of building IRI
     * @return returns a query string
     */
    private static Query bulkGetBuildingUsageQuery(List<String> uriStringList) {
        WhereBuilder wb = new WhereBuilder();
        SelectBuilder sb = new SelectBuilder();
        wb.addPrefix("ontoBuiltEnv", OntologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv))
                .addPrefix("rdf", OntologyURIHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addWhere("?building", "ontoBuiltEnv:hasPropertyUsage", "?usage")
                .addWhere("?usage", "rdf:type", "?BuildingUsage")
                .addOptional("?usage", "ontoBuiltEnv:hasUsageShare", "?UsageShare");

        sb.addVar("?BuildingUsage").addVar("?UsageShare").addVar("?building")
                .addWhere(wb);
        // Add VALUES clause for building IRIs
        for (String uri : uriStringList) {
            sb.addValueVar("?building", NodeFactory.createURI(uri));
        }
        return sb.build();
    }

    /**
     * Converts OntoBuiltEnv building usage type to convention used by CEA
     * 
     * @param usage OntoBuiltEnv building usage type
     * @return building usage per CEA convention
     */
    public static String toCEAConvention(String usage) {
        switch (usage) {
            case ("DOMESTIC"):
                return "MULTI_RES";
            case ("SINGLERESIDENTIAL"):
                return "SINGLE_RES";
            case ("MULTIRESIDENTIAL"):
                return "MULTI_RES";
            case ("EMERGENCYSERVICE"):
                return "HOSPITAL";
            case ("FIRESTATION"):
                return "HOSPITAL";
            case ("POLICESTATION"):
                return "HOSPITAL";
            case ("MEDICALCARE"):
                return "HOSPITAL";
            case ("HOSPITAL"):
                return usage;
            case ("CLINIC"):
                return "HOSPITAL";
            case ("EDUCATION"):
                return "UNIVERSITY";
            case ("SCHOOL"):
                return usage;
            case ("UNIVERSITYFACILITY"):
                return "UNIVERSITY";
            case ("OFFICE"):
                return usage;
            case ("RETAILESTABLISHMENT"):
                return "RETAIL";
            case ("RELIGIOUSFACILITY"):
                return "MUSEUM";
            case ("INDUSTRIALFACILITY"):
                return "INDUSTRIAL";
            case ("EATINGESTABLISHMENT"):
                return "RESTAURANT";
            case ("DRINKINGESTABLISHMENT"):
                return "RESTAURANT";
            case ("HOTEL"):
                return usage;
            case ("SPORTSFACILITY"):
                return "GYM";
            case ("CULTURALFACILITY"):
                return "MUSEUM";
            case ("TRANSPORTFACILITY"):
                return "INDUSTRIAL";
            case ("NON-DOMESTIC"):
                return "INDUSTRIAL";
            default:
                return "MULTI_RES";
        }
    }
}
