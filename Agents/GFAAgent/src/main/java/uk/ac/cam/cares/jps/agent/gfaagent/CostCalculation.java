package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashMap;

import org.json.JSONArray;
import org.springframework.core.io.ClassPathResource;
import org.apache.commons.io.IOUtils;

import com.opencsv.bean.CsvToBeanBuilder;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class CostCalculation {
    private RemoteStoreClient remoteStoreClient;
    private Map<String, Building> iriToBuildingMap;

    public CostCalculation(String ontopUrl) {
        remoteStoreClient = new RemoteStoreClient(ontopUrl);
        iriToBuildingMap = new HashMap<>();
    }

    public void setBuildings() {
        String buildingQuery;
        try (InputStream is = new ClassPathResource("building.sparql").getInputStream()) {
            buildingQuery = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException("Error reading building.sparql", e);
        }

        JSONArray queryResult = remoteStoreClient.executeQuery(buildingQuery);

        for (int i = 0; i < queryResult.length(); i++) {
            String buildingIri = queryResult.getJSONObject(i).getString("building");
            String usage = queryResult.getJSONObject(i).getString("usage");
            double usageShare = queryResult.getJSONObject(i).getDouble("usageShare");
            double gfa = queryResult.getJSONObject(i).getDouble("gfa");

            iriToBuildingMap.computeIfAbsent(buildingIri, Building::new);
            iriToBuildingMap.get(buildingIri).addUsageShare(usage, usageShare);
            iriToBuildingMap.get(buildingIri).setGFA(gfa);
        }
    }

    public void calculateCost() {
        Map<String, Double> usageToCostMap = getUsageToCostMap(getAISCost());

        iriToBuildingMap.values().forEach(building -> {
            List<Double> costs = new ArrayList<>();
            // weight each usage cost according to usage share (area)
            building.getUsageShare().entrySet().forEach(entry -> {
                String usage = entry.getKey();
                double usageShare = entry.getValue();
                costs.add(usageToCostMap.get(usage) * usageShare * building.getGFA());
            });
            double totalCost = costs.stream().mapToDouble(Double::valueOf).sum();
            building.setCost(totalCost);
        });
    }

    public void uploadData(Connection conn) {
        GFAPostGISClient.createSchema(conn);
        GFAPostGISClient.createCostTable(conn);

        iriToBuildingMap.values().forEach(building -> {
            GFAPostGISClient.addCostData(building.getIri(), building.getCost(), conn);
        });
    }

    /**
     * reads the file ais_cost.csv and calculates average cost for each category
     * 
     * @return
     */
    private Map<String, Double> getAISCost() {
        List<AISCost> aisCost;

        try (InputStream is = new ClassPathResource("ais_cost.csv").getInputStream()) {
            aisCost = new CsvToBeanBuilder<AISCost>(new InputStreamReader(is)).withType(AISCost.class).build().parse();
        } catch (IOException e) {
            throw new RuntimeException("Error reading ais_cost.csv", e);
        }

        Map<String, List<Double>> individualCosts = new HashMap<>();

        aisCost.stream().forEach(row -> {
            individualCosts.computeIfAbsent(row.getCategory(), k -> new ArrayList<>());
            individualCosts.get(row.getCategory()).add(row.getAverage());
        });

        // calculate overall average for each category
        Map<String, Double> aisOverallAverage = new HashMap<>();

        individualCosts.entrySet().forEach(entry -> {
            aisOverallAverage.put(entry.getKey(),
                    entry.getValue().stream().mapToDouble(Double::valueOf).average().getAsDouble());
        });

        return aisOverallAverage;
    }

    private Map<String, Double> getUsageToCostMap(Map<String, Double> aisCost) {
        Map<String, Double> usageToCostMap = new HashMap<>();

        List<OntoBuiltEnvMapping> ontoBuiltEnvMapping;

        try (InputStream is = new ClassPathResource("ais_ontobuiltenv_mapping.csv").getInputStream()) {
            ontoBuiltEnvMapping = new CsvToBeanBuilder<OntoBuiltEnvMapping>(new InputStreamReader(is))
                    .withType(OntoBuiltEnvMapping.class).build().parse();
        } catch (IOException e) {
            throw new RuntimeException("Error reading ais_ontobuiltenv_mapping.csv", e);
        }

        ontoBuiltEnvMapping.stream().forEach(row -> {
            double cost = aisCost.get(row.getAISType());
            usageToCostMap.put(row.getEnvType(), cost);
        });

        return usageToCostMap;
    }
}
