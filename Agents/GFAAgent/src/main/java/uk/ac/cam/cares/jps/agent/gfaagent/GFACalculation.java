package uk.ac.cam.cares.jps.agent.gfaagent;

import java.sql.Connection;
import java.util.HashMap;
import java.util.Map;

import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class GFACalculation {
    private RemoteStoreClient ontopClient;

    public GFACalculation(String ontopUrl) {
        ontopClient = new RemoteStoreClient(ontopUrl);
    }

    /******************************************** */
    /* Calculate GFA and store in citydb.cityobject_genericattrib */
    /**********************************************/

    public void calculationGFA(Connection conn) {
        Map<String, Integer> iriToFloorMap = getIriToFloorMap();

        iriToFloorMap.entrySet().forEach(entry -> {
            String buildingIri = entry.getKey();
            int floor = entry.getValue();
            GFAPostGISClient.addGFAData(buildingIri, floor, conn);
        });
    }

    Map<String, Integer> getIriToFloorMap() {
        Map<String, Integer> iriToFloorMap = new HashMap<>();
        String query = """
                PREFIX env: <https://www.theworldavatar.com/kg/ontobuiltenv/>
                SELECT ?building ?floor
                WHERE {
                  ?building env:hasNumberOfFloors/env:hasValue ?floor .
                }
                """;

        JSONArray queryResult = ontopClient.executeQuery(query);
        for (int i = 0; i < queryResult.length(); i++) {
            String building = queryResult.getJSONObject(i).getString("building");
            int floor = queryResult.getJSONObject(i).getInt("floor");
            iriToFloorMap.put(building, floor);
        }

        return iriToFloorMap;
    }

}
