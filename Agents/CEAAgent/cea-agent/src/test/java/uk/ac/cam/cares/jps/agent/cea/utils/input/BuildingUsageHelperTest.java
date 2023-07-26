package uk.ac.cam.cares.jps.agent.cea.utils.input;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import org.json.JSONArray;
import org.json.JSONObject;
import java.util.Map;

public class BuildingUsageHelperTest {
    @Test
    public void testGetBuildingUsages() {
        OntologyURIHelper ontologyURIHelper = new OntologyURIHelper("CEAAgentConfig");

        BuildingUsageHelper usageHelper = new BuildingUsageHelper(ontologyURIHelper);

        String uriString = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_test";
        Map<String, Double> result;

        String usage = "School";
        String ceaUsage = usageHelper.toCEAConvention(usage.toUpperCase());
        usage = ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv) + usage;
        String usage1 = "Office";
        String ceaUsage1 = usageHelper.toCEAConvention(usage1.toUpperCase());
        usage1 = ontologyURIHelper.getOntologyUri(OntologyURIHelper.ontobuiltenv) + usage1;

        JSONObject usageJSON = new JSONObject()
                .put("BuildingUsage", usage)
                .put("UsageShare", 0.70);
        JSONObject usageJSON1 = new JSONObject()
                .put("BuildingUsage", usage1)
                .put("UsageShare", 0.30);

        JSONArray usageArray = new JSONArray().put(usageJSON).put(usageJSON1);

        try (MockedStatic<AccessAgentCaller> accessAgentCallerMock = mockStatic(AccessAgentCaller.class)) {
            // test when there is no building information in knowledge graph
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(new JSONArray());

            result = usageHelper.getBuildingUsages(uriString, "");

            assertEquals(1, result.size());

            for (Map.Entry<String, Double> entry : result.entrySet()) {
                assertTrue(entry.getKey().equals("MULTI_RES"));
                assertEquals(1.00, entry.getValue());
            }

            // test when there are building usage information in knowledge graph
            accessAgentCallerMock.when(() -> AccessAgentCaller.queryStore(anyString(), anyString()))
                    .thenReturn(usageArray);

            result = usageHelper.getBuildingUsages(uriString, "");

            assertEquals(2, result.size());
            assertTrue(result.containsKey(ceaUsage));
            assertTrue(result.containsKey(ceaUsage1));
            assertEquals(0.70, result.get(ceaUsage));
            assertEquals(0.30, result.get(ceaUsage1));
        }
    }
}
