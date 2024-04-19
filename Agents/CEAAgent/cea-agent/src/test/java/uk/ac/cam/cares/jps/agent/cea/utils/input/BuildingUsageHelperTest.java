package uk.ac.cam.cares.jps.agent.cea.utils.input;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;

import uk.ac.cam.cares.jps.agent.cea.utils.FileReader;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Map;

public class BuildingUsageHelperTest {
    @Test
    public void testGetBuildingUsages() {

        String uriString = "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_test";
        Map<String, Double> result;

        String usage = "School";
        String ceaUsage = BuildingUsageHelper.toCEAConvention(usage.toUpperCase());
        usage = "test/" + usage;
        String usage1 = "Office";
        String ceaUsage1 = BuildingUsageHelper.toCEAConvention(usage1.toUpperCase());
        usage1 = "test/" + usage1;

        JSONObject usageJSON = new JSONObject()
                .put("BuildingUsage", usage)
                .put("UsageShare", 0.70);
        JSONObject usageJSON1 = new JSONObject()
                .put("BuildingUsage", usage1)
                .put("UsageShare", 0.30);

        JSONArray usageArray = new JSONArray().put(usageJSON).put(usageJSON1);

        // test when there is no usage information
        String content = "uri.ontology.ontocitygml=test/\nuri.ontology.om=test/\nuri.ontology.ontoubemmp=test/\nuri.ontology.rdf=test/\nuri.ontology.owl=test/\n" +
                "uri.ontology.bot=test/\nuri.ontology.ontobuiltenv=test/\nuri.ontology.ontobuiltstructure=test/\nuri.ontology.ontotimeseries=test/\nuri.ontology.ontoems=test/\n" +
                "uri.ontology.geo=test/\nuri.ontology.geofunction=test/\nuri.ontology.bldg=test/\nuri.ontology.grp=test/\nuri.ontology.gml=test/\n" +
                "uri.ontology.geoliteral=test/\nuri.ontology.geofunction=test/\nuri.opengis.epsg=test/\nuri.service.geo=test/";
        InputStream mockInputStream = new ByteArrayInputStream(content.getBytes());
        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedConstruction<RemoteStoreClient> remoteStoreClientMock = mockConstruction(RemoteStoreClient.class,
                    (mock, context) -> {
                        doReturn(new JSONArray()).when(mock).executeQuery(anyString());
                    })) {

                result = BuildingUsageHelper.getBuildingUsages(uriString, "");

                assertEquals(1, result.size());

                for (Map.Entry<String, Double> entry : result.entrySet()) {
                    assertTrue(entry.getKey().equals("MULTI_RES"));
                    assertEquals(1.00, entry.getValue());
                }
            }
        }

        // test when there is usage information
        try (MockedStatic<FileReader> fileReaderMock = mockStatic(FileReader.class)) {
            fileReaderMock.when(() -> FileReader.getStream(anyString())).thenReturn(mockInputStream);
            try (MockedConstruction<RemoteStoreClient> remoteStoreClientMock = mockConstruction(RemoteStoreClient.class,
                    (mock, context) -> {
                        doReturn(usageArray).when(mock).executeQuery(anyString());
                    })) {
                result = BuildingUsageHelper.getBuildingUsages(uriString, "");

                assertEquals(2, result.size());
                assertTrue(result.containsKey(ceaUsage));
                assertTrue(result.containsKey(ceaUsage1));
                assertEquals(0.70, result.get(ceaUsage));
                assertEquals(0.30, result.get(ceaUsage1));
            }
        }
    }
}
