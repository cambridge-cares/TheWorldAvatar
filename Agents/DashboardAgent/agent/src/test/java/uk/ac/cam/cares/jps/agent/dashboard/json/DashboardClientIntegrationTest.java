package uk.ac.cam.cares.jps.agent.dashboard.json;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.grafana.GrafanaEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.google.gson.JsonArray;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.agent.dashboard.IntegrationTestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.stack.PostGisClientTest;
import uk.ac.cam.cares.jps.agent.dashboard.stack.SparqlClientTest;
import uk.ac.cam.cares.jps.agent.dashboard.stack.StackClient;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.sql.Connection;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class DashboardClientIntegrationTest {
    private static final String SAMPLE_SQL_DATABASE = "test";

    @BeforeAll
    static void setup() {
        // For testing dashboard administrative setup
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.TEST_POSTGIS_JDBC)) {
            String dbCreationQuery = "CREATE DATABASE " + SAMPLE_SQL_DATABASE;
            IntegrationTestUtils.updateDatabase(conn, dbCreationQuery);
        } catch (Exception e) {
            throw new RuntimeException("Unable to set up test databases: " + e.getMessage());
        }
        // For testing dashboard creation
        IntegrationTestUtils.createNamespace(IntegrationTestUtils.SPATIAL_ZONE_NAMESPACE);
        IntegrationTestUtils.createNamespace(IntegrationTestUtils.GENERAL_NAMESPACE);
        PostGisClientTest.genSampleDatabases();
    }

    @AfterEach
    void resetDashboard() {
        IntegrationTestUtils.deleteDataSources();
        IntegrationTestUtils.deleteServiceAccounts();
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_DELETE);
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_DELETE);
        IntegrationTestUtils.deletePasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH);
        IntegrationTestUtils.deletePasswordFile(IntegrationTestUtils.TEST_DASHBOARD_PASSWORD_PATH);
    }

    @AfterAll
    static void cleanUp() {
        PostGisClientTest.removeSampleDatabases();
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.TEST_POSTGIS_JDBC)) {
            String dbCreationQuery = "DROP DATABASE IF EXISTS " + SAMPLE_SQL_DATABASE;
            IntegrationTestUtils.updateDatabase(conn, dbCreationQuery);
        } catch (Exception e) {
            throw new RuntimeException("Unable to clean up databases: " + e.getMessage());
        }
    }

    @Test
    void testConstructor() {
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class, (mock, context) -> {
            // Ensure all mocks return the test dashboard url and to allow the program to continue
            Mockito.when(mock.getDashboardUrl()).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getServiceUrl());
            // Ensure all mocks return the dashboard credentials to function
            Mockito.when(mock.getDashboardUser()).thenReturn(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER);
            Mockito.when(mock.getDashboardPassword()).thenReturn(IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
        })) {
            StackClient mockStackClient = new StackClient();
            // Execute method
            assertNotNull(new DashboardClient(mockStackClient));
        }
    }

    @Test
    void testInitDashboard_NoData() {
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class, (mock, context) -> {
            // Ensure all mocks return the test dashboard url and to allow the program to continue
            Mockito.when(mock.getDashboardUrl()).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getServiceUrl());
            // Ensure all mocks return the dashboard credentials to function
            Mockito.when(mock.getDashboardUser()).thenReturn(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER);
            Mockito.when(mock.getDashboardPassword()).thenReturn(IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            // Mock that this returns an empty string array as this test is not for creating dashboards
            Mockito.when(mock.getAllOrganisations()).thenReturn(new String[]{});
        })) {
            StackClient mockStackClient = new StackClient();
            DashboardClient client = new DashboardClient(mockStackClient);
            // Execute method
            client.initDashboard();
            // Verify if an account has been created
            List<Map<String, Object>> accountInfo = (List<Map<String, Object>>) IntegrationTestUtils.retrieveServiceAccounts(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            assertEquals(1, accountInfo.size());
            assertEquals(1.0, accountInfo.get(0).get("tokens")); // Only one token should be generated
            assertEquals(IntegrationTestUtils.SERVICE_ACCOUNT_NAME, accountInfo.get(0).get(IntegrationTestUtils.NAME_KEY));
            // Verify that no data sources has been created
            JsonArray dataSources = IntegrationTestUtils.retrieveDataSources(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            assertTrue(dataSources.isEmpty());
        }
    }

    @Test
    void testInitDashboard_ExistingServiceAccountWithNoData() {
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class, (mock, context) -> {
            // Ensure all mocks return the test dashboard url and to allow the program to continue
            Mockito.when(mock.getDashboardUrl()).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getServiceUrl());
            // Ensure all mocks return the dashboard credentials to function
            Mockito.when(mock.getDashboardUser()).thenReturn(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER);
            Mockito.when(mock.getDashboardPassword()).thenReturn(IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            // Mock that this returns an empty string array as this test is not for creating dashboards
            Mockito.when(mock.getAllOrganisations()).thenReturn(new String[]{});
        })) {
            StackClient mockStackClient = new StackClient();
            DashboardClient client = new DashboardClient(mockStackClient);
            // Execute method twice to replicate the service account being created when the agent is running for the second time onwards
            client.initDashboard();
            client.initDashboard();
            // Verify the accounts are created as expected
            List<Map<String, Object>> accountInfo = (List<Map<String, Object>>) IntegrationTestUtils.retrieveServiceAccounts(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            assertEquals(1, accountInfo.size()); // Only one account should be created
            assertEquals(IntegrationTestUtils.SERVICE_ACCOUNT_NAME, accountInfo.get(0).get(IntegrationTestUtils.NAME_KEY));
            assertEquals(2.0, accountInfo.get(0).get("tokens")); // There should be two tokens
            // Verify that no data sources has been created
            JsonArray dataSources = IntegrationTestUtils.retrieveDataSources(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            assertTrue(dataSources.isEmpty());
        }
    }

    @Test
    void testInitDashboard_OneDatabaseConnection() {
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class, (mock, context) -> {
            // Ensure all mocks return the test dashboard url and to allow the program to continue
            Mockito.when(mock.getDashboardUrl()).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getServiceUrl());
            // Ensure all mocks return the dashboard credentials to function
            Mockito.when(mock.getDashboardUser()).thenReturn(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER);
            Mockito.when(mock.getDashboardPassword()).thenReturn(IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            // Mock that this returns an empty string array as this test is not for creating dashboards
            Mockito.when(mock.getAllOrganisations()).thenReturn(new String[]{});
            Mockito.when(mock.getDatabaseNames()).thenReturn(List.of(new String[]{SAMPLE_SQL_DATABASE}));
            Mockito.when(mock.getRdbDomain()).thenReturn(IntegrationTestUtils.TEST_POSTGIS_JDBC);
            Mockito.when(mock.getRdbUser()).thenReturn(IntegrationTestUtils.TEST_POSTGIS_USER);
            Mockito.when(mock.getRdbPassword()).thenReturn(IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        })) {
            StackClient mockStackClient = new StackClient();
            DashboardClient client = new DashboardClient(mockStackClient);
            // Execute method
            client.initDashboard();
            // Verify if an account has been created
            List<Map<String, Object>> accountInfo = (List<Map<String, Object>>) IntegrationTestUtils.retrieveServiceAccounts(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            assertEquals(1, accountInfo.size());
            assertEquals(1.0, accountInfo.get(0).get("tokens")); // Only one token should be generated
            assertEquals(IntegrationTestUtils.SERVICE_ACCOUNT_NAME, accountInfo.get(0).get(IntegrationTestUtils.NAME_KEY));
            // Verify that only one data source has been created
            JsonArray dataSources = IntegrationTestUtils.retrieveDataSources(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            assertEquals(1, dataSources.size());
        }
    }

    @Test
    void testInitDashboard_RepeatedDatabaseConnection() {
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class, (mock, context) -> {
            // Ensure all mocks return the test dashboard url and to allow the program to continue
            Mockito.when(mock.getDashboardUrl()).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getServiceUrl());
            // Ensure all mocks return the dashboard credentials to function
            Mockito.when(mock.getDashboardUser()).thenReturn(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER);
            Mockito.when(mock.getDashboardPassword()).thenReturn(IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            // Mock that this returns an empty string array as this test is not for creating dashboards
            Mockito.when(mock.getAllOrganisations()).thenReturn(new String[]{});
            Mockito.when(mock.getDatabaseNames()).thenReturn(List.of(new String[]{SAMPLE_SQL_DATABASE}));
            Mockito.when(mock.getRdbDomain()).thenReturn(IntegrationTestUtils.TEST_POSTGIS_JDBC);
            Mockito.when(mock.getRdbUser()).thenReturn(IntegrationTestUtils.TEST_POSTGIS_USER);
            Mockito.when(mock.getRdbPassword()).thenReturn(IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        })) {
            StackClient mockStackClient = new StackClient();
            DashboardClient client = new DashboardClient(mockStackClient);
            // Execute method twice and verify only one connection has been created
            client.initDashboard();
            client.initDashboard();
            // Verify if an account has been created
            List<Map<String, Object>> accountInfo = (List<Map<String, Object>>) IntegrationTestUtils.retrieveServiceAccounts(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            assertEquals(1, accountInfo.size());
            assertEquals(2.0, accountInfo.get(0).get("tokens")); // Two tokens should be generated
            assertEquals(IntegrationTestUtils.SERVICE_ACCOUNT_NAME, accountInfo.get(0).get(IntegrationTestUtils.NAME_KEY));
            // Verify that only one data source has been created
            JsonArray dataSources = IntegrationTestUtils.retrieveDataSources(IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
            assertEquals(1, dataSources.size());
        }
    }

    @Test
    void testInitDashboard_InvalidJsonmodel() {
        // Insert these triples into the blazegraph
        SparqlClientTest.insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        SparqlClientTest.insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, true);
        // Create password files
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_DASHBOARD_PASSWORD_PATH, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class, (mock, context) -> {
            // Ensure all mocks return the test config class for the method to continue
            Mockito.when(mock.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class)).thenReturn(IntegrationTestUtils.SPARQL_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(IntegrationTestUtils.POSTGIS_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("grafana", GrafanaEndpointConfig.class)).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG);
        }); MockedConstruction<GrafanaModel> mockModel = Mockito.mockConstruction(GrafanaModel.class, (mock, context) -> {
            // Ensure the json model returns an invalid JSON
            Mockito.when(mock.construct()).thenReturn("INVALID JSON");
        })) {
            StackClient stackClient = new StackClient();
            DashboardClient client = new DashboardClient(stackClient);
            // Execute method and ensure the right error is thrown
            IllegalArgumentException resultingError = assertThrows(IllegalArgumentException.class, () ->
                    client.initDashboard()
            );
            assertEquals("Bad request data! The json model is not compliant with Grafana standards!", resultingError.getMessage());
        }
    }

    @Test
    void testInitDashboard_Success() {
        // Insert these triples into the blazegraph
        SparqlClientTest.insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        SparqlClientTest.insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, true);
        // Create password files
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_DASHBOARD_PASSWORD_PATH, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class, (mock, context) -> {
            // Ensure all mocks return the test config class for the method to continue
            Mockito.when(mock.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class)).thenReturn(IntegrationTestUtils.SPARQL_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(IntegrationTestUtils.POSTGIS_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("grafana", GrafanaEndpointConfig.class)).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG);
        })) {
            StackClient stackClient = new StackClient();
            DashboardClient client = new DashboardClient(stackClient);
            Queue<String> dashboardUids = client.initDashboard();
            while (!dashboardUids.isEmpty()) {
                String uid = dashboardUids.poll();
                try {
                    Map<String, Object> jsonModel = (Map<String, Object>) IntegrationTestUtils.retrieveDashboard(uid, IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
                    verifyDashboardContents(jsonModel);
                } finally {
                    IntegrationTestUtils.deleteDashboard(uid);
                }
            }
        }
    }

    @Test
    void testInitDashboard_OverwriteSuccess() {
        // Insert these triples into the blazegraph
        SparqlClientTest.insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        SparqlClientTest.insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, true);
        // Create password files
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_DASHBOARD_PASSWORD_PATH, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class, (mock, context) -> {
            // Ensure all mocks return the test config class for the method to continue
            Mockito.when(mock.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class)).thenReturn(IntegrationTestUtils.SPARQL_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(IntegrationTestUtils.POSTGIS_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("grafana", GrafanaEndpointConfig.class)).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG);
        })) {
            StackClient stackClient = new StackClient();
            DashboardClient client = new DashboardClient(stackClient);
            // Execute method twice so that there is a pre-existing dashboard that should just be overwritten
            client.initDashboard();
            Queue<String> dashboardUids = client.initDashboard();
            while (!dashboardUids.isEmpty()) {
                String uid = dashboardUids.poll();
                try {
                    Map<String, Object> jsonModel = (Map<String, Object>) IntegrationTestUtils.retrieveDashboard(uid, IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
                    verifyDashboardContents(jsonModel);
                } finally {
                    IntegrationTestUtils.deleteDashboard(uid);
                }
            }
        }
    }

    private static void verifyDashboardContents(Map<String, Object> jsonModel) {
        // Verify the number of panels generated
        List<Map<String, Object>> rows = (List<Map<String, Object>>) jsonModel.get("panels");
        assertEquals(2, rows.size()); // there should only be two rows generated
        rows.forEach((row) -> {
            String title = row.get("title").toString();
            List<Object> panels = (List<Object>) row.get("panels");
            if (title.contains(SparqlClientTest.RELATIVE_HUMIDITY)) {
                assertEquals(3, panels.size()); // If the row is for relative humidity, three panels should be generated

            } else if (title.equals(StringHelper.addSpaceBetweenCapitalWords(SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_TYPE))) {
                assertEquals(2, panels.size()); // If the row is for smart sensor, two panels should be generated

            }
        });
        // Verify the number of templating variables generated
        Map<String, Object> templatingMap = (Map<String, Object>) jsonModel.get("templating");
        List<Object> templateVarList = (List<Object>) templatingMap.get("list");
        assertEquals(6, templateVarList.size()); // Six variables should be generated
    }
}