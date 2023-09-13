package uk.ac.cam.cares.jps.agent.dashboard.json;

import com.google.gson.JsonArray;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.agent.dashboard.IntegrationTestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.stack.StackClient;

import java.sql.Connection;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class DashboardClientIntegrationTest {
    private static final String SAMPLE_SQL_DATABASE = "test";

    @BeforeAll
    static void setup() {
        try (Connection conn = IntegrationTestUtils.connectDatabase(IntegrationTestUtils.TEST_POSTGIS_JDBC)) {
            String dbCreationQuery = "CREATE DATABASE " + SAMPLE_SQL_DATABASE;
            IntegrationTestUtils.updateDatabase(conn, dbCreationQuery);
        } catch (Exception e) {
            throw new RuntimeException("Unable to set up test databases: " + e.getMessage());
        }
    }

    @AfterEach
    void resetDashboard() {
        IntegrationTestUtils.deleteDataSources();
        IntegrationTestUtils.deleteServiceAccounts();
    }

    @AfterAll
    static void cleanUp() {
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
            Mockito.when(mock.getDashboardUrl()).thenReturn(IntegrationTestUtils.TEST_DASHBOARD_URL);
        })) {
            StackClient mockStackClient = new StackClient();
            // Execute method
            assertNotNull(new DashboardClient(mockStackClient, IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS));
        }
    }

    @Test
    void testInitDashboardWithNoData() {
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class, (mock, context) -> {
            // Ensure all mocks return the test dashboard url and to allow the program to continue
            Mockito.when(mock.getDashboardUrl()).thenReturn(IntegrationTestUtils.TEST_DASHBOARD_URL);
            // Mock that this returns an empty string array as this test is not for creating dashboards
            Mockito.when(mock.getAllSpatialZones()).thenReturn(new String[]{});
        })) {
            StackClient mockStackClient = new StackClient();
            DashboardClient client = new DashboardClient(mockStackClient, IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
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
    void testInitDashboardForExistingServiceAccountWithNoData() {
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class, (mock, context) -> {
            // Ensure all mocks return the test dashboard url and to allow the program to continue
            Mockito.when(mock.getDashboardUrl()).thenReturn(IntegrationTestUtils.TEST_DASHBOARD_URL);
            // Mock that this returns an empty string array as this test is not for creating dashboards
            Mockito.when(mock.getAllSpatialZones()).thenReturn(new String[]{});
        })) {
            StackClient mockStackClient = new StackClient();
            DashboardClient client = new DashboardClient(mockStackClient, IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
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
    void testInitDashboardWithOneDatabaseConnection() {
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class, (mock, context) -> {
            // Ensure all mocks return the test dashboard url and to allow the program to continue
            Mockito.when(mock.getDashboardUrl()).thenReturn(IntegrationTestUtils.TEST_DASHBOARD_URL);
            // Mock that this returns an empty string array as this test is not for creating dashboards
            Mockito.when(mock.getAllSpatialZones()).thenReturn(new String[]{});
            Mockito.when(mock.getDatabaseNames()).thenReturn(List.of(new String[]{SAMPLE_SQL_DATABASE}));
            Mockito.when(mock.getPostGisCredentials()).thenReturn(new String[]{IntegrationTestUtils.TEST_POSTGIS_URL,
                    IntegrationTestUtils.TEST_POSTGIS_USER, IntegrationTestUtils.TEST_POSTGIS_PASSWORD});
        })) {
            StackClient mockStackClient = new StackClient();
            DashboardClient client = new DashboardClient(mockStackClient, IntegrationTestUtils.DASHBOARD_ACCOUNT_USER, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
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
}