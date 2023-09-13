package uk.ac.cam.cares.jps.agent.dashboard.json;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.agent.dashboard.IntegrationTestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.stack.StackClient;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class DashboardClientIntegrationTest {
    private static final String DASHBOARD_ACCOUNT_USER = "admin";
    private static final String DASHBOARD_ACCOUNT_PASS = "admin";

    @AfterEach
    void resetDashboard() {
        IntegrationTestUtils.deleteServiceAccounts(DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS);
    }

    @Test
    void testConstructor() {
        try (MockedConstruction<StackClient> mockClient = Mockito.mockConstruction(StackClient.class, (mock, context) -> {
            // Ensure all mocks return the test dashboard url and to allow the program to continue
            Mockito.when(mock.getDashboardUrl()).thenReturn(IntegrationTestUtils.TEST_DASHBOARD_URL);
        })) {
            StackClient mockStackClient = new StackClient();
            // Execute method
            assertNotNull(new DashboardClient(mockStackClient, DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS));
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
            DashboardClient client = new DashboardClient(mockStackClient, DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS);
            // Execute method
            client.initDashboard();
            // Verify if an account has been created
            List<Map<String, Object>> accountInfo = (List<Map<String, Object>>) IntegrationTestUtils.retrieveServiceAccounts(DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS);
            assertEquals(1, accountInfo.size());
            assertEquals(IntegrationTestUtils.SERVICE_ACCOUNT_NAME, accountInfo.get(0).get(IntegrationTestUtils.NAME_KEY));
            // Verify that no data sources has been created
            Map<String, Object> dataSources = (Map<String, Object>) IntegrationTestUtils.retrieveDataSources(DASHBOARD_ACCOUNT_USER, DASHBOARD_ACCOUNT_PASS);
            assertEquals(0, dataSources.size());
        }
    }
}