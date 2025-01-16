package uk.ac.cam.cares.jps.agent.dashboard.stack;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.grafana.GrafanaEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.agent.dashboard.IntegrationTestUtils;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.Organisation;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.OrganisationTest;
import uk.ac.cam.cares.jps.agent.dashboard.datamodel.ThresholdTest;
import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class StackClientTest {
    @BeforeAll
    static void setup() {
        IntegrationTestUtils.createNamespace(IntegrationTestUtils.SPATIAL_ZONE_NAMESPACE);
        IntegrationTestUtils.createNamespace(IntegrationTestUtils.GENERAL_NAMESPACE);
        PostGisClientTest.genSampleDatabases();
    }

    @AfterEach
    void clearNamespace() {
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_DELETE);
        IntegrationTestUtils.updateEndpoint(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT, IntegrationTestUtils.SPARQL_DELETE);
        IntegrationTestUtils.deletePasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH);
        IntegrationTestUtils.deletePasswordFile(IntegrationTestUtils.TEST_DASHBOARD_PASSWORD_PATH);
    }

    @AfterAll
    static void cleanUp() {
        PostGisClientTest.removeSampleDatabases();
    }

    @Test
    void testGetDashboardConfigs_Url_User_Password() {
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_DASHBOARD_PASSWORD_PATH, IntegrationTestUtils.DASHBOARD_ACCOUNT_PASS);
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class, (mock, context) -> {
            // Ensure all mocks return the test config class for the method to continue
            Mockito.when(mock.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class)).thenReturn(IntegrationTestUtils.SPARQL_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(IntegrationTestUtils.POSTGIS_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("grafana", GrafanaEndpointConfig.class)).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG);
        })) {
            StackClient mockStackClient = new StackClient();
            assertEquals(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getServiceUrl(), mockStackClient.getDashboardUrl());
            assertEquals(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getUsername(), mockStackClient.getDashboardUser());
            assertEquals(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG.getPassword(), mockStackClient.getDashboardPassword());
        }
    }

    @Test
    void testGetAllOrganisations_NoData() {
        // Create a postgis password file
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class, (mock, context) -> {
            // Ensure all mocks return the test config class for the method to continue
            Mockito.when(mock.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class)).thenReturn(IntegrationTestUtils.SPARQL_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(IntegrationTestUtils.POSTGIS_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("grafana", GrafanaEndpointConfig.class)).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG);
        })) {
            StackClient mockStackClient = new StackClient();
            List<Organisation> organisations = mockStackClient.getAllOrganisations();
            // Verify there is no organisation if no relevant triples are found
            assertEquals(0, organisations.size());
        }
    }

    @Test
    void testGetAllOrganisations_WithData() {
        // Insert these triples into the blazegraph
        SparqlClientTest.insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        SparqlClientTest.insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, true);
        // Create a postgis password file
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class, (mock, context) -> {
            // Ensure all mocks return the test config class for the method to continue
            Mockito.when(mock.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class)).thenReturn(IntegrationTestUtils.SPARQL_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(IntegrationTestUtils.POSTGIS_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("grafana", GrafanaEndpointConfig.class)).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG);
        })) {
            StackClient mockStackClient = new StackClient();
            List<Organisation> organisations = mockStackClient.getAllOrganisations();
            // Verify there is an organisation if there are relevant triples
            assertEquals(1, organisations.size());
        }
    }

    @Test
    void testGetDatabaseNames() {
        // Insert these triples into the blazegraph
        SparqlClientTest.insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        SparqlClientTest.insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, true);
        // Create a postgis password file
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class, (mock, context) -> {
            // Ensure all mocks return the test config class for the method to continue
            Mockito.when(mock.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class)).thenReturn(IntegrationTestUtils.SPARQL_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(IntegrationTestUtils.POSTGIS_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("grafana", GrafanaEndpointConfig.class)).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG);
        })) {
            StackClient mockStackClient = new StackClient();
            List<String> databases = mockStackClient.getDatabaseNames();
            // Verify that there are two databases
            assertEquals(2, databases.size());
            databases.forEach((database) ->
                    assertTrue(database.equals(PostGisClientTest.TEMPERATURE_SQL_DATABASE) || database.equals(PostGisClientTest.ALL_SQL_DATABASE))
            );
        }
    }

    @Test
    void testGetPostGisCredentials_RdbDomain_User_Pass() {
        // Insert these triples into the blazegraph
        SparqlClientTest.insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        SparqlClientTest.insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, true);
        // Create a postgis password file
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class, (mock, context) -> {
            // Ensure all mocks return the test config class for the method to continue
            Mockito.when(mock.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class)).thenReturn(IntegrationTestUtils.SPARQL_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(IntegrationTestUtils.POSTGIS_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("grafana", GrafanaEndpointConfig.class)).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG);
        })) {
            StackClient mockStackClient = new StackClient();
            // Verify that credentials are as follows
            assertEquals(IntegrationTestUtils.DOCKER_HOST_NAME + ":5431", mockStackClient.getRdbDomain());
            assertEquals(IntegrationTestUtils.TEST_POSTGIS_USER, mockStackClient.getRdbUser());
            assertEquals(IntegrationTestUtils.TEST_POSTGIS_PASSWORD, mockStackClient.getRdbPassword());

        }
    }

    @Test
    void testGetAllTimeSeries() {
        // Insert these triples into the blazegraph
        SparqlClientTest.insertFacilityTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        SparqlClientTest.insertSystemTriples(IntegrationTestUtils.SPATIAL_ZONE_SPARQL_ENDPOINT);
        SparqlClientTest.insertAssetTriples(IntegrationTestUtils.GENERAL_SPARQL_ENDPOINT, false);
        // Create a postgis password file
        IntegrationTestUtils.createPasswordFile(IntegrationTestUtils.TEST_POSTGIS_PASSWORD_PATH, IntegrationTestUtils.TEST_POSTGIS_PASSWORD);
        try (MockedConstruction<ContainerClient> mockClient = Mockito.mockConstruction(ContainerClient.class, (mock, context) -> {
            // Ensure all mocks return the test config class for the method to continue
            Mockito.when(mock.readEndpointConfig("blazegraph", BlazegraphEndpointConfig.class)).thenReturn(IntegrationTestUtils.SPARQL_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("postgis", PostGISEndpointConfig.class)).thenReturn(IntegrationTestUtils.POSTGIS_ENDPOINT_CONFIG);
            Mockito.when(mock.readEndpointConfig("grafana", GrafanaEndpointConfig.class)).thenReturn(IntegrationTestUtils.DASHBOARD_ENDPOINT_CONFIG);
        })) {
            StackClient mockStackClient = new StackClient();
            List<Organisation> organisations = mockStackClient.getAllOrganisations();
            assertEquals(1, organisations.size()); // verifies if there is only one organisation
            for (Organisation organisation : organisations) {
                OrganisationTest.verifyOrganisationModel(organisation, OrganisationTest.genExpectedOrgOutputs(false,
                        new String[]{SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_NAME, SparqlClientTest.SAMPLE_LAB_SMART_SENSOR_TYPE, SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT,
                                SparqlClientTest.SMART_SENSOR_ONE_TEMPERATURE_MEASURE, SparqlClientTest.SMART_SENSOR_ONE_TEMPERATURE_TIME_SERIES_IRI,
                                PostGisClientTest.SAMPLE_SMART_SENSOR_ONE_TEMPERATURE_COLUMN, PostGisClientTest.SAMPLE_TEMPERATURE_TABLE, PostGisClientTest.TEMPERATURE_SQL_DATABASE
                        },
                        new String[]{SparqlClientTest.SAMPLE_OFFICE_SYSTEM_NAME, StringHelper.SYSTEM_KEY, SparqlClientTest.ELECTRICITY_CONSUMPTION, SparqlClientTest.ELECTRICITY_CONSUMPTION_UNIT,
                                SparqlClientTest.SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SparqlClientTest.SAMPLE_OFFICE_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI,
                                PostGisClientTest.SAMPLE_OFFICE_SYSTEM_ELEC_CONSUMPTION_COLUMN, PostGisClientTest.SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE, PostGisClientTest.ALL_SQL_DATABASE
                        },
                        new String[]{SparqlClientTest.SAMPLE_OFFICE_SUB_SYSTEM_NAME, StringHelper.SYSTEM_KEY, SparqlClientTest.ELECTRICITY_CONSUMPTION, SparqlClientTest.ELECTRICITY_CONSUMPTION_UNIT,
                                SparqlClientTest.SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_MEASURE, SparqlClientTest.SAMPLE_OFFICE_SUB_SYSTEM_ELECTRICITY_CONSUMPTION_TIME_SERIES_IRI,
                                PostGisClientTest.SAMPLE_OFFICE_SUB_SYSTEM_ELEC_CONSUMPTION_COLUMN, PostGisClientTest.SAMPLE_SYSTEM_ELEC_CONSUMPTION_TABLE, PostGisClientTest.ALL_SQL_DATABASE
                        },
                        new String[]{SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT,
                                SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_MEASURE, SparqlClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_TIME_SERIES_IRI,
                                PostGisClientTest.SAMPLE_OFFICE_DIRECTOR_ROOM_TEMPERATURE_COLUMN, PostGisClientTest.SAMPLE_TEMPERATURE_TABLE, PostGisClientTest.TEMPERATURE_SQL_DATABASE
                        },
                        new String[]{SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.TEMPERATURE, SparqlClientTest.TEMPERATURE_UNIT,
                                SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_MEASURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_TIME_SERIES_IRI,
                                PostGisClientTest.SAMPLE_OFFICE_STAFF_ROOM_TEMPERATURE_COLUMN, PostGisClientTest.SAMPLE_TEMPERATURE_TABLE, PostGisClientTest.TEMPERATURE_SQL_DATABASE
                        },
                        new String[]{SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_NAME, StringHelper.ROOM_KEY, SparqlClientTest.RELATIVE_HUMIDITY, null,
                                SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_MEASURE, SparqlClientTest.SAMPLE_OFFICE_STAFF_ROOM_REL_HUMIDITY_TIME_SERIES_IRI,
                                PostGisClientTest.SAMPLE_OFFICE_STAFF_ROOM_HUMIDITY_COLUMN, PostGisClientTest.SAMPLE_ROOM_HUMIDITY_TABLE, PostGisClientTest.ALL_SQL_DATABASE
                        }
                ), ThresholdTest.genExpectedThresholds(
                        new String[]{SparqlClientTest.RELATIVE_HUMIDITY, String.valueOf(SparqlClientTest.RELATIVE_HUMIDITY_MIN_THRESHOLD), String.valueOf(SparqlClientTest.RELATIVE_HUMIDITY_MAX_THRESHOLD)},
                        new String[]{SparqlClientTest.TEMPERATURE, String.valueOf(SparqlClientTest.TEMPERATURE_MIN_THRESHOLD), String.valueOf(SparqlClientTest.TEMPERATURE_MAX_THRESHOLD)}
                ));
            }
        }
    }
}
