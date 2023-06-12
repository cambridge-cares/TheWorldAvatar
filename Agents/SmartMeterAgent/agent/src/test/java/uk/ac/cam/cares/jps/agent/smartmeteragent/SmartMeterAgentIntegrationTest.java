package uk.ac.cam.cares.jps.agent.smartmeteragent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Connection;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.io.TempDir;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.StoreRouter;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

@Testcontainers
public class SmartMeterAgentIntegrationTest {
    
    @TempDir
    File tempFolder;

    SmartMeterAgent smAgent = new SmartMeterAgent();
    
    //User defined variables
    //set the desired access agent version number here
    static final String ACCESS_AGENT_VERSION = "1.7.0";
    
    /////////////////////////////////////////////////////////
    static final String ACCESS_AGENT_IMAGE ="ghcr.io/cambridge-cares/access-agent:" + ACCESS_AGENT_VERSION; 
    static final String BLAZEGRAPH_IMAGE = "docker.cmclinnovations.com/blazegraph_for_tests:1.0.0";
    static final String POSTGRES_IMAGE = "postgres:13.3"; 
    static final int BLAZEGRAPH_INTERNAL_PORT = 9999;
    static final String TEST_NAMESPACE = "kb";
    
    //Put all containers on the same network
    static final Network NETWORK = Network.newNetwork();
    static final String STORE_ROUTER_CONTAINER_ALIAS = "store-router-container";
    static final String TARGET_STORE_CONTAINER_ALIAS = "target-store-container";
    
    static final String STORE_ROUTER_ENDPOINT = "http://" + STORE_ROUTER_CONTAINER_ALIAS  
            + ":" + Integer.toString(BLAZEGRAPH_INTERNAL_PORT) + "/blazegraph/namespace/kb/sparql";
    
    //Create only one store router triple store and Access Agent for the entire test
    @Container
    static final GenericContainer<?> STORE_ROUTER_CONTAINER = new GenericContainer<>(DockerImageName.parse(BLAZEGRAPH_IMAGE))
                                                .withExposedPorts(BLAZEGRAPH_INTERNAL_PORT)
                                                .withNetwork(NETWORK)
                                                .withNetworkAliases(STORE_ROUTER_CONTAINER_ALIAS);
    @Container
    static final GenericContainer<?> ACCESS_AGENT_CONTAINER = new GenericContainer<>(DockerImageName.parse(ACCESS_AGENT_IMAGE))
                                                .withExposedPorts(8080)
                                                .withEnv(StoreRouter.STOREROUTER_ENDPOINT_NAME,STORE_ROUTER_ENDPOINT)
                                                .withNetwork(NETWORK)
                                                .dependsOn(STORE_ROUTER_CONTAINER);

    //Create a new target store for each test
    @Container
    GenericContainer<?> targetStoreContainer = new GenericContainer<>(DockerImageName.parse(BLAZEGRAPH_IMAGE))
                                                .withExposedPorts(BLAZEGRAPH_INTERNAL_PORT)
                                                .withNetwork(NETWORK)
                                                .withNetworkAliases(TARGET_STORE_CONTAINER_ALIAS);

    @Container
    PostgreSQLContainer<?> postgreSQLContainer = new PostgreSQLContainer<>(POSTGRES_IMAGE);
    
    RemoteStoreClient targetStoreClient;
    String targetStoreLabel;
    String targetResourceID;

    static TimeSeriesClient<OffsetDateTime> tsClient;
    RemoteRDBStoreClient rdbStoreClient;

    
    /////////////////////////////////////////////////////////
	
	@BeforeAll
	static void setupAll() {
		try {
			STORE_ROUTER_CONTAINER.start();
			ACCESS_AGENT_CONTAINER.start();
		} catch (Exception e) {
			throw new JPSRuntimeException("SmartMeterAgentIntegrationTest: Docker container startup failed. Please try running tests again.");
		}
	}
	
	@BeforeEach
	void setupEach() throws IOException {
		try {
			targetStoreContainer.start();
            postgreSQLContainer.start();
		} catch (Exception e) {
			throw new JPSRuntimeException("SmartMeterAgentIntegrationTest: Docker container startup failed. Please try running tests again.");
		}
        //Upload routing information
		String targetStoreEndpointInternal = "http://" + TARGET_STORE_CONTAINER_ALIAS 
                                            + ":" + BLAZEGRAPH_INTERNAL_PORT
                                            + "/blazegraph/namespace/" + TEST_NAMESPACE + "/sparql";
        String targetStoreEndpointExternal = "http://" + targetStoreContainer.getHost() 
                                            + ":" + targetStoreContainer.getFirstMappedPort()
                                            + "/blazegraph/namespace/" + TEST_NAMESPACE + "/sparql";

        String uploadUrl = "http://" + ACCESS_AGENT_CONTAINER.getHost() 
                            + ":" + ACCESS_AGENT_CONTAINER.getFirstMappedPort()
                            + "/access-agent/upload";
        targetStoreLabel = "kb" + targetStoreContainer.getFirstMappedPort();
        String uploaded = IntegrationTestHelper.uploadRoutingData(targetStoreLabel, targetStoreEndpointInternal, uploadUrl);
		assertTrue("SmartMeterAgentIntegrationTest: Routing data upload failed.", uploaded.equals("1 endpoint(s) uploaded."));
				
		//Connect to target store and set targetResourceID
		targetStoreClient = new RemoteStoreClient(targetStoreEndpointExternal,targetStoreEndpointExternal);
		targetResourceID = "http://" + ACCESS_AGENT_CONTAINER.getHost() 
		                + ":" + ACCESS_AGENT_CONTAINER.getFirstMappedPort()
		                + "/" + targetStoreLabel;

        // Create and set time-series client
        // Set endpoint to the triple store. The host and port are read from the container
        String endpoint = "http://" + targetStoreContainer.getHost() + ":" 
                        + targetStoreContainer.getFirstMappedPort()
                        + "/blazegraph/namespace/kb/sparql";

        // Set up a kb client that points to the location of the triple store
        RemoteStoreClient kbClient = new RemoteStoreClient();
        kbClient.setUpdateEndpoint(endpoint);
        kbClient.setQueryEndpoint(endpoint);

        // Initialise TimeSeriesClient client with pre-configured kb client
        tsClient = new TimeSeriesClient<>(kbClient, OffsetDateTime.class, null, "postgres", "cares1010");

        // Configure database access
        String rdbUrl = postgreSQLContainer.getJdbcUrl();
        String rdbUsername = postgreSQLContainer.getUsername();
        String rdbPassword = postgreSQLContainer.getPassword();
        tsClient.setRDBClient(rdbUrl, rdbUsername, rdbPassword);
        rdbStoreClient = new RemoteRDBStoreClient(rdbUrl, rdbUsername, rdbPassword);

        tsClient = IntegrationTestHelper.prepareTimeSeries(tsClient);

        smAgent = new SmartMeterAgent() {
            @Override
            public void loadConfigs(String filepath) throws IOException {
                if (filepath.contains("db")) {
                    this.dbUrl = "jdbc:sqlite:src/test/resources/test.db";
                    this.dbUsername = rdbUsername;
                    this.dbPassword = rdbPassword;
                    return;
                }
                this.dbUrl = rdbUrl;
                this.dbUsername = rdbUsername;
                this.dbPassword = rdbPassword;
                this.kbClient.setQueryEndpoint(endpoint);
                this.kbClient.setUpdateEndpoint(endpoint);
            }
        };

        try {
            smAgent.loadConfigs("");
        } catch (IOException e) {
            throw new JPSRuntimeException("RDB client configuration failed.");
        }
    }

	@AfterEach
	public void stopEach() {
		if (targetStoreContainer.isRunning()) {
			targetStoreContainer.stop();
		}
        if (postgreSQLContainer.isRunning()) {
            postgreSQLContainer.stop();
        }
	}
	
	@AfterAll
	public static void stopAll(){
		if (STORE_ROUTER_CONTAINER.isRunning()) {
			STORE_ROUTER_CONTAINER.stop();
		}
		if (ACCESS_AGENT_CONTAINER.isRunning()) {
			ACCESS_AGENT_CONTAINER.stop();
		}
	}
    
    /////////////////////////////////////////////////////////
	// Integration tests
    /////////////////////////////////////////////////////////
    @Test
    public void testGetDataIris() {
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        String[] mapping1 = {"1", "device1"};
        String[] mapping2 = {"2"};
        List<String[]> mappings = new ArrayList<>();
        mappings.add(mapping1);
        mappings.add(mapping2);

        JSONArray actualResult = smAgent.getDataIris(targetResourceID, mappings);
        if (actualResult.getJSONObject(0).getString("BusNumbervalue").equals("1")) {
            assertEquals("http://example.com/device1pd", actualResult.getJSONObject(0).optString("PdIri"));
            assertEquals("http://example.com/device1qd", actualResult.getJSONObject(0).optString("QdIri"));
            assertEquals("device1", actualResult.getJSONObject(0).getString("device"));
        } else if (actualResult.getJSONObject(0).getString("BusNumbervalue").equals("2")) {
            assertEquals("http://example.com/device2pd", actualResult.getJSONObject(0).optString("PdIri"));
            assertEquals("http://example.com/device2qd", actualResult.getJSONObject(0).optString("QdIri"));
            assertEquals("http://example.com/device2current", actualResult.getJSONObject(0).optString("currentIri"));
            assertEquals("http://example.com/device2voltage", actualResult.getJSONObject(0).optString("voltageIri"));
            assertEquals("0", actualResult.getJSONObject(0).getString("device"));
        } else {
            throw new AssertionError("Incorrect BusNumbervalue.");
        }
        if (actualResult.getJSONObject(1).getString("BusNumbervalue").equals("1")) {
            assertEquals("http://example.com/device1pd", actualResult.getJSONObject(1).optString("PdIri"));
            assertEquals("http://example.com/device1qd", actualResult.getJSONObject(1).optString("QdIri"));
            assertEquals("device1", actualResult.getJSONObject(1).getString("device"));
        } else if (actualResult.getJSONObject(1).getString("BusNumbervalue").equals("2")) {
            assertEquals("http://example.com/device2pd", actualResult.getJSONObject(1).optString("PdIri"));
            assertEquals("http://example.com/device2qd", actualResult.getJSONObject(1).optString("QdIri"));
            assertEquals("http://example.com/device2current", actualResult.getJSONObject(1).optString("currentIri"));
            assertEquals("http://example.com/device2voltage", actualResult.getJSONObject(1).optString("voltageIri"));
            assertEquals("0", actualResult.getJSONObject(1).getString("device"));
        } else {
            throw new AssertionError("Incorrect BusNumbervalue.");
        }
    }

    @Test
    public void testUploadSmartMeterData() {
        JSONObject device1IRIs = new JSONObject()
                                .put("device", "device1")
                                .put("BusNumbervalue", "1")
                                .put("PdIri", "http://example.com/device1pd")
                                .put("QdIri", "http://example.com/device1qd");
        JSONObject device2IRIs = new JSONObject()
                                .put("device", "0")
                                .put("BusNumbervalue", "2")
                                .put("PdIri", "http://example.com/device2pd")
                                .put("QdIri", "http://example.com/device2qd")
                                .put("currentIri", "http://example.com/device2current")
                                .put("voltageIri", "http://example.com/device2voltage")
                                .put("frequencyIri", "http://example.com/device2frequency");
        JSONArray dataIRIArray = new JSONArray().put(device1IRIs).put(device2IRIs);
        JSONObject device1Values = new JSONObject()
                                .put("time", "2022-11-09T21:09:00+00:00")
                                .put("device", "device1")
                                .put("pd", "-1") // negative sign will be changed by SMAgent
                                .put("current", "2")
                                .put("voltage", "3")
                                .put("frequency", "4");
        JSONArray dataValueArray = new JSONArray().put(device1Values);
        smAgent.uploadSmartMeterData(dataValueArray, dataIRIArray);
        OffsetDateTime time = OffsetDateTime.parse("2022-11-09T21:09:00+00:00");
        List<String> dataIris = new ArrayList<String>();
        dataIris.add("http://example.com/device1pd");
        dataIris.add("http://example.com/device1qd");
        dataIris.add("http://example.com/device2pd");
        dataIris.add("http://example.com/device2qd");
        dataIris.add("http://example.com/device2current");
        dataIris.add("http://example.com/device2voltage");
        dataIris.add("http://example.com/device2frequency");
        // Check that values are uploaded correctly
        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeries<OffsetDateTime> pdTimeseries = tsClient.getTimeSeriesWithinBounds(dataIris, time, time, conn);
            assertEquals("1.0", pdTimeseries.getValuesAsString(dataIris.get(0)).get(0)); // device1 Pd
            assertEquals("0", pdTimeseries.getValuesAsString(dataIris.get(1)).get(0)); // device1 Qd
            assertEquals("0", pdTimeseries.getValuesAsString(dataIris.get(2)).get(0)); // device2 Pd
            assertEquals("0", pdTimeseries.getValuesAsString(dataIris.get(3)).get(0)); // device2 Qd
            assertEquals("0", pdTimeseries.getValuesAsString(dataIris.get(4)).get(0)); // device2 current
            assertEquals("0", pdTimeseries.getValuesAsString(dataIris.get(5)).get(0)); // device2 voltage
            assertEquals("0", pdTimeseries.getValuesAsString(dataIris.get(6)).get(0)); // device2 frequency
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
    }

    @Test
    public void testQueryLatestDataFromDb() {
        // the database file contains invalid readings for 2022-11-09 21:40, 21:41, and 21:46
        // and                                                                                                                                                                                                     valid readings for 2022-11-09 21: 42 and 21:45
        String currentTime1 = "2022-11-09 21:46";
        String[] mapping1 = {"1", "device1"};
        String[] mapping2 = {"2", "device2"};
        List<String[]> mappings = new ArrayList<>();
        mappings.add(mapping1);
        mappings.add(mapping2);

        // Check that latest valid readings are read correctly
        JSONArray actualResult = smAgent.queryLatestDataFromDb(currentTime1, mappings);
        assertEquals("2022-11-09T21:45:00+00:00", actualResult.getJSONObject(0).optString("time"));
        assertEquals("2022-11-09T21:45:00+00:00", actualResult.getJSONObject(1).optString("time"));
        assertEquals("device1", actualResult.getJSONObject(0).optString("device"));
        assertEquals("device2", actualResult.getJSONObject(1).optString("device"));

        assertEquals(-0.0037503, actualResult.getJSONObject(0).getDouble("pd"), 0.0001);
        assertEquals(0.126606, actualResult.getJSONObject(0).getDouble("current"), 0.0001);
        assertEquals(222.063781, actualResult.getJSONObject(0).getDouble("voltage"), 0.0001);
        assertEquals(49.999741, actualResult.getJSONObject(0).getDouble("frequency"), 0.0001);
        assertEquals(-0.514298, actualResult.getJSONObject(1).getDouble("pd"), 0.0001);
        assertEquals(2.37722166, actualResult.getJSONObject(1).getDouble("current"), 0.0001);
        assertEquals(216.6744436, actualResult.getJSONObject(1).getDouble("voltage"), 0.0001);
        assertEquals(49.999718, actualResult.getJSONObject(1).getDouble("frequency"), 0.0001);

        // Check that correct exception is thrown when no valid readings can be found within 10 attempts
        String currentTime2 = "2022-11-09 21:41";
        Exception exception = assertThrows(JPSRuntimeException.class, 
                                            ()->{smAgent.queryLatestDataFromDb(currentTime2, mappings);});
        String expectedMessage = "Please check that all devices in the mapping file are switched on.";
        String actualMessage = exception.getMessage();
        assertTrue(actualMessage.contains(expectedMessage));
    }

    @Test
    public void testUploadHistoricalDataFromDb() {
        // the database file contains invalid readings for 2022-11-09 21:40, 21:41, and 21:46
        // and valid readings for 2022-11-09 21: 42 and 21:45
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        String beforeTime = "2022-11-09 21:43";
        String afterTime = "2001-01-01 00:00";
        String[] mapping1 = {"1", "device1"};
        String[] mapping2 = {"2", "device2"};
        List<String[]> mappings = new ArrayList<>();
        mappings.add(mapping1);
        mappings.add(mapping2);
        int numOfReadings = smAgent.uploadHistoricalDataFromDb(beforeTime, afterTime, mappings, targetResourceID);
        assertEquals(1, numOfReadings);

        // Check that values are uploaded correctly
        List<String> dataIris = new ArrayList<String>();
        dataIris.add("http://example.com/device1pd");
        dataIris.add("http://example.com/device1qd");
        // device 2 has current and voltage instantiated (but not frequency)
        dataIris.add("http://example.com/device2pd");
        dataIris.add("http://example.com/device2qd");
        dataIris.add("http://example.com/device2current");
        dataIris.add("http://example.com/device2voltage");
        OffsetDateTime time = OffsetDateTime.parse("2022-11-09T21:42:00+00:00");
        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeries<OffsetDateTime> timeseries = tsClient.getTimeSeriesWithinBounds(dataIris, time, time, conn);
            assertEquals(-0.013268, Double.parseDouble(timeseries.getValuesAsString(dataIris.get(0)).get(0)), 0.0001); // device1 Pd
            assertEquals(0, Double.parseDouble(timeseries.getValuesAsString(dataIris.get(1)).get(0)), 0.0001); // device1 Qd
            assertEquals(0.85862064, Double.parseDouble(timeseries.getValuesAsString(dataIris.get(2)).get(0)), 0.0001); // device2 Pd
            assertEquals(0, Double.parseDouble(timeseries.getValuesAsString(dataIris.get(3)).get(0)), 0.0001); // device2 Qd
            assertEquals(4.574993, Double.parseDouble(timeseries.getValuesAsString(dataIris.get(4)).get(0)), 0.0001); // device2 current
            assertEquals(216.72646, Double.parseDouble(timeseries.getValuesAsString(dataIris.get(5)).get(0)), 0.0001); // device2 voltage
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
    }

    @Test
    public void testReadDataFromCsvFile() throws IOException {
        File readingFile = new File(tempFolder, "readings.csv");
        FileWriter writer = new FileWriter(readingFile);
        writer.write(IntegrationTestHelper.smartMeterReading1);
        writer.write(IntegrationTestHelper.smartMeterReading2);
        writer.write(IntegrationTestHelper.smartMeterReading3);
        writer.write(IntegrationTestHelper.smartMeterReading4);
        writer.write(IntegrationTestHelper.smartMeterReading5);
        writer.write(IntegrationTestHelper.smartMeterReading6);
        writer.close();
        String filename = tempFolder.toPath().toString() + "/readings.csv";
        List<String> devices = new ArrayList<>();
        devices.add("device1");
        devices.add("device2");
        OffsetDateTime afterTime = OffsetDateTime.parse("2022-10-26T18:24:00+00:00");
        OffsetDateTime beforeTime = OffsetDateTime.parse("2022-10-26T18:24:58+00:00");
        JSONArray dataIriArray = new JSONArray();
        JSONObject dataIri1 = new JSONObject()
                            .put("device", "device1")
                            .put("BusNumbervalue", "1")
                            .put("PdIri", "http://example.com/device1pd")
                            .put("QdIri", "http://example.com/device1qd");
        JSONObject dataIri2 = new JSONObject()
                            .put("device", "device2")
                            .put("BusNumbervalue", "2")
                            .put("PdIri", "http://example.com/device2pd")
                            .put("QdIri", "http://example.com/device2qd")
                            .put("currentIri", "http://example.com/device2current")
                            .put("voltageIri", "http://example.com/device2voltage")
                            .put("frequencyIri", "http://example.com/device2frequency");
        dataIriArray.put(dataIri1);
        dataIriArray.put(dataIri2);
        assertEquals(1, smAgent.readDataFromCsvFile(filename, devices, beforeTime, afterTime, dataIriArray));
        List<String> dataIris = new ArrayList<String>();
        dataIris.add("http://example.com/device1pd");
        dataIris.add("http://example.com/device1qd");
        dataIris.add("http://example.com/device2pd");
        dataIris.add("http://example.com/device2qd");
        dataIris.add("http://example.com/device2current");
        dataIris.add("http://example.com/device2voltage");
        dataIris.add("http://example.com/device2frequency");
        // Check that values are uploaded correctly
        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeries<OffsetDateTime> timeseriesData = tsClient.getTimeSeriesWithinBounds(dataIris, afterTime, beforeTime, conn);
            assertEquals(-0.3177623, Double.parseDouble(timeseriesData.getValuesAsString(dataIris.get(0)).get(0)), 0.0001); // device1 Pd
            assertEquals(0, Double.parseDouble(timeseriesData.getValuesAsString(dataIris.get(1)).get(0)), 0.0001); // device1 Qd
            assertEquals(-0.1177623, Double.parseDouble(timeseriesData.getValuesAsString(dataIris.get(2)).get(0)), 0.0001); // device2 Pd
            assertEquals(0, Double.parseDouble(timeseriesData.getValuesAsString(dataIris.get(3)).get(0)), 0.0001); // device2 Qd
            assertEquals(1.426896, Double.parseDouble(timeseriesData.getValuesAsString(dataIris.get(4)).get(0)), 0.0001); // device2 current
            assertEquals(224.016235, Double.parseDouble(timeseriesData.getValuesAsString(dataIris.get(5)).get(0)), 0.0001); // device2 voltage
            assertEquals(49.999737, Double.parseDouble(timeseriesData.getValuesAsString(dataIris.get(6)).get(0)), 0.0001); // device2 frequency
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
    }

    @Test
    public void testProcessRequestParametersHistoricalDb() throws IOException {
        // the database file contains invalid readings for 2022-11-09 21:40, 21:41, and 21:46
        // and valid readings for 2022-11-09 21: 42 and 21:45
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        SmartMeterAgentLauncher agentLauncher = new SmartMeterAgentLauncher() {
            @Override
            public SmartMeterAgent getSmartMeterAgent() {
                return smAgent;
            }
            @Override
            public List<String[]> getDataMappings(SmartMeterAgent agent) {
                return agent.getDataMappings(tempFolder.toPath().toString());
            }
        };
        File mappingFile = new File(tempFolder, "mappings.csv");
        FileWriter writer = new FileWriter(mappingFile);
        writer.write("1,device1\n");
        writer.write("2,device2");
        writer.close();

        JSONObject request1 = new JSONObject()
                            .put("dataSource", "database")
                            .put("dataRequired", "historical")
                            .put("microgrid", targetResourceID);
        JSONObject expectedResult = new JSONObject().put("uploadStatus", "2 readings uploaded successfully.");
        JSONObject actualResult = agentLauncher.processRequestParameters(request1);
        assertTrue(actualResult.similar(expectedResult));

        JSONObject request2 = new JSONObject()
                            .put("dataSource", "database")
                            .put("dataRequired", "historical")
                            .put("microgrid", targetResourceID)
                            .put("dataBefore", "2022-11-09 21:41");
        expectedResult = new JSONObject().put("uploadStatus", "No valid reading found.");
        actualResult = agentLauncher.processRequestParameters(request2);
        assertTrue(actualResult.similar(expectedResult));
    }

    @Test
    public void testProcessRequestParametersCsv() throws IOException {
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        SmartMeterAgentLauncher agentLauncher = new SmartMeterAgentLauncher() {
            @Override
            public SmartMeterAgent getSmartMeterAgent() {
                return smAgent;
            }
            @Override
            public List<String[]> getDataMappings(SmartMeterAgent agent) {
                return agent.getDataMappings(tempFolder.toPath().toString());
            }
            @Override
            public String getCsvFilename() {
                return tempFolder.toPath().toString() + "/readings.csv";
            }
        };
        File mappingFile = new File(tempFolder, "mappings.csv");
        FileWriter writer = new FileWriter(mappingFile);
        writer.write("1,device1\n");
        writer.write("2,device2");
        writer.close();
        File readingFile = new File(tempFolder, "readings.csv");
        writer = new FileWriter(readingFile);
        writer.write(IntegrationTestHelper.smartMeterReading1);
        writer.write(IntegrationTestHelper.smartMeterReading2);
        writer.write(IntegrationTestHelper.smartMeterReading3);
        writer.write(IntegrationTestHelper.smartMeterReading4);
        writer.write(IntegrationTestHelper.smartMeterReading7);
        writer.write(IntegrationTestHelper.smartMeterReading8);
        writer.close();

        JSONObject request1 = new JSONObject()
                            .put("dataSource", "csv")
                            .put("dataRequired", "historical")
                            .put("microgrid", targetResourceID);
        JSONObject expectedResult = new JSONObject().put("uploadStatus", "1 readings uploaded successfully.");
        JSONObject actualResult = agentLauncher.processRequestParameters(request1);
        assertTrue(actualResult.similar(expectedResult));

        JSONObject request2 = new JSONObject()
                            .put("dataSource", "csv")
                            .put("dataRequired", "historical")
                            .put("microgrid", targetResourceID)
                            .put("dataBefore", "2022-10-26 18:23");
        expectedResult = new JSONObject().put("uploadStatus", "No valid reading found.");
        actualResult = agentLauncher.processRequestParameters(request2);
        assertTrue(actualResult.similar(expectedResult));
    }
}
