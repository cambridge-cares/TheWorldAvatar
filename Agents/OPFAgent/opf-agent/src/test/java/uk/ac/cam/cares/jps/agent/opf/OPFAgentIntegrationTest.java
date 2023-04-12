package test.java.uk.ac.cam.cares.jps.agent.opf;
import static org.junit.Assert.assertTrue;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Connection;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

import org.json.JSONObject;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import main.java.uk.ac.cam.cares.jps.agent.opf.OPFAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.StoreRouter;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.base.config.AgentLocator;

@Testcontainers
public class OPFAgentIntegrationTest {

    @TempDir
    File tempFolder;

    private OPFAgent agent = new OPFAgent();

	////////////////////////////////////////////////
	
	//User defined variables
	//set the desired access agent version number here
	static final String ACCESS_AGENT_VERSION = "1.7.0";
	
	//////////////////////////////////////////////////
	
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

	//////////////////////////////////////////////////
	
	@BeforeAll
	static void setupAll() {
		try {
			STORE_ROUTER_CONTAINER.start();
			ACCESS_AGENT_CONTAINER.start();	
		} catch (Exception e) {
			throw new JPSRuntimeException("OPFAgentIntegrationTest: Docker container startup failed. Please try running tests again");
		}
	}
	
	@BeforeEach
	void setupEach() throws IOException {
		try {	
			targetStoreContainer.start();	
            postgreSQLContainer.start();				
		} catch (Exception e) {
			throw new JPSRuntimeException("OPFAgentIntegrationTest: Docker container startup failed. Please try running tests again");
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
		assertTrue("OPFAgentIntegrationTest: Routing data upload failed.", uploaded.equals("1 endpoint(s) uploaded."));
				
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

        agent = new OPFAgent(){
            @Override
            public void loadTSClientConfigs(String filepath) throws IOException {
                this.dbUrl = rdbUrl;
                this.dbUsername = rdbUsername;
                this.dbPassword = rdbPassword;
                this.kbClient.setQueryEndpoint(endpoint);
                this.kbClient.setUpdateEndpoint(endpoint);
            }
            @Override
            // Skip OPF simulation
            public String runPythonScript(String script, String folder, String[] fileNames) throws Exception {
                String result = "";
                // Check that input files are generated
                String[] requiredFiles = {"/bus.txt", "/branch.txt", "/gen.txt", "/genCost.txt", "/mappingforbus.csv", "/mappingforbranch.csv",
                "/mappingforgenerator.csv", "/mappingforgeneratorcost.csv", "/mappingforbattery.csv", "/baseMVA.txt"};
                for (int i = 0; i < 10; i++) {
                    assertTrue(new File(folder + requiredFiles[i]).isFile());
                }
                // Generate fake output files
                File busOutFile = new File(folder, "outputBusOPF.txt");
                File outputStatus = new File(folder, "outputStatus.txt");
                FileWriter writer = new FileWriter(busOutFile);
                writer.write("1	1.0	0.0	1.2881944114750332	1.3084762183180412	0.0	0.0\n");
                writer.close();
                writer = new FileWriter(outputStatus);
                writer.write("1\nConverged!\n");
                writer.close();

                return result;
            }
        };

        try {
            agent.loadTSClientConfigs("");
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
    
    //////////////////////////////////////////////////
	// Integration tests
    @Test
    public void testReadTimeSeriesData() {
        String[] busIRIsWithoutSolar = {"http://www.sampleIRI.org/sample_IRI_Pd", "http://www.sampleIRI.org/sample_IRI_Qd"};
        String[] busIRIsWithSolar = {"http://www.sampleIRI.org/sample_IRI_Pd", "http://www.sampleIRI.org/sample_IRI_Qd", 
                                    "http://www.sampleIRI.org/sample_IRI_solarPd", "http://www.sampleIRI.org/sample_IRI_solarQd"};
        List<String[]> dataIRIsWithoutSolar = new ArrayList<String[]>();
        List<String[]> dataIRIsWithSolar = new ArrayList<String[]>();
        dataIRIsWithoutSolar.add(busIRIsWithoutSolar);
        dataIRIsWithSolar.add(busIRIsWithSolar);
        String[] expectedValuesWithoutSolar = {"0", "0"};
        String[] expectedValuesWithSolar = {"0", "0", "3", null};

        OffsetDateTime time = OffsetDateTime.parse("2020-01-01T08:00:00+00:00");
        List<String[]> actualValuesWithoutSolar = agent.readTimeSeriesData(dataIRIsWithoutSolar, time, 1, "false");
        List<String[]> actualValuesWithSolar = agent.readTimeSeriesData(dataIRIsWithSolar, time, 1, "true");
        assertArrayEquals(expectedValuesWithoutSolar, actualValuesWithoutSolar.get(0));
        assertArrayEquals(expectedValuesWithSolar, actualValuesWithSolar.get(0));
    }

    @Test
    public void testUpdateBusInfo() throws IOException {
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);

        File busMapping = new File(tempFolder, "mappingforbus.csv");
        FileWriter writer = new FileWriter(busMapping);
        writer.write("1,4,bus\n");
        writer.close();
        
        List<String[]> busValues = new ArrayList<String[]>();
        String[] busArray = {"4", "1.0", "0.0", "1.2881944114750332", "1.3084762183180412", "0.0", "0.0"};
        busValues.add(busArray);
        OffsetDateTime time = OffsetDateTime.parse("2020-01-01T08:00:00+00:00");
        String baseUrl = tempFolder.toPath().toString();
        agent.updateBusInfo(targetResourceID, busValues, time, baseUrl);

        // Check update results
        List<String> vmValues = new ArrayList<String>();
        List<String> vaValues = new ArrayList<String>();
        vmValues.add("http://www.sampleIRI.org/sample_IRI_Vm");
        vaValues.add("http://www.sampleIRI.org/sample_IRI_Va");
        
        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeries<OffsetDateTime> vmTimeseries = tsClient.getTimeSeriesWithinBounds(vmValues, time, time, conn);
            TimeSeries<OffsetDateTime> vaTimeseries = tsClient.getTimeSeriesWithinBounds(vaValues, time, time, conn);
            String vmOutput = vmTimeseries.getValuesAsString(vmValues.get(0)).get(0);
            String vaOutput = vaTimeseries.getValuesAsString(vaValues.get(0)).get(0);
            assertEquals("1.0", vmOutput);
            assertEquals("0.0", vaOutput);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
    }

    @Test
    public void testGenerateInputWithoutSolar() throws IOException {
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        String baseUrl = tempFolder.toPath().toString();
        agent.generateInputWithAccessAgent(targetResourceID, baseUrl, "1", "2020-01-01T08:00:00+00:00", "false");

        // Expected input files to be generated
        String[] fileNames = {"/bus.txt", "/branch.txt", "/gen.txt", "/genCost.txt", "/mappingforbus.csv", "/mappingforbranch.csv",
                             "/mappingforgenerator.csv", "/mappingforgeneratorcost.csv", "/mappingforbattery.csv", "/baseMVA.txt"};

        for (int i = 0; i < 10; i++) {
            assertTrue(new File(baseUrl + fileNames[i]).isFile());
        }

        String actualBus = FileUtil.readFileLocally(baseUrl + fileNames[0]);
        String expectedBus = "1	3	0	0	0	0	1	1.0	0.0	11	1	1	1\n";
        assertEquals(expectedBus, actualBus);
        String actualBranch = FileUtil.readFileLocally(baseUrl + fileNames[1]);
        String expectedBranch = "1	1	0.018436446	0.012435537	0.0	9900	0	0	0	0	1	-360	360\n";
        assertEquals(expectedBranch, actualBranch);
        String actualGen = FileUtil.readFileLocally(baseUrl + fileNames[2]);
        String expectedGen = "1	0.0	0.0	10.0	-10.0	1	100	1	10.0	0	0	0	0	0	0	0	0	0	0	0	0\n";
        assertEquals(expectedGen, actualGen);
        String actualGenCost = FileUtil.readFileLocally(baseUrl + fileNames[3]);
        String expectedGenCost = "2	0	0	3	0	20	0\n";
        assertEquals(expectedGenCost, actualGenCost);
        String actualbaseMVA = FileUtil.readFileLocally(baseUrl + fileNames[9]);
        String expectedBaseMVA = "1";
        assertEquals(expectedBaseMVA, actualbaseMVA);
    }

    @Test
    public void testGenerateInputWithSolar() throws IOException {
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        String baseUrl = tempFolder.toPath().toString();
        agent.generateInputWithAccessAgent(targetResourceID, baseUrl, "1", "2020-01-01T08:00:00+00:00", "true");

        // Expected input files to be generated
        String[] fileNames = {"/bus.txt", "/branch.txt", "/gen.txt", "/genCost.txt", "/mappingforbus.csv", "/mappingforbranch.csv",
                             "/mappingforgenerator.csv", "/mappingforgeneratorcost.csv", "/mappingforbattery.csv", "/baseMVA.txt"};

        for (int i = 0; i < 10; i++) {
            assertTrue(new File(baseUrl + fileNames[i]).isFile());
        }

        String actualBus = FileUtil.readFileLocally(baseUrl + fileNames[0]);
        String expectedBus = "1	3	-3.000000000	0	0	0	1	1.0	0.0	11	1	1	1\n";
        assertEquals(expectedBus, actualBus);
        String actualBranch = FileUtil.readFileLocally(baseUrl + fileNames[1]);
        String expectedBranch = "1	1	0.018436446	0.012435537	0.0	9900	0	0	0	0	1	-360	360\n";
        assertEquals(expectedBranch, actualBranch);
        String actualGen = FileUtil.readFileLocally(baseUrl + fileNames[2]);
        String expectedGen = "1	0.0	0.0	10.0	-10.0	1	100	1	10.0	0	0	0	0	0	0	0	0	0	0	0	0\n";
        assertEquals(expectedGen, actualGen);
        String actualGenCost = FileUtil.readFileLocally(baseUrl + fileNames[3]);
        String expectedGenCost = "2	0	0	3	0	20	0\n";
        assertEquals(expectedGenCost, actualGenCost);
        String actualbaseMVA = FileUtil.readFileLocally(baseUrl + fileNames[9]);
        String expectedBaseMVA = "1";
        assertEquals(expectedBaseMVA, actualbaseMVA);
    }

    @Test
    public void testDoConversion() throws IOException {
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        String baseUrl = tempFolder.toPath().toString();
        String modelType = "OPF";

        // Prepare output files to use
        File busOutFile = new File(tempFolder, "outputBusOPF.txt");
        File busMapping = new File(tempFolder, "mappingforbus.csv");

        FileWriter writer = new FileWriter(busOutFile);
        writer.write("4	1.0	0.0	1.2881944114750332	1.3084762183180412	0.0	0.0\n");
        writer.close();
        writer = new FileWriter(busMapping);
        writer.write("1,4,bus\n");
        writer.close();   
        agent.doConversionWithAccessAgent(targetResourceID, baseUrl, OffsetDateTime.parse("2020-01-01T08:00:00+00:00"), modelType);

        // Check update results
        OffsetDateTime time = OffsetDateTime.parse("2020-01-01T08:00:00+00:00");
        List<String> vmValues = new ArrayList<String>();
        List<String> vaValues = new ArrayList<String>();
        vmValues.add("http://www.sampleIRI.org/sample_IRI_Vm");
        vaValues.add("http://www.sampleIRI.org/sample_IRI_Va");

        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeries<OffsetDateTime> vmTimeseries = tsClient.getTimeSeriesWithinBounds(vmValues, time, time, conn);
            TimeSeries<OffsetDateTime> vaTimeseries = tsClient.getTimeSeriesWithinBounds(vaValues, time, time, conn);
            String vmOutput = vmTimeseries.getValuesAsString(vmValues.get(0)).get(0);
            String vaOutput = vaTimeseries.getValuesAsString(vaValues.get(0)).get(0);
            assertEquals("1.0", vmOutput);
            assertEquals("0.0", vaOutput);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
    }

    @Test
    public void testStartSimulationWithoutSolar() throws IOException {
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        String baseUrl = tempFolder.toPath().toString();
        String modelType = "OPF";

        JSONObject expectedOutput = new JSONObject();
		expectedOutput.put("electricalnetwork", targetResourceID);
        expectedOutput.put("status", "converged");

        try {
            JSONObject actualOutput = agent.startSimulationWithAccessAgent(targetResourceID, baseUrl, modelType, 
                                    "1", "2020-01-01T08:00:00+00:00", "false");
            // Check output status
            assertEquals(expectedOutput.get("electricalnetwork"), actualOutput.get("electricalnetwork"));
            assertEquals(expectedOutput.get("status"), actualOutput.get("status"));
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        // Expected input files to be generated
        String[] fileNames = {"/bus.txt", "/branch.txt", "/gen.txt", "/genCost.txt", "/mappingforbus.csv", "/mappingforbranch.csv",
        "/mappingforgenerator.csv", "/mappingforgeneratorcost.csv", "/mappingforbattery.csv", "/baseMVA.txt"};
        String actualBus = FileUtil.readFileLocally(baseUrl + fileNames[0]);
        String expectedBus = "1	3	0	0	0	0	1	1.0	0.0	11	1	1	1\n";
        assertEquals(expectedBus, actualBus);
        String actualBranch = FileUtil.readFileLocally(baseUrl + fileNames[1]);
        String expectedBranch = "1	1	0.018436446	0.012435537	0.0	9900	0	0	0	0	1	-360	360\n";
        assertEquals(expectedBranch, actualBranch);
        String actualGen = FileUtil.readFileLocally(baseUrl + fileNames[2]);
        String expectedGen = "1	0.0	0.0	10.0	-10.0	1	100	1	10.0	0	0	0	0	0	0	0	0	0	0	0	0\n";
        assertEquals(expectedGen, actualGen);
        String actualGenCost = FileUtil.readFileLocally(baseUrl + fileNames[3]);
        String expectedGenCost = "2	0	0	3	0	20	0\n";
        assertEquals(expectedGenCost, actualGenCost);
        String actualbaseMVA = FileUtil.readFileLocally(baseUrl + fileNames[9]);
        String expectedBaseMVA = "1";
        assertEquals(expectedBaseMVA, actualbaseMVA);
        
        // Check update results
        OffsetDateTime time = OffsetDateTime.parse("2020-01-01T08:00:00+00:00");
        List<String> vmValues = new ArrayList<String>();
        List<String> vaValues = new ArrayList<String>();
        vmValues.add("http://www.sampleIRI.org/sample_IRI_Vm");
        vaValues.add("http://www.sampleIRI.org/sample_IRI_Va");

        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeries<OffsetDateTime> vmTimeseries = tsClient.getTimeSeriesWithinBounds(vmValues, time, time, conn);
            TimeSeries<OffsetDateTime> vaTimeseries = tsClient.getTimeSeriesWithinBounds(vaValues, time, time, conn);
            String vmOutput = vmTimeseries.getValuesAsString(vmValues.get(0)).get(0);
            String vaOutput = vaTimeseries.getValuesAsString(vaValues.get(0)).get(0);
            assertEquals("1.0", vmOutput);
            assertEquals("0.0", vaOutput);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
    }

    @Test
    public void testStartSimulationWithSolar() throws IOException {
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        String baseUrl = tempFolder.toPath().toString();
        String modelType = "OPF";

        JSONObject expectedOutput = new JSONObject();
		expectedOutput.put("electricalnetwork", targetResourceID);
        expectedOutput.put("status", "converged");

        try {
            JSONObject actualOutput = agent.startSimulationWithAccessAgent(targetResourceID, baseUrl, modelType, 
                                "1", "2020-01-01T08:00:00+00:00", "true");
            // Check output status
            assertEquals(expectedOutput.get("electricalnetwork"), actualOutput.get("electricalnetwork"));
            assertEquals(expectedOutput.get("status"), actualOutput.get("status"));
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        // Expected input files to be generated
        String[] fileNames = {"/bus.txt", "/branch.txt", "/gen.txt", "/genCost.txt", "/mappingforbus.csv", "/mappingforbranch.csv",
        "/mappingforgenerator.csv", "/mappingforgeneratorcost.csv", "/mappingforbattery.csv", "/baseMVA.txt"};
        String actualBus = FileUtil.readFileLocally(baseUrl + fileNames[0]);
        String expectedBus = "1	3	-3.000000000	0	0	0	1	1.0	0.0	11	1	1	1\n";
        assertEquals(expectedBus, actualBus);
        String actualBranch = FileUtil.readFileLocally(baseUrl + fileNames[1]);
        String expectedBranch = "1	1	0.018436446	0.012435537	0.0	9900	0	0	0	0	1	-360	360\n";
        assertEquals(expectedBranch, actualBranch);
        String actualGen = FileUtil.readFileLocally(baseUrl + fileNames[2]);
        String expectedGen = "1	0.0	0.0	10.0	-10.0	1	100	1	10.0	0	0	0	0	0	0	0	0	0	0	0	0\n";
        assertEquals(expectedGen, actualGen);
        String actualGenCost = FileUtil.readFileLocally(baseUrl + fileNames[3]);
        String expectedGenCost = "2	0	0	3	0	20	0\n";
        assertEquals(expectedGenCost, actualGenCost);
        String actualbaseMVA = FileUtil.readFileLocally(baseUrl + fileNames[9]);
        String expectedBaseMVA = "1";
        assertEquals(expectedBaseMVA, actualbaseMVA);
        
        // Check update results
        OffsetDateTime time = OffsetDateTime.parse("2020-01-01T08:00:00+00:00");
        List<String> vmValues = new ArrayList<String>();
        List<String> vaValues = new ArrayList<String>();
        vmValues.add("http://www.sampleIRI.org/sample_IRI_Vm");
        vaValues.add("http://www.sampleIRI.org/sample_IRI_Va");

        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeries<OffsetDateTime> vmTimeseries = tsClient.getTimeSeriesWithinBounds(vmValues, time, time, conn);
            TimeSeries<OffsetDateTime> vaTimeseries = tsClient.getTimeSeriesWithinBounds(vaValues, time, time, conn);
            String vmOutput = vmTimeseries.getValuesAsString(vmValues.get(0)).get(0);
            String vaOutput = vaTimeseries.getValuesAsString(vaValues.get(0)).get(0);
            assertEquals("1.0", vmOutput);
            assertEquals("0.0", vaOutput);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
    }

    @Test
    public void testProcessRequestParametersWithoutSolar() {
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        String baseUrl = AgentLocator.getCurrentJpsAppDirectory(this) + "/opf-result";
        JSONObject requestParams = new JSONObject()
                                .put("electricalnetwork", targetResourceID)
                                .put("baseMVA", "1")
                                .put("time", "2020-01-01T08:00:00+00:00")
                                .put("hasSolar", "false");

        JSONObject expectedOutput = new JSONObject();
        expectedOutput.put("electricalnetwork", targetResourceID);
        expectedOutput.put("status", "converged");     
        
        JSONObject actualOutput = agent.processRequestParameters(requestParams, null);
        assertEquals(expectedOutput.get("electricalnetwork"), actualOutput.get("electricalnetwork"));
        assertEquals(expectedOutput.get("status"), actualOutput.get("status"));

        // Expected input files to be generated
        String[] fileNames = {"/bus.txt", "/branch.txt", "/gen.txt", "/genCost.txt", "/mappingforbus.csv", "/mappingforbranch.csv",
        "/mappingforgenerator.csv", "/mappingforgeneratorcost.csv", "/mappingforbattery.csv", "/baseMVA.txt"};
        String actualBus = FileUtil.readFileLocally(baseUrl + fileNames[0]);
        String expectedBus = "1	3	0	0	0	0	1	1.0	0.0	11	1	1	1\n";
        assertEquals(expectedBus, actualBus);
        String actualBranch = FileUtil.readFileLocally(baseUrl + fileNames[1]);
        String expectedBranch = "1	1	0.018436446	0.012435537	0.0	9900	0	0	0	0	1	-360	360\n";
        assertEquals(expectedBranch, actualBranch);
        String actualGen = FileUtil.readFileLocally(baseUrl + fileNames[2]);
        String expectedGen = "1	0.0	0.0	10.0	-10.0	1	100	1	10.0	0	0	0	0	0	0	0	0	0	0	0	0\n";
        assertEquals(expectedGen, actualGen);
        String actualGenCost = FileUtil.readFileLocally(baseUrl + fileNames[3]);
        String expectedGenCost = "2	0	0	3	0	20	0\n";
        assertEquals(expectedGenCost, actualGenCost);
        String actualbaseMVA = FileUtil.readFileLocally(baseUrl + fileNames[9]);
        String expectedBaseMVA = "1";
        assertEquals(expectedBaseMVA, actualbaseMVA);
                
        // Check update results
        OffsetDateTime time = OffsetDateTime.parse("2020-01-01T08:00:00+00:00");
        List<String> vmValues = new ArrayList<String>();
        List<String> vaValues = new ArrayList<String>();
        vmValues.add("http://www.sampleIRI.org/sample_IRI_Vm");
        vaValues.add("http://www.sampleIRI.org/sample_IRI_Va");

        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeries<OffsetDateTime> vmTimeseries = tsClient.getTimeSeriesWithinBounds(vmValues, time, time, conn);
            TimeSeries<OffsetDateTime> vaTimeseries = tsClient.getTimeSeriesWithinBounds(vaValues, time, time, conn);
            String vmOutput = vmTimeseries.getValuesAsString(vmValues.get(0)).get(0);
            String vaOutput = vaTimeseries.getValuesAsString(vaValues.get(0)).get(0);
            assertEquals("1.0", vmOutput);
            assertEquals("0.0", vaOutput);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
    }

    @Test
    public void testProcessRequestParametersWithSolar() {
        targetStoreClient = IntegrationTestHelper.populateRemoteStore(targetStoreClient);
        String baseUrl = AgentLocator.getCurrentJpsAppDirectory(this) + "/opf-result";
        JSONObject requestParams = new JSONObject()
                                .put("electricalnetwork", targetResourceID)
                                .put("baseMVA", "1")
                                .put("time", "2020-01-01T08:00:00+00:00")
                                .put("hasSolar", "true");

        JSONObject expectedOutput = new JSONObject();
        expectedOutput.put("electricalnetwork", targetResourceID);
        expectedOutput.put("status", "converged");     
        
        JSONObject actualOutput = agent.processRequestParameters(requestParams, null);
        assertEquals(expectedOutput.get("electricalnetwork"), actualOutput.get("electricalnetwork"));
        assertEquals(expectedOutput.get("status"), actualOutput.get("status"));

        // Expected input files to be generated
        String[] fileNames = {"/bus.txt", "/branch.txt", "/gen.txt", "/genCost.txt", "/mappingforbus.csv", "/mappingforbranch.csv",
        "/mappingforgenerator.csv", "/mappingforgeneratorcost.csv", "/mappingforbattery.csv", "/baseMVA.txt"};
        String actualBus = FileUtil.readFileLocally(baseUrl + fileNames[0]);
        String expectedBus = "1	3	-3.000000000	0	0	0	1	1.0	0.0	11	1	1	1\n";
        assertEquals(expectedBus, actualBus);
        String actualBranch = FileUtil.readFileLocally(baseUrl + fileNames[1]);
        String expectedBranch = "1	1	0.018436446	0.012435537	0.0	9900	0	0	0	0	1	-360	360\n";
        assertEquals(expectedBranch, actualBranch);
        String actualGen = FileUtil.readFileLocally(baseUrl + fileNames[2]);
        String expectedGen = "1	0.0	0.0	10.0	-10.0	1	100	1	10.0	0	0	0	0	0	0	0	0	0	0	0	0\n";
        assertEquals(expectedGen, actualGen);
        String actualGenCost = FileUtil.readFileLocally(baseUrl + fileNames[3]);
        String expectedGenCost = "2	0	0	3	0	20	0\n";
        assertEquals(expectedGenCost, actualGenCost);
        String actualbaseMVA = FileUtil.readFileLocally(baseUrl + fileNames[9]);
        String expectedBaseMVA = "1";
        assertEquals(expectedBaseMVA, actualbaseMVA);
                
        // Check update results
        OffsetDateTime time = OffsetDateTime.parse("2020-01-01T08:00:00+00:00");
        List<String> vmValues = new ArrayList<String>();
        List<String> vaValues = new ArrayList<String>();
        vmValues.add("http://www.sampleIRI.org/sample_IRI_Vm");
        vaValues.add("http://www.sampleIRI.org/sample_IRI_Va");

        try (Connection conn = rdbStoreClient.getConnection()) {
            TimeSeries<OffsetDateTime> vmTimeseries = tsClient.getTimeSeriesWithinBounds(vmValues, time, time, conn);
            TimeSeries<OffsetDateTime> vaTimeseries = tsClient.getTimeSeriesWithinBounds(vaValues, time, time, conn);
            String vmOutput = vmTimeseries.getValuesAsString(vmValues.get(0)).get(0);
            String vaOutput = vaTimeseries.getValuesAsString(vaValues.get(0)).get(0);
            assertEquals("1.0", vmOutput);
            assertEquals("0.0", vaOutput);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
    }    
}
