package uk.ac.cam.cares.jps.agent.fh;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.jena.sparql.function.library.print;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.semanticweb.owlapi.util.IRIComparator;
import org.skyscreamer.jsonassert.*;
import org.springframework.test.context.transaction.BeforeTransaction;

import com.bigdata.service.ndx.pipeline.IndexWriteTask.M;
import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import wiremock.org.eclipse.jetty.util.ajax.JSON;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import java.io.*;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;

public class FHAgentDerivationTest {
    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Container
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
													 .withExposedPorts(9999);
	
	// Create Docker container with postgres 13.3 image from Docker Hub
	@Container
	private PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:13.3");

    // The default instance used in the tests
    private FHAgent testAgent;
    private FHAgentDerivation testDerivator;
    RemoteStoreClient storeClient;
    JSONObject exampleData;
    // The mocking instance for the time series client
    @SuppressWarnings("unchecked")
    private final TimeSeriesClient<OffsetDateTime> mockTSClient = (TimeSeriesClient<OffsetDateTime>) Mockito.mock(TimeSeriesClient.class);

    // A default list of IRIs
    private final List<String> iris = Arrays.asList("iri1");
    // Default list of JSON keys
    private final String[] mockDerivVarKey = {"occupiedState1"};
    
    private static final String ONTODERIV = "https://www.theworldavatar.com/kg/ontoderivation/";
    private static final String ONTOAGE = "http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#";
    private static final String ONTOTS = "https://www.theworldavatar.com/kg/ontotimeseries/";
    private static final String W3TIME = "http://www.w3.org/2006/time#";

    private static final Prefix P_AGENT = SparqlBuilder.prefix("ontoagent",iri(ONTOAGE));
    private static final Prefix P_TS = SparqlBuilder.prefix("ontotimeseries", iri(ONTOTS));
    private static final Prefix P_W3TIME = SparqlBuilder.prefix("w3", iri(W3TIME));
    private static final Prefix P_DERIV = SparqlBuilder.prefix("ontoderivation", iri(ONTODERIV));

    private static final Iri Service = P_AGENT.iri("Service");
    private static final Iri hasTimeSeries = P_TS.iri("hasTimeSeries");
    private static final Iri hasHttpUrl = P_AGENT.iri("hasHttpUrl");
    private static final Iri hasTime = P_W3TIME.iri("hasTime");
    private static final Iri isDerivedFrom = P_DERIV.iri("isDerivedFrom");
    private static final Iri belongsTo = P_DERIV.iri("belongsTo");
    //Default list of timestamps

    //Mock set of keys from Thingsboard
    private final List<String> mockRawVarKey = Arrays.asList("avgDist1");

    //Request body for creating namespace
    private String reqBody = "com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n" +
                            "com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms\r\n" +
                            "com.bigdata.rdf.sail.isolatableIndices=false\r\n" +
                            "com.bigdata.rdf.sail.truthMaintenance=false\r\n" +
                            "com.bigdata.rdf.store.AbstractTripleStore.justify=false\r\n" +
                            "com.bigdata.rdf.sail.namespace=testDeriv\r\n" +
                            "com.bigdata.namespace.testDeriv.spo.com.bigdata.btree.BTree.branchingFactor=1024\r\n" +
                            "com.bigdata.rdf.store.AbstractTripleStore.quads=false\r\n" +
                            "com.bigdata.namespace.testDeriv.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n"+
                            "com.bigdata.journal.Journal.groupCommit=false\r\n" +
                            "com.bigdata.rdf.store.AbstractTripleStore.geoSpatial=false\r\n"+
                            "com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false";

    //Blazegraph endpoint
    private String sparql_endpoint;

    //Postgres endpoint and credentials
    private String jdbcURL;
    private String postgresUsername;
    private String postgresPassword;

    //Paths for temporary test files
    String mappingFile1;
        
    @Before
	public void startContainers() throws IOException, URISyntaxException {
		try {
			// Start Blazegraph container
			blazegraph.start();
			// Start postgreSQL container
			postgres.start();
		} catch (Exception e) {
			throw new JPSRuntimeException("Docker container startup failed. Please try running tests again");
		}

        //URI for blazegraph endpoint
        URIBuilder builder = new URIBuilder().setScheme("http").setHost(blazegraph.getHost()).setPort(blazegraph.getFirstMappedPort()).setPath("/blazegraph/namespace");

        // create a new namespace (endpoint) on blazegraph with geospatial enabled
		CloseableHttpClient httpclient = HttpClients.createDefault();
		HttpPost postRequest = new HttpPost(builder.build());
		postRequest.setEntity(new StringEntity(reqBody, ContentType.DEFAULT_TEXT));
		CloseableHttpResponse response = httpclient.execute(postRequest);
		
        //For some reason the getFirstHeader/ getLastHeader returns null
		//sparql_endpoint = response.getFirstHeader("Location").getValue();
        sparql_endpoint = new URIBuilder().setScheme("http").setHost(blazegraph.getHost()).setPort(blazegraph.getFirstMappedPort()).setPath("/blazegraph/namespace/testDeriv/sparql").toString();

		jdbcURL = postgres.getJdbcUrl();
        postgresUsername = postgres.getUsername();
        postgresPassword = postgres.getPassword();


    }

    private void writePropertyFile(String filepath, List<String> properties) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }

    @Before
    public void initializeAgent() throws IOException {
        // Create a properties file that points to a dummy mapping folder //
        // Create an empty folder
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Add mapping file into the empty folder
        mappingFile1 = Paths.get(mappingFolder.getAbsolutePath(), "example_mapping1.properties").toString();
        ArrayList<String> mappings1 = new ArrayList<>();
       
        mappings1.add(mockDerivVarKey[0] + "=example:prefix/api_" + mockDerivVarKey[0]);
        writePropertyFile(mappingFile1, mappings1);
        // Filepath for the properties file
        
        String agentPropFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        String clientPropFile = Paths.get(folder.getRoot().toString(), "client.properties").toString();
        String iriMapperFile = Paths.get(folder.getRoot().toString(), "iriMapping.txt").toString();
        
        writePropertyFile(agentPropFile, Arrays.asList(new String[]{"thingsboard.mappingfolder=TEST_MAPPINGS", 
            "derivation.mapping=avgDist1:occupiedState1", 
            "threshold.tally = 170.", 
            "tally.limit = 1",
            "tally.max = 2",
            "tally.min = 0",
            "decrease.factor = 0.15",
            "increase.factor = 0.5",
            "data_bridge.url = https://example.com", 
            "use_stack = false", 
            "derivation.baseurl = http://derivationexample.com/triplestore/repository/"
        }));
        
        String[] clientPropParam = {"db_url="+jdbcURL, "db.user="+postgresUsername, "db.password="+postgresPassword,"sparql.query.endpoint="+sparql_endpoint, "sparql.update.endpoint="+sparql_endpoint};
        writePropertyFile(clientPropFile, Arrays.asList(clientPropParam));
        // To create testAgent without an exception being thrown, SystemLambda is used to mock an environment variable
        // To mock the environment variable, a try catch need to be used
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
        		 testAgent = new FHAgent(agentPropFile);
                 testDerivator = new FHAgentDerivation(agentPropFile, clientPropFile, iriMapperFile, testAgent.getTimeseriesIRI());
        	 });
        }
        // There should not be any exception thrown as the agent is initiated correctly
        catch (Exception e) {
            System.out.println(e);
            throw new IOException(e);
        }
        // Set the mocked time series client
        //testAgent.setTsClient(mockTSClient);

        //Set the RemoteStoreClient
        //storeClient =  new RemoteStoreClient(sparql_endpoint, sparql_endpoint);
        storeClient = testDerivator.storeClient;
    }

    @Before
    public void createExampleData() {
        exampleData = new JSONObject();
        exampleData.put("timeClass", "INSTANTANEOUS");
        JSONArray testTimestamps = new JSONArray(new String[]{"2009-02-13T21:20:00", "2009-02-13T21:20:01", "2009-02-13T21:20:02"});
        exampleData.put("ts", testTimestamps);
        JSONArray testValues = new JSONArray(new Double[]{1.,2.,3.});
        String testIRI = "example:prefix/api_"+mockDerivVarKey[0];
        JSONObject testValPair = new JSONObject();
        testValPair.put(testIRI, testValues);
        exampleData.put("values", testValPair);
    }

    @Test
    public void testConstructor() throws IOException {
        //TODO is this needed? Most of the time the agent fails first before derivation instantiation happens

        //Empty agent.properties
        try {
            String propertiesFile = Paths.get(folder.getRoot().toString(), "empty_agent.properties").toString();
            // Run constructor on an empty file should give an exception
            writePropertyFile(propertiesFile, new ArrayList<>());

            String clientPropFile = Paths.get(folder.getRoot().toString(), "client.properties").toString();
            String[] clientPropParam = {"db_url="+jdbcURL, "db.user="+postgresUsername, "db.password="+postgresPassword, "sparql.query.endpoint="+sparql_endpoint, "sparql.update.endpoint="+sparql_endpoint};
            writePropertyFile(clientPropFile, Arrays.asList(clientPropParam));
            String iriMapperFile = Paths.get(folder.getRoot().toString(), "iriMapping.txt").toString();
        
        
            new FHAgentDerivation(propertiesFile, clientPropFile, iriMapperFile, testAgent.getTimeseriesIRI());
            Assert.fail();
        }
        
        catch (Exception e) {
            Assert.assertTrue(e.toString().contains("Error on initialising derivation instantiation"));
        }
        //Empty client.properties -- The FHAgent constructor failed first here, is this one still necessary?

        //Check if mapping file is generated and content is consistent
        try {
            String iriMapperFile = Paths.get(folder.getRoot().toString(), "iriMapping.txt").toString();
            File mapping = new File(iriMapperFile);
            Assert.assertTrue(mapping.exists());

            Map<String, String> iriMapper = testDerivator.iriMap;
            Map<String,String> iriFromFile = new HashMap<>();
            //get content of iriMapping.txt
            BufferedReader reader;

            try {
                reader = new BufferedReader(new FileReader(iriMapperFile));
                String line = reader.readLine();

                while (line != null) {
                    String[] pair = line.split("=");
                    iriFromFile .put(pair[0], pair[1]);
                    
                    line = reader.readLine();
                }

                reader.close();
            } catch (IOException e) {
                e.printStackTrace();
            }

            for(String key: iriMapper.keySet()){
                Assert.assertEquals(iriMapper.get(key), iriFromFile.get(key));
            }


        } catch (Exception e) {
            throw e;
        }

        //If mapping file exist, use mapping file instead
        //Compare the IRI after call, should be the same before and after call
        //iriMap already exist from previous test

        //TODO make the config in a method to reduce line usage
        try{
            Map<String, String> iriMapper = testDerivator.iriMap;
            Map<String, String> iriFromFile = new HashMap<>();

            String agentPropFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
            String clientPropFile = Paths.get(folder.getRoot().toString(), "client.properties").toString();
            String iriMapperFile = Paths.get(folder.getRoot().toString(), "iriMapping.txt").toString();

            //call constructor again to see if createIRI is called
            new FHAgentDerivation(agentPropFile, clientPropFile, iriMapperFile, testAgent.getTimeseriesIRI());

            BufferedReader reader;
            try {
                reader = new BufferedReader(new FileReader(iriMapperFile));
                String line = reader.readLine();

                while (line != null) {
                    String[] pair = line.split("=");
                    iriFromFile .put(pair[0], pair[1]);
                    
                    line = reader.readLine();
                }

                reader.close();
            } catch (IOException e) {
                e.printStackTrace();
            }

            for(String key: iriMapper.keySet()){
                Assert.assertEquals(iriMapper.get(key), iriFromFile.get(key));
            }



        }catch (Exception e){
            throw e;
        }



    }

    @Test
    public void testInstantiateAgent() {
        //Test with dummy URL
        testDerivator.instantiateAgent("http://localhost:1010/fh-agent/instantiate");
        SelectQuery queryURL = Queries.SELECT();
        queryURL.where(queryURL.var().has(hasHttpUrl, queryURL.var())).prefix(P_AGENT);

        JSONArray queryUrlResult = storeClient.executeQuery(queryURL.getQueryString());
        //System.out.println(queryResult);
        //Should only have 1 FHAgent instance; 1 column for iri, 1 for url
        Assert.assertEquals(1, queryUrlResult.length());
        JSONObject columns = queryUrlResult.getJSONObject(0);
        String agentIRI = columns.getString("x1");
        Assert.assertEquals("http://localhost:1010/fh-agent/instantiate", agentIRI);

        //Check if agent is instantiated
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(RDF.TYPE, Service)).prefix(P_AGENT);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        Assert.assertTrue(queryResult.length() >= 1);

        //DEBUG show all triple for debugging
        SelectQuery queryAllTriple = Queries.SELECT();
        queryAllTriple.where(queryAllTriple.var().has(queryAllTriple.var(), queryAllTriple.var()));
        System.out.println(storeClient.execute(queryAllTriple.getQueryString()));

        //Check raw and derived var instances
        //Derived var has belongsTo derivationWithTimeseries
        SelectQuery queryDeriv = Queries.SELECT();
        queryDeriv.where(queryDeriv.var().has(belongsTo, queryDeriv.var())).prefix(P_DERIV);
        //System.out.println(queryDeriv.getQueryString());
        JSONArray queryResultDeriv = storeClient.executeQuery(queryDeriv.getQueryString());
        //System.out.println(queryResultDeriv);
        //Must be equal to the number of derived variables
        Assert.assertTrue( queryResultDeriv.getJSONObject(0).getString("x0").equals("example:prefix/api_occupiedState1"));

        //Raw var has isDerivedFrom
        SelectQuery queryRaw = Queries.SELECT();
        queryRaw.where(queryRaw.var().has(isDerivedFrom, queryRaw.var())).prefix(P_DERIV);
        //IRI Map contains both derived and raw var
        //The number of raw var instances must be equal to irimap size - # of derived var
        System.out.println(storeClient.executeQuery(queryRaw.getQueryString()));
        Assert.assertEquals(testDerivator.iriMap.size()-testAgent.getNumberOfTimeSeries(), storeClient.executeQuery(queryRaw.getQueryString()).length());
    }


    @After
	public void cleanUp() {
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
		if (postgres.isRunning()) {
			postgres.stop();
		}
	}
}
