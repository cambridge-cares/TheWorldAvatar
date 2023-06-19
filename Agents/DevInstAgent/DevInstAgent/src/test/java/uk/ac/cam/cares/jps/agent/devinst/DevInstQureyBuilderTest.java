package uk.ac.cam.cares.jps.agent.devinst;


import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.jena.base.Sys;
import org.apache.jena.sparql.function.library.print;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static org.mockito.ArgumentMatchers.isA;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.semanticweb.owlapi.util.IRIComparator;
import org.springframework.test.context.transaction.BeforeTransaction;

import com.bigdata.service.ndx.pipeline.IndexWriteTask.M;
import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import wiremock.com.jayway.jsonpath.internal.function.text.Length;
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

public class DevInstQureyBuilderTest {
    private static final String ONTODEV = "https://www.theworldavatar.com/kg/ontodevice/";
    private static final Prefix P_DEV = SparqlBuilder.prefix("ontodevice",iri(ONTODEV));
    private static final Prefix P_SAREF = SparqlBuilder.prefix("saref", iri("https://saref.etsi.org/core/"));
    private static final Prefix P_AGENT = SparqlBuilder.prefix("ontoagent",iri("https://www.theworldavatar.com/kg/ontoagent/"));
    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));

    private static final Iri SmartSensor = P_DEV.iri("SmartSensor");
    private static final Iri MicroController = P_DEV.iri("MicroController");
    private static final Iri Sensor = P_SAREF.iri("Sensor");
    private static final Iri ProximitySensor = P_DEV.iri("ProximitySensor");

    private static final Iri consistsOf = P_SAREF.iri("consistsOf");
    private static final Iri sendsSignalTo = P_DEV.iri("sendsSignalTo");
    private static final Iri measures = P_DEV.iri("measures");
    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    @Container
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
													 .withExposedPorts(9999);

    private DevInstQueryBuilder queryBuilder;
    RemoteStoreClient storeClient;
    
    JSONObject exampleRequest;
    JSONObject exampleFind;
    JSONObject exampleEmptyMap;
    JSONObject exampleAddQuery;

    //Blazegraph endpoint
    private String sparql_endpoint;

    private String reqBody = "com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n"+
    "com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms\r\n"+
    "com.bigdata.rdf.sail.isolatableIndices=false\r\n"+
    "com.bigdata.rdf.sail.truthMaintenance=false\r\n"+
    "com.bigdata.rdf.store.AbstractTripleStore.justify=false\r\n"+
    "com.bigdata.rdf.sail.namespace=testDeriv\r\n"+
    "com.bigdata.namespace.testDeriv.spo.com.bigdata.btree.BTree.branchingFactor=1024\r\n"+
    "com.bigdata.rdf.store.AbstractTripleStore.quads=false\r\n"+
    "com.bigdata.namespace.testDeriv.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n"+
    "com.bigdata.journal.Journal.groupCommit=false\r\n"+
    "com.bigdata.rdf.store.AbstractTripleStore.geoSpatial=false\r\n"+
    "com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false";

    @Ignore("Test containers requires docker to function")
    @Before
    public void startContainers() throws IOException, URISyntaxException {
		try {
			// Start Blazegraph container
			blazegraph.start();
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

    @Ignore("Test containers requires docker to function")
    @Before
    public void initializeAgent() throws IOException {
        // Create a properties file that points to a dummy mapping folder //
        // Create an empty folder
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);

        // Filepath for the properties file
        
        String clientPropFile = Paths.get(folder.getRoot().toString(), "client.properties").toString();
        
        String[] clientPropParam = {"sparql.query.endpoint="+sparql_endpoint, "sparql.update.endpoint="+sparql_endpoint};
        writePropertyFile(clientPropFile, Arrays.asList(clientPropParam));

        //Set the RemoteStoreClient
        storeClient =  new RemoteStoreClient(sparql_endpoint, sparql_endpoint);
        //storeClient = queryBuilder.storeClient;
        // To create testAgent without an exception being thrown, SystemLambda is used to mock an environment variable
        // To mock the environment variable, a try catch need to be used
        try {
        	SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                 queryBuilder = new DevInstQueryBuilder(storeClient);
        	 });
        }
        // There should not be any exception thrown as the agent is initiated correctly
        catch (Exception e) {
            System.out.println(e);
            throw new IOException(e);
        }
        // Set the mocked time series client
        //testAgent.setTsClient(mockTSClient);

        
    }

    public static JSONObject parseJSONFile(String filename) throws JSONException, IOException {
        String content = new String(Files.readAllBytes(Paths.get(filename)));
        return new JSONObject(content);
    }
    
    @Before
    public void createExampleData () throws IOException{
        File file = new File("./src/test/java/uk/ac/cam/cares/jps/agent/devinst/exampleRequests/exampleRequest.json");
        //for(String fileNames : file.list()) System.out.println(fileNames);
        //System.out.println(file.exists());
        exampleRequest = parseJSONFile("./src/test/java/uk/ac/cam/cares/jps/agent/devinst/exampleRequests/exampleRequest.json");
        exampleFind = parseJSONFile("./src/test/java/uk/ac/cam/cares/jps/agent/devinst/exampleRequests/exampleRequest_Find.json");
        exampleEmptyMap = parseJSONFile("./src/test/java/uk/ac/cam/cares/jps/agent/devinst/exampleRequests/exampleRequest_EmptyMapper.json");
        exampleAddQuery = parseJSONFile("./src/test/java/uk/ac/cam/cares/jps/agent/devinst/exampleRequests/exampleRequest_AdditionalQ.json");

    }

    @Ignore("Test containers requires docker to function")
    @Test
    public void testEmptyMapper () {
        try{
            queryBuilder.InsertDevice(exampleEmptyMap);
            Assert.fail("Device Instantiation succeed despite entpy IRI Mapper");
        }
        catch (Exception e) {
            Assert.assertTrue(e.toString().contains("Please provide either the IRI or the following keyword:"));
        }
        
        
    }

    @Ignore("Test containers requires docker to function")
    @Test
    public void testInstantiation () {
        queryBuilder.InsertDevice(exampleRequest);

        SelectQuery query = Queries.SELECT();
        //Has to have: Microcontroller
        query.where(query.var().isA(MicroController)).prefix(P_DEV);
        JSONArray result = storeClient.executeQuery(query.getQueryString());
        Assert.assertTrue(result.length() == 1);
        String obtainedIRI = result.getJSONObject(0).getString("x0");
        Assert.assertTrue(obtainedIRI.contains("ESP32"));

        //SmartSensor
        query = Queries.SELECT();
        query.where(query.var().isA(SmartSensor)).prefix(P_DEV);
        result = storeClient.executeQuery(query.getQueryString());
        Assert.assertTrue(result.length() == 1);
        obtainedIRI = result.getJSONObject(0).getString("x0");
        Assert.assertTrue(obtainedIRI.contains("ProximitySensor_FH-02"));


        //Sensor
        query = Queries.SELECT();
        query.where(query.var().isA(Sensor)).prefix(P_DEV, P_SAREF);
        result = storeClient.executeQuery(query.getQueryString());
        Assert.assertTrue(result.length() == 1);
        obtainedIRI = result.getJSONObject(0).getString("x0");
        Assert.assertTrue(obtainedIRI.contains("HCSR04") && !obtainedIRI.contains("HCSR04_ProximitySensor"));

        //Proximity Sensor
        query = Queries.SELECT();
        query.where(query.var().isA(ProximitySensor)).prefix(P_DEV);
        result = storeClient.executeQuery(query.getQueryString());
        Assert.assertTrue(result.length() == 1);
        obtainedIRI = result.getJSONObject(0).getString("x0");
        Assert.assertTrue(obtainedIRI.contains("HCSR04_ProximitySensor"));

        //Raw Readings
        query = Queries.SELECT();
        query.where(query.var().has(measures, query.var())).prefix(P_DEV);
        result = storeClient.executeQuery(query.getQueryString());
        Assert.assertTrue(result.length() == 1);
        obtainedIRI = result.getJSONObject(0).getString("x1");
        Assert.assertTrue(obtainedIRI.contains("AvgDist_FH02"));

    }

    @Ignore("Test containers requires docker to function")
    @Test
    public void testFindIRI(){
        //Add dummyraw var here
        ModifyQuery modify = Queries.MODIFY();
        String exampleRawString = "http://www.example.com/prefix/api_AvgDist_FH02";
        Iri exampleRaw = iri(exampleRawString);
        Iri Length = iri("http://www.ontology-of-units-of-measure.org/resource/om-2/Length");
        modify.insert(exampleRaw.isA(Length));
        storeClient.executeUpdate(modify.getQueryString());

        //Check if IRI is consistent
        queryBuilder.InsertDevice(exampleFind);

        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(measures, query.var())).prefix(P_DEV);
        JSONArray result = storeClient.executeQuery(query.getQueryString());
        Assert.assertTrue(result.length() == 1);
        String obtainedIRI = result.getJSONObject(0).getString("x1");
        //System.out.println(obtainedIRI);
        Assert.assertEquals(obtainedIRI, exampleRawString);
    }

    @Ignore("Test containers requires docker to function")
    @Test
    public void testAdditionalQuery() {
        queryBuilder.InsertDevice(exampleAddQuery);

        //Check all triples is instantiated from additional queries
        SelectQuery query = Queries.SELECT();
        TriplePattern triple = GraphPatterns.tp(iri("http://example.com/prefix/OPENLABAREA"), iri("https://w3id.org/bot#containsElement"), query.var());
        query.where(triple);
        JSONArray result = storeClient.executeQuery(query.getQueryString());
        Assert.assertEquals(2, result.length());

        Boolean hasProxSens = false;
        Boolean hasWFH = false;

        for(int i =0; i < 2; i++){
            String obtainedIRI = result.getJSONObject(i).getString("x0");
            if (obtainedIRI.contains("ProximitySensor_FH-02")){
                hasProxSens = true;
            }

            if (obtainedIRI.equals("http://example.com/prefix/WFH")){
                hasWFH = true;
            }
        }

        Assert.assertTrue(hasProxSens && hasWFH);

        query = Queries.SELECT();
        triple = GraphPatterns.tp(query.var(), iri("https://www.theworldavatar.com/kg/ontodevice/isAttachedTo"), iri("http://example.com/prefix/WFH"));
        query.where(triple);
        result = storeClient.executeQuery(query.getQueryString());
        String obtainedIRI = result.getJSONObject(0).getString("x0");
        Assert.assertTrue(obtainedIRI.contains("ProximitySensor_FH-02"));

        query = Queries.SELECT();
        query.where(query.var().isA(iri("http://www.ontology-of-units-of-measure.org/resource/om-2/Length")));
        result = storeClient.executeQuery(query.getQueryString());
        obtainedIRI = result.getJSONObject(1).getString("x0");
        //System.out.println(result);
        Assert.assertTrue(obtainedIRI.contains("http://example.com/prefix/testingGen"));

    }

    @After
	public void cleanUp() {
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
	}
    
}
