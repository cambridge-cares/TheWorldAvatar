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


public class DevInstAgentLauncherTest {
    RemoteStoreClient storeClient;
    String sparql_endpoint = "http://host.docker.internal:9999/blazegraph/namespace/dev_inst/sparql";
    DevInstAgentLauncher testLauncher;
    JSONObject exampleRequest = new JSONObject();
    public static final String KEY_DESCRIPTOR = "Descriptor";
	public static final String KEY_MICROCONTROLLER = "MicroController";
	public static final String KEY_IRIMAPPER = "IRIMapper";
	public static final String KEY_ADDITRIONALQUERY = "AdditionalQuery";
    public static final String KEY_CLIENTPROPERTY = "CLIENTPROPERTIES";
    File mappingFolder;


    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
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
    public void initializeLauncher() throws IOException {
        // Create a properties file that points to a dummy mapping folder //
        // Create an empty folder
        String folderName = "mappings";
        mappingFolder = folder.newFolder(folderName);

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
                testLauncher = new DevInstAgentLauncher();
        	 });
        }
        // There should not be any exception thrown as the agent is initiated correctly
        catch (Exception e) {
            System.out.println(e);
            throw new IOException(e);
        }

        
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
        JSONObject desc = parseJSONFile("./src/test/java/uk/ac/cam/cares/jps/agent/devinst/exampleRequests/exampleRequest.json");
        exampleRequest.put(KEY_CLIENTPROPERTY, KEY_CLIENTPROPERTY);
        exampleRequest.put(KEY_DESCRIPTOR, desc);
    }

    @Test
    public void testValidate (){
        try {
            SystemLambda.withEnvironmentVariable(KEY_CLIENTPROPERTY, mappingFolder.getCanonicalPath()).execute(() -> {
                Assert.assertTrue(testLauncher.validateInput(exampleRequest));
            });
        }
        // There should not be any exception thrown as the agent is initiated correctly
        catch (Exception e) {
            System.out.println(e);
        }
    }

    @Test
    public void testValidateEmpty (){
        Assert.assertFalse(testLauncher.validateInput(new JSONObject()));
    }

    @Test
    public void testValidateMissingClientProp (){
        JSONObject exampleMissingClientProp = exampleRequest;
        exampleMissingClientProp.remove(KEY_CLIENTPROPERTY);
        Assert.assertFalse(testLauncher.validateInput(exampleMissingClientProp));
    }

    @Test
    public void testValidateMissingDescriptor (){
        JSONObject exampleMissingDesc = exampleRequest;
        exampleMissingDesc.remove(KEY_DESCRIPTOR);
        Assert.assertFalse(testLauncher.validateInput(exampleMissingDesc));
    }

}


