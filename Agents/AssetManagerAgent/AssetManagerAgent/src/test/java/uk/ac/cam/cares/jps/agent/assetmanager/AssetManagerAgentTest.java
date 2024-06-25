package uk.ac.cam.cares.jps.agent.assetmanager;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.jena.base.Sys;
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
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.isA;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.*;
import org.junit.experimental.runners.Enclosed;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.semanticweb.owlapi.util.IRIComparator;
import org.apache.commons.io.IOUtils;

import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
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
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;

public class AssetManagerAgentTest {
    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    private static String folderReqBody = "./src/test/java/uk/ac/cam/cares/jps/agent/assetmanager/request_body";

    @Container
	private GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
													 .withExposedPorts(9999);

    private static AssetManagerAgent agent;
    Map<String, RemoteStoreClient> storeClientMap = new HashMap<String, RemoteStoreClient>();
    
    JSONObject exampleRequest;
    JSONObject exampleFind;
    JSONObject exampleEmptyMap;
    JSONObject exampleAddQuery;

    //Blazegraph endpoint
    HashMap<String, String> namespaceMap = new HashMap<String, String>();
    String[] namespaceList = {"asset", "office", "purchasedocs", "lab", "bms"};


    private String reqBody = "com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n"+
    "com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms\r\n"+
    "com.bigdata.rdf.sail.isolatableIndices=false\r\n"+
    "com.bigdata.rdf.sail.truthMaintenance=false\r\n"+
    "com.bigdata.rdf.store.AbstractTripleStore.justify=false\r\n"+
    "com.bigdata.rdf.sail.namespace=NAMESPACE\r\n"+
    "com.bigdata.namespace.NAMESPACE.spo.com.bigdata.btree.BTree.branchingFactor=1024\r\n"+
    "com.bigdata.rdf.store.AbstractTripleStore.quads=false\r\n"+
    "com.bigdata.namespace.NAMESPACE.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n"+
    "com.bigdata.journal.Journal.groupCommit=false\r\n"+
    "com.bigdata.rdf.store.AbstractTripleStore.geoSpatial=false\r\n"+
    "com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false";

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
        String folderName = "config";
        File mappingFolder, folderManual, folderQR;
        mappingFolder = folder.newFolder(folderName);
        folderManual = folder.newFolder("manuals");
        folderQR = folder.newFolder("folderQR");
        //Test fail when the propfile/folders/env var does not exist
        //Test fail when only password or username is provided as KG credntial


        // Filepath for the properties file
        
        String agentPropFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        String ontoMapPropFile = Paths.get(folder.getRoot().toString(), "ontologyMap.properties").toString();
        String tsSearchPropFile = Paths.get(folder.getRoot().toString(), "tsSearch.properties").toString();
        
        String folderManualFilepath = Paths.get(folder.getRoot().toString(), "manuals/").toString();
        String folderQRFilepath = Paths.get(folder.getRoot().toString(), "folderQR/").toString();

        List<String> clientPropParam = new ArrayList<String>();
        for (Map.Entry<String, String> set  : namespaceMap.entrySet()){
            String ns = set.getKey();
            String endpoint = set.getValue();
            clientPropParam.add("endpoint.kg."+ns+"="+endpoint);
        }
        clientPropParam.add("auth.kg.user=");
        clientPropParam.add("auth.kg.pass=");
        //The variable/URLs below does not actually work. To be mocked
        clientPropParam.add("endpoint.printer=http://host.docker.internal:5000/print");
        clientPropParam.add("target_qr_size=4.");
        clientPropParam.add("url.manual=https://www.theworldavatar.com:1010/careslab/manuals/");
        clientPropParam.add("url.docupload=http://host.docker.internal:1016/document-upload-agent/adddocument");

        String[] ontoMapPropParam = {"LabEquipment=https://www.theworldavatar.com/kg/ontoassetmanagement/LabEquipment",
            "Laptop=https://www.theworldavatar.com/kg/ontoassetmanagement/Laptop",
            "Monitor=https://www.theworldavatar.com/kg/ontoassetmanagement/Monitor",
            "OtherIT=https://www.theworldavatar.com/kg/ontoassetmanagement/OtherIT",
            "Printer=https://www.theworldavatar.com/kg/ontoassetmanagement/Printer",
            "Workstation=https://www.theworldavatar.com/kg/ontoassetmanagement/Workstation"};
        String[] tsSearchPropParam = {"depth =" + String.valueOf(5),
            "predicate=https://www.theworldavatar.com/kg/ontotimeseries/hasTimeSeries,"+
             "https://saref.etsi.org/core/hasState,"+
             "https://www.theworldavatar.com/kg/ontobms/hasLowLevelState,"+
             "https://www.theworldavatar.com/kg/ontobms/hasMediumLevelState,"+
             "https://www.theworldavatar.com/kg/ontobms/hasHighLevelState,"+
             "https://www.theworldavatar.com/kg/ontodevice/sendsSignalTo,"+
             "https://saref.etsi.org/core/consistsOf,"+
             "https://www.theworldavatar.com/kg/ontodevice/measures,"+
             "http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue,"+
             "https://www.theworldavatar.com/kg/ontodevice/observes" };
        writePropertyFile(agentPropFile, clientPropParam);
        writePropertyFile(ontoMapPropFile, Arrays.asList(ontoMapPropParam));
        writePropertyFile(tsSearchPropFile, Arrays.asList(tsSearchPropParam));

        //Set the RemoteStoreClient
        for (Map.Entry<String, String> set  : namespaceMap.entrySet()){
            String ns = set.getKey();
            String endpoint = set.getValue();
            storeClientMap.put(ns, new RemoteStoreClient(endpoint, endpoint));
        }
        
        // To create testAgent without an exception being thrown, SystemLambda is used to mock an environment variable
        // To mock the environment variable, a try catch need to be used
        /*ENV AGENTPROPERTIES "/root/agent.properties"
        ENV ONTOMAPPROPERTIES "/root/ontologyMap.properties"
        ENV TSSEARCH "/root/tsSearch.properties"
        ENV FOLDERQR "/root/folderQR/"
        ENV FOLDERMANUAL "/root/manuals/"*/
        try {
        	SystemLambda.withEnvironmentVariable("AGENTPROPERTIES", agentPropFile)
                .and("ONTOMAPPROPERTIES", ontoMapPropFile)
                .and("TSSEARCH", tsSearchPropFile)
                .and("FOLDERQR", folderQRFilepath)
                .and("FOLDERMANUAL", folderManualFilepath)
                .execute(() -> {
                agent = new AssetManagerAgent();
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
        //exampleRequest.put(KEY_CLIENTPROPERTY, KEY_CLIENTPROPERTY);
        //exampleRequest.put(KEY_DESCRIPTOR, desc);

    }

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

        // create a new namespace (endpoint) on blazegraph 
        for (String namespace : namespaceList){
            CloseableHttpClient httpclient = HttpClients.createDefault();
            HttpPost postRequest = new HttpPost(builder.build());
            postRequest.setEntity(new StringEntity(reqBody, ContentType.DEFAULT_TEXT));
            CloseableHttpResponse response = httpclient.execute(postRequest);

            //For some reason the getFirstHeader/ getLastHeader returns null
		    //sparql_endpoint = response.getFirstHeader("Location").getValue();
            namespaceMap.put(namespace, new URIBuilder().setScheme("http").setHost(blazegraph.getHost()).setPort(blazegraph.getFirstMappedPort()).setPath("/blazegraph/namespace/"+namespace+"/sparql").toString());
        }
		
    }

      /*
     * Functionality tested:
    *       "/retrieve", 
            "/retrievebydocs", 
            "/getuidata", 
            "/instantiate",
            "/addmaintenance",
            "/updatetime",
            "/deletemaintenance",
            "/delete"
        Functionality not tested (relies on other agent / printer):
            "/print", 
            "/printbulk",
            "/addmanual",
            "/addassetimage",
            "/addpurchdocfile"
            
     */

    @RunWith(Enclosed.class)
    public static class TestInstantiate {
        /*Test fail when content is invalid - singular asset
            - Missing mandatory field
            - ID invalid or already exist
            - AssetClass not in list
            - key Room empty/ room does not exist when FacilityLocation in CARES premise      
        //Test fail when content is invalid - set assets
            - No setData key
            * NOTE: The request content of each assets does not need to be retested as internally
            it called the same methods for singular assets - as long as the singular asset methods are tested first
        //Test succeed - singular asset and set assets
            - in Cares Office / Cares Lab / out of Cares facility
            - with/without ID provided
            - with/without existing IRIs
            - 
        */
        
        //read example test data
        static JSONObject missingMandatory, invalidID, noRoomKey, invalidAssetClass, roomNotRecorded;
        static JSONObject defaultBody, existingIRI, inLab, noID, outOfCARES;
        
        @Before
        void importJSONfiles () throws Exception{
            try {
                missingMandatory = parseJSON(folderReqBody  + "/init/fail/init_missingMandatory.json");
                invalidID = parseJSON(folderReqBody  + "/init/fail/init_invalidID.json");
                noRoomKey = parseJSON(folderReqBody  + "/init/fail/init_NoRoomKey.json");
                invalidAssetClass = parseJSON(folderReqBody  + "/init/fail/init_invalidAssetClass.json");
                roomNotRecorded = parseJSON(folderReqBody  + "/init/fail/init_roomDoesNotExist.json");

                defaultBody = parseJSON(folderReqBody  + "/init/success/init_default.json");
                existingIRI = parseJSON(folderReqBody  + "/init/success/init_existingIRI.json");
                inLab = parseJSON(folderReqBody  + "/init/success/init_inLab.json");
                noID = parseJSON(folderReqBody  + "/init/success/init_NoID.json");
                outOfCARES = parseJSON(folderReqBody  + "/init/success/init_outOfCARES.json");

            }catch (Exception e) {
                throw new Exception("Test Failed when importing request body from file:: " + e);
            }
        }

        //Test calling agents
        @Ignore("Reuiqres docker installed")
        @Test
        public static void testMandatoryField() { 
            assertTrue(agent.getRequestParameters(missingMandatory, "/instantiate").getJSONArray("Result").getString(0).contains("The asset class keys cannot be retrieved from the properties file:"));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        public static void testinvalidID() { 
            assertTrue(agent.getRequestParameters(invalidID, "/instantiate").getJSONArray("Result").getString(0).contains("Asset data is invalid."));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        public static void testRoomsAndLocationsError() { 
            assertTrue(agent.getRequestParameters(roomNotRecorded, "/instantiate").getJSONArray("Result").getString(0).contains("Instantiation failed"));
            assertTrue(agent.getRequestParameters(noRoomKey, "/instantiate").getJSONArray("Result").getString(0).contains("Instantiation failed"));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        public static void testDefaultBody() { 
            JSONObject result = agent.getRequestParameters(defaultBody, "/instantiate");
            assertTrue(result.getJSONArray("Result").getString(1).contains("Command Success"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("ID").equals("2023-10-27/6"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("deviceIRI").contains("https://www.theworldavatar.com/kg/ontodevice/"));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        public static void testExistingIRI() { 
            JSONObject result = agent.getRequestParameters(existingIRI, "/instantiate");
            assertTrue(result.getJSONArray("Result").getString(1).contains("Command Success"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("ID").equals("2023-10-27/4"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("deviceIRI").equals("https://www.theworldavatar.com/kg/ontodevice/Device_fdaa745d-c447-4a90-ab13-b00e0d95675c"));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        public static void testLocationOutOfCARES() { 
            JSONObject result = agent.getRequestParameters(outOfCARES, "/instantiate");
            assertTrue(result.getJSONArray("Result").getString(1).contains("Command Success"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("ID").equals("2023-10-27/7"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("deviceIRI").contains("https://www.theworldavatar.com/kg/ontodevice/"));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        public static void testNoIDProvided() { 
            JSONObject result = agent.getRequestParameters(noID, "/instantiate");
            assertTrue(result.getJSONArray("Result").getString(1).contains("Command Success"));
            //assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("ID").equals("2024-03-10/8")); // Assuming the test order follows the order it is written it should be 8.
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("ID").contains("2024-03-10"));
            assertTrue(result.getJSONArray("Result").getJSONObject(0).getString("deviceIRI").contains("https://www.theworldavatar.com/kg/ontodevice/"));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        public static void testAssetAlreadyExist () throws Exception {
            //Assumes the other unit tests succeed
            defaultBody.put("ID", "2023-10-27/1000");
            JSONObject debug = agent.getRequestParameters(defaultBody, "/instantiate");
            if(debug.getJSONArray("Result").getString(1).contains("Command Success")){
                JSONObject result = agent.getRequestParameters(defaultBody, "/instantiate");
                //assertTrue(result.getJSONArray("Result").getString(0).contains("Instance already exist for id:"));
                assertTrue(result.getJSONArray("Result").getString(0).contains("Instantiation failed:"));
            }
            else{
                throw new Exception("Failed to create duplicate for instantiate duplicate test" );
            }
        }
    }
    
    @RunWith(Enclosed.class)
    public static class TestRetrieve {
        /*Test fail when content is invalid
            - ID is invalid IRI/ID
            - ID does not exist

        //Test succeed
            - ID using assetID or IRI
            - Asset in either lab or office namespace
            - Check response, ensure valid content
            - with/ without timeseries - todo
        */

        //read example test data
        static JSONObject doesNotExist, invalid;
        static JSONObject inLab, justID, justIRI;
        
        @Before
        void importJSONfiles () throws Exception{
            try {
                doesNotExist = parseJSON(folderReqBody  + "/retrieve/fail/retrieve_doesNotExist.json");
                invalid = parseJSON(folderReqBody  + "/retrieve/fail/retrieve_invalid.json");
                
                justID = parseJSON(folderReqBody  + "/retrieve/success/retrieve_justID.json");
                justIRI = parseJSON(folderReqBody  + "/retrieve/success/retrieve_justIRI.json");
                inLab = parseJSON(folderReqBody  + "/retrieve/success/retrieve_inLab.json");
                
            } catch (Exception e) {
                throw new Exception("Test Failed when importing request body from file:: " + e);
            }
        }

        @Ignore("Reuiqres docker installed")
        @Test
        static void testRetrieveDoesNotExist () {
            JSONObject result = agent.getRequestParameters(doesNotExist, "/retrieve");
            assertTrue(result.getJSONArray("Result").getString(0).contains("Failed to get ID/IRI."));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        static void testRetrieveInvalidID () {
            JSONObject result = agent.getRequestParameters(invalid, "/retrieve");
            assertTrue(result.getJSONArray("Result").getString(0).contains("Failed to get ID/IRI."));
        }

        //The following tests are run after the instantiate tests
        @Ignore("Reuiqres docker installed")
        @Test
        static void testRetrieveJustID () {
            JSONObject result = agent.getRequestParameters(justID, "/retrieve");
            assertTrue(result.getJSONArray("ID").getString(0).equals("2023-10-27/4"));
            assertTrue(result.getJSONArray("Result").length() == 3);
        }

        @Ignore("Reuiqres docker installed")
        @Test
        static void testRetrieveJustIRI () {
            JSONObject result = agent.getRequestParameters(justIRI, "/retrieve");
            assertTrue(result.getJSONArray("ID").getString(1).equals("https://www.theworldavatar.com/kg/ontodevice/Device_fdaa745d-c447-4a90-ab13-b00e0d95675c"));
            assertTrue(result.getJSONArray("Result").length() == 3);
        }
    
    }

    @Ignore("Reuiqres docker installed")
    @Test
    public void TestRetrieveUIData() {
         /*Test succeed
            - Just call it, should work regardless
        */
        JSONObject param = new JSONObject("{\"assetData\": {}}");
        JSONObject result = agent.getRequestParameters(param, "/getuidata");
        assertTrue(result.has("result"));
        String[] keyArr = {"Type", "User", "Invoice", "Element", "Manufacturer", "PurchaseOrder", "Supplier", "DeliveryOrder", "Workspace"};
        for (String key : keyArr) {
            assertTrue(result.getJSONObject("result").has(key));
        }
        
    }

    @RunWith(Enclosed.class)
    public static class TestMaintenanceManagement {
         /*Test fail when content is invalid
            - Add maintenance:
                - ID does not exist / invalid
                - ID is an IRI (only accepts asset ID)
                - next service time after last service time
                - last service time in the future
                - negative interval or 0 month interval
                - no service provider
            - update maintenance time:
                SHOULD NOT FAIL unless the other feature failed
            - delete maintenenance schedule:
                - IRI does not exist

        //Test succeed
            - Add maintenance:
                - One time maintenance - with either last service or next schedule
                - Regular maintenance - with next service schedule
                - Regular maintenance - without next service schedule, with interval
                - Multiple maintenance sched on a single asset
            - update maintenance time:
                SHOULD NOT FAIL unless the other feature failed
            - delete maintenenance schedule:
                - Just run on an existing maintenance schedule IRI, should remove the all associated triples
        */

        //read example test data
        static JSONObject doesNotExist, invalidID, lastInFuture, nextBeforeLast, nonPositiveInterval, noServiceProvider, useIRI;
        static JSONObject addOneTime, regular, regularNoNext;
        
        @Before
        void importJSONfiles () throws Exception{
            try {
                doesNotExist = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_delete_idDoesNotExist.json");
                invalidID = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_invalidID.json");
                lastInFuture = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_lastInFuture.json");
                nextBeforeLast = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_nextBeforeLast.json");
                nonPositiveInterval = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_nonPositiveInterval.json");
                noServiceProvider = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_noServiceProvider.json");
                useIRI = parseJSON(folderReqBody  + "/maintenance/fail/maintenance_add_useIRI.json");
                
                addOneTime = parseJSON(folderReqBody  + "/maintenance/success/maintenance_add_oneTime.json");
                regular = parseJSON(folderReqBody  + "/maintenance/success/maintenance_add_regular.json");
                regularNoNext = parseJSON(folderReqBody  + "/maintenance/success/maintenance_add_regular_withoutNext.json");
                
            } catch (Exception e) {
                throw new Exception("Test Failed when importing request body from file:: " + e);
            }
        }

        @Ignore("Reuiqres docker installed")
        @Test
        static void testInvalidAssetID() {
            JSONObject result = agent.getRequestParameters(invalidID, "/addmaintenance");
            assertTrue(result.getJSONArray("Result").getString(0).contains("Maintenance data is invalid."));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        static void testInvalidMaintenanceTiming() {
            lastInFuture.put("LastService", LocalDate.now().plusDays(1).toString());
            JSONObject resultLastInFuture = agent.getRequestParameters(lastInFuture, "/addmaintenance");
            assertTrue(resultLastInFuture.getJSONArray("Result").getString(0).contains("Last service date cannot be in the future"));

            JSONObject resultNextBeforeLast = agent.getRequestParameters(nextBeforeLast, "/addmaintenance");
            assertTrue(resultNextBeforeLast.getJSONArray("Result").getString(0).contains("Next service date is before last service date"));

            JSONObject resultNonPositiveInterval = agent.getRequestParameters(nonPositiveInterval, "/addmaintenance");
            assertTrue(resultNonPositiveInterval.getJSONArray("Result").getString(0).contains("Interval has to be > 0"));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        static void testNoServiceProvider() {
            JSONObject result = agent.getRequestParameters(noServiceProvider, "/addmaintenance");
            assertTrue(result.getJSONArray("Result").getString(0).contains("Maintenance data is invalid."));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        static void testAddMaintenance() {
            JSONObject result = agent.getRequestParameters(addOneTime, "/addmaintenance");
            result = agent.getRequestParameters(regular, "/addmaintenance");
            result = agent.getRequestParameters(regularNoNext, "/addmaintenance");

            //TODO check here if instances are created correctly
            JSONArray allMaintenanceData = agent.instanceHandler.assetRetriever.getAllMaintenanceScheduleIRI();
            assertTrue(allMaintenanceData.length() > 0);
            //Check triple data here
        }

        @Ignore("Reuiqres docker installed")
        @Test
        static void testUpdateMaintenance() {
            JSONObject result = agent.getRequestParameters(new JSONObject("{\"assetData\": {}}"), "/updatetime");

            //TODO check if time instances are updated correctly
        }



        @Ignore("Reuiqres docker installed")
        @Test
        static void testAssetDoesNotExist() {
            JSONObject result = agent.getRequestParameters(doesNotExist, "/addmaintenance");
            assertTrue(result.getJSONArray("Result").getString(0).contains("Device is unregistered for ID"));

            result = agent.getRequestParameters(doesNotExist, "/deletemaintenance");
            assertTrue(result.getJSONArray("Result").getString(0).contains("Device is unregistered for ID"));
        }

        @Ignore("Reuiqres docker installed")
        @Test
        static void testDeleteMaintenance() {
            //Get IRI of the maintenance data and call delete on all
            JSONArray maintenanceList = agent.instanceHandler.assetRetriever.getAllMaintenanceScheduleIRI();
            for (int i = 0; i < maintenanceList.length(); i++) {
                String maintenanceScheduleIRI = maintenanceList.getJSONObject(i).getString("maintenanceScheduleIRI");
                agent.getRequestParameters(new JSONObject("{\"assetData\": {\"ID\" : "+maintenanceScheduleIRI+"}}"), "/deletemaintenance");
            }

            maintenanceList = agent.instanceHandler.assetRetriever.getAllMaintenanceScheduleIRI();
            assertTrue(maintenanceList.length() == 0);

            //TODO check here if instances are deleted correctly
        }

    }

    @RunWith(Enclosed.class)
    public static class TestDelete {
        /*Test fail when content is invalid
            - ID is invalid IRI/ID
            - ID does not exist

        //Test succeed
            - ID using assetID or IRI
            - Asset in either lab or office namespace
        */
        
        @Ignore("Reuiqres docker installed")
        @Test
        static void deleteInvalidID() {
            JSONObject invalidID = new JSONObject("{\"assetData\":{\"ID\": \"duck\"}}");
            JSONObject result = agent.getRequestParameters(invalidID, "/delete");
            assertTrue(result.getJSONArray("Result").getString(0).contains("Failed to get ID/IRI."));
        
            JSONObject notExistID = new JSONObject("{\"assetData\":{\"ID\": \"2023-10-27/6000\"}}");
            result = agent.getRequestParameters(notExistID, "/delete");
            assertTrue(result.getJSONArray("Result").getString(0).contains("Failed to get ID/IRI."));
        }

        @Ignore("Reuiqres docker installed")
        static void deleteSuccess() {
            //All asset ID created assuming its done in the order the tests were written:
            //2023-10-27/6, 2023-10-27/4, 2023-10-27/7, 2024-03-10/8, 2023-10-27/1000
            String[] idArr = {"2023-10-27/6", "2023-10-27/4", "2023-10-27/7", "2024-03-10/8", "2023-10-27/1000", "https://www.theworldavatar.com/kg/ontodevice/Device_fdaa745d-c447-4a90-ab13-b00e0d95675c"};
            for (String id: idArr ){
                agent.getRequestParameters(new JSONObject("{\"assetData\": {\"ID\" : "+id+"}}"), "/deletemaintenance");
            }
        }

    }

    private static JSONObject parseJSON (String filepath) throws Exception {
        File f = new File(filepath);
        if (f.exists()){
            InputStream is = new FileInputStream(filepath);
            String jsonTxt = IOUtils.toString(is, "UTF-8");
            JSONObject json = new JSONObject(jsonTxt);
            return json;
        }
        throw new Exception("JSON file for testing does not exist: " + filepath);
    }

    @After
	public void cleanUp() {
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
	}
    
}