package uk.ac.cam.cares.jps.agent.assetmanager;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.apache.commons.io.IOUtils;

import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import java.io.*;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

/*
 * Parent class for testing asset manager agent endpoints
 * Sets up the agent, blazegraph, environment variables and config files before running the tests and cleanup after
 */


public class EndpointTest {
    // Temporary folder to place a properties file
    String folderReqBody = "./src/test/java/uk/ac/cam/cares/jps/agent/assetmanager/request_body";
    String folderSPARQL = "./src/test/java/uk/ac/cam/cares/jps/agent/assetmanager/sparql";

    @Container
	public GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
													 .withExposedPorts(9999);

    public AssetManagerAgent agent;
    public Map<String, RemoteStoreClient> storeClientMap = new HashMap<String, RemoteStoreClient>();

    //Blazegraph endpoint
    private HashMap<String, String> namespaceMap = new HashMap<String, String>();
    private String[] namespaceList = {"asset", "office", "purchasedocs", "lab", "bms"};


    private static String reqBody = "com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n"+
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
            String copyReqBody = reqBody;
            CloseableHttpClient httpclient = HttpClients.createDefault();
            HttpPost postRequest = new HttpPost(builder.build());
            postRequest.setEntity(new StringEntity(copyReqBody.replace("NAMESPACE", namespace), ContentType.DEFAULT_TEXT));
            CloseableHttpResponse response = httpclient.execute(postRequest);

            //For some reason the getFirstHeader/ getLastHeader returns null
		    //sparql_endpoint = response.getFirstHeader("Location").getValue();
            namespaceMap.put(namespace, new URIBuilder().setScheme("http").setHost(blazegraph.getHost()).setPort(blazegraph.getFirstMappedPort()).setPath("/blazegraph/namespace/"+namespace+"/sparql").toString());
        }

        //Set the RemoteStoreClient
        for (Map.Entry<String, String> set  : namespaceMap.entrySet()){
            String ns = set.getKey();
            String endpoint = set.getValue();
            storeClientMap.put(ns, new RemoteStoreClient(endpoint, endpoint));
        }

		RemoteStoreClient officeStore = storeClientMap.get("office");
        String officeRoomABox = new String(Files.readAllBytes(Paths.get(folderSPARQL+"/init_room_office.rq")));
        officeStore.executeUpdate(officeRoomABox);
        RemoteStoreClient labStore = storeClientMap.get("lab");
        String labRoomABox = new String(Files.readAllBytes(Paths.get(folderSPARQL+"/init_room_lab.rq")));
        labStore.executeUpdate(labRoomABox);
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
                String agentProperties = System.getenv("AGENTPROPERTIES");
                String ontoMapProperties = System.getenv("ONTOMAPPROPERTIES");
                String FOLDER_QR = System.getenv("FOLDERQR");
                String FOLDER_MANUAL = System.getenv("FOLDERMANUAL");
                String TSSEARCH = System.getenv("TSSEARCH");
                System.out.print(agentProperties+ "::" + 
                    ontoMapProperties+ "::"  + 
                    FOLDER_QR+ "::"  + 
                    FOLDER_MANUAL+ "::"  +
                    TSSEARCH
                );
                agent = new AssetManagerAgent();
        	 });
        }
        // There should not be any exception thrown as the agent is initiated correctly
        catch (Exception e) {
            System.out.println(e);
            throw new IOException(e);
        }
 
        
    }

  
    @Before
    public void importJSONfiles () throws Exception{
        try {
            //Import any request body by overloading here
            return;

        }catch (Exception e) {
            throw new Exception("Test Failed when importing request body from file:: " + e);
        }
    }
        

    static JSONObject parseJSONFile(String filepath) throws Exception {
        File f = new File(filepath);
        if (f.exists()){
            InputStream is = new FileInputStream(filepath);
            String jsonTxt = IOUtils.toString(is, "UTF-8");
            JSONObject json = new JSONObject(jsonTxt);
            return json;
        }
        throw new Exception("JSON file for testing does not exist: " + filepath);
    }

    public static JSONObject parseJSON(String filename) throws JSONException, IOException {
        String content = new String(Files.readAllBytes(Paths.get(filename))).replace("\n", " ");
        return new JSONObject(content);
    }

    @After
	public void cleanUp() {
		if (blazegraph.isRunning()) {
			blazegraph.stop();
		}
	}
    
}