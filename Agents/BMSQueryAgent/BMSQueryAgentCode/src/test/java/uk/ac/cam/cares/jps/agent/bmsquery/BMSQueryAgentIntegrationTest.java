package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.FileEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.io.*;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

@Ignore ("Requires triple store endpoint set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
@Testcontainers
public class BMSQueryAgentIntegrationTest {
    @Container
    private static final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);

    //temp folder for temp namespace properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private BMSQueryAgentLauncher launcher;
    private BMSQueryAgent agent;
    private static CloseableHttpClient httpClient;

    @BeforeClass
    public static void setUpBeforeClass() {
        try {
            blazegraph.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }
    }

    @Before
    public void setUp() throws IOException {
        httpClient = HttpClients.createDefault();
        createNewNameSpace();

        launcher = new BMSQueryAgentLauncher();

        RemoteStoreClient rsClient = new RemoteStoreClient(getBlazegraphEndPoint(), getBlazegraphEndPoint());
        agent = new BMSQueryAgent();
        agent.setRSClient(rsClient, Arrays.asList(getNamespaceUrl("bms"), getNamespaceUrl("lab")));

//        postNewData("lab", "");
    }


    @After
    public void tearDown() throws IOException {
        for (String namespace : Arrays.asList("bms", "lab")) {
            clearBlazegraphData(namespace);
        }
    }

    @AfterClass
    public static void tearDownAfterClass() throws IOException {
        if (blazegraph.isRunning()) {
            blazegraph.stop();
        }
        httpClient.close();
    }

    @Test
    public void testQueryAllZones_Empty() {
        JSONObject result = agent.queryAllZones();
        assertTrue(result.getJSONObject("buildings").keySet().isEmpty());
    }

    @Test
    public void testQueryAllZones() throws IOException {
        createNewData("lab", "testQueryAllZones.xml");
        JSONObject result = agent.queryAllZones();

        JSONObject buildings = result.getJSONObject("buildings");
        assertEquals(2, buildings.keySet().size());

        JSONObject building = buildings.getJSONObject("http://www.example.com/MockBuilding_1");
        assertEquals("Mock Building 1", building.getString("label"));

        JSONObject building2 = buildings.getJSONObject("http://www.example.com/MockBuilding_2");
        assertEquals("Mock Building 2", building2.getString("label"));

        JSONObject facilities = building.getJSONObject("facilities");
        assertEquals(2, facilities.keySet().size());

        JSONObject lab1 = facilities.getJSONObject("https://www.example.com/MockLabOne");
        assertEquals("Mock Lab 1", lab1.getString("label"));

        JSONObject lab1Rooms = lab1.getJSONObject("rooms");
        assertEquals(2, lab1Rooms.keySet().size());

        JSONObject lab2 = facilities.getJSONObject("https://www.example.com/MockLabTwo");
        assertEquals(0, lab2.getJSONObject("rooms").keySet().size());
        assertEquals("Mock Lab 2", lab2.getString("label"));

        JSONObject openLabArea = lab1Rooms.getJSONObject("http://www.example.com/Room_1");
        assertEquals("Room 1", openLabArea.getString("label"));

        JSONObject powderProcessingRoom = lab1Rooms.getJSONObject("http://www.example.com/Room_2");
        assertEquals("Room 2", powderProcessingRoom.getString("label"));
    }

    @Test
    public void testQueryEquipmentInstances() throws IOException {
        createNewData("lab", "testQueryEquipment.xml");

        JSONObject result = agent.queryEquipmentInstances("http://www.example.com/Room_1");
        assertEquals(2, result.getJSONArray("Equipments").length());
    }

    @Test
    public void testQueryEquipmentInstances_Empty() throws IOException {
        createNewData("lab", "testQueryEquipment_Empty.xml");

        JSONObject result = agent.queryEquipmentInstances("http://www.example.com/Room_1");
        assertTrue(result.getJSONArray("Equipments").isEmpty());
    }

    private String getBlazegraphEndPoint() {
        return "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort() +"/blazegraph";
    }

    private String getNamespaceUrl(String namespace) {
        return getBlazegraphEndPoint() + "/namespace/" + namespace + "/sparql";
    }

    private String prepareBlazegraphPropertiesFile(String namespace) throws IOException {
        List<String> labPropertiesFile = readNamespacePropertiesTemplateFile(namespace);
        String blazegraphProperty = Paths.get(folder.getRoot().toString(), namespace + "namespace_properties.xml").toString();
        writeToTempFolder(blazegraphProperty, labPropertiesFile);
        return blazegraphProperty;
    }

    private void createNewNameSpace() throws IOException {
        String labPropertyFile = prepareBlazegraphPropertiesFile("lab");
        String bmsPropertyFile = prepareBlazegraphPropertiesFile("bms");

        HttpPost httpPostLab = new HttpPost(getBlazegraphEndPoint() + "/namespace");
        httpPostLab.setEntity(new FileEntity(new File(labPropertyFile), ContentType.APPLICATION_XML));
        httpPostLab.setHeader("Content-Type", "application/xml");

        HttpPost httpPostBMS = new HttpPost(getBlazegraphEndPoint() + "/namespace");
        httpPostBMS.setEntity(new FileEntity(new File(bmsPropertyFile), ContentType.APPLICATION_XML));
        httpPostBMS.setHeader("Content-Type", "application/xml");

        try (CloseableHttpResponse ignored = httpClient.execute(httpPostLab)) {
            try (CloseableHttpResponse ignored2 = httpClient.execute(httpPostBMS)) {;}
        }
    }

    private void createNewData(String namespace, String fileName) throws IOException {
        String path = "src/test/resources/";
        File file = new File(path + fileName);
        String filePath = file.getAbsolutePath();

        HttpPost addData = new HttpPost(getNamespaceUrl(namespace));
        addData.setEntity(new FileEntity(new File(filePath), ContentType.create("application/rdf+xml")));
        addData.setHeader("Content-Type", "application/rdf+xml");
        try (CloseableHttpResponse ignored = httpClient.execute(addData)) {;}
    }

    private void clearBlazegraphData(String namespace) throws IOException {
        HttpDelete deleteNamespace = new HttpDelete(getNamespaceUrl(namespace));
        deleteNamespace.setHeader("Accept", "application/xml");
        System.out.println(deleteNamespace);
        try (CloseableHttpResponse ignored = httpClient.execute(deleteNamespace)) {;}
    }

    private void writeToTempFolder(String filepath, List<String> properties) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }

    private List<String> readNamespacePropertiesTemplateFile(String namespace) {
        String path = "src/test/resources/";

        File file = new File(path + "newNamespacePropertiesFileTemplate.xml");
        String templatePath = file.getAbsolutePath();

        List<String> results = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(templatePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.contains("[NAMESPACE]")) {
                    line = line.replace("[NAMESPACE]", namespace);
                }
                results.add(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        return results;
    }

}
