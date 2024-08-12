package com.cmclinnovations.stack.clients.geoserver;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockStatic;
import static org.mockserver.model.HttpResponse.response;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.time.Duration;
import java.util.HashMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockserver.integration.ClientAndServer;
import org.mockserver.model.HttpRequest;
import org.mockserver.model.RequestDefinition;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.startupcheck.IsRunningStartupCheckStrategy;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import com.cmclinnovations.stack.clients.core.ClientWithEndpoint;
import com.cmclinnovations.stack.clients.core.RESTEndpointConfig;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.DockerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;

@Testcontainers
public class GeoServerClientTest {

    private static final String EXISTING_WORKSPACE = "existingWorkspace";
    private static final String NEW_WORKSPACE = "newWorkspace";
    private static final String EXISTING_COVERAGE_STORE = "existingCoverageStore";
    private static final String NEW_COVERAGE_STORE = "newCoverageStore";

    private static GeoServerClient geoServerClient;

    private static ClientAndServer mockServer;

    public static final DockerImageName FAKE_GEOSERVER = DockerImageName.parse("ubuntu:22.04");

    private static URL restURL;

    private static MockedStatic<PostGISClient> postGISClientFactoryMock;

    @SuppressWarnings({ "null", "resource" })
    @Container
    static GenericContainer<?> dummyContainer = new GenericContainer<>(FAKE_GEOSERVER)
            .withCommand("tail -f /dev/null")
            .withCreateContainerCmdModifier(
                    cmd -> cmd.withName(StackClient.prependStackName("geoserver", "-"))
                            .withLabels(new HashMap<>(StackClient.getStackNameLabelMap())))
            .withStartupCheckStrategy(new IsRunningStartupCheckStrategy().withTimeout(Duration.ofSeconds(10)));

    @BeforeAll
    static void setup() throws MalformedURLException {
        startServer();
        writePostGISConfig();
        writeGeoServerConfig();
    }

    static void startServer() throws MalformedURLException {

        int port = ThreadLocalRandom.current().nextInt(50000, 55000);
        mockServer = new ClientAndServer(port);

        Assertions.assertTrue(mockServer.hasStarted(10, 2, TimeUnit.SECONDS));

        restURL = new URL("http://localhost:" + port);

    }

    static void writePostGISConfig() {

        PostGISEndpointConfig endpointConfig = new PostGISEndpointConfig("postgis", "test-postgis", "1234",
                "user", null);

        ClientWithEndpoint.writeEndpointConfig(endpointConfig);

        PostGISClient postGISClientMock = Mockito.spy(PostGISClient.getInstance());
        doNothing().when(postGISClientMock).createDatabase(anyString());

        postGISClientFactoryMock = mockStatic(PostGISClient.class);
        postGISClientFactoryMock.when(PostGISClient::getInstance).thenReturn(postGISClientMock);

    }

    static void writeGeoServerConfig() throws MalformedURLException {

        GeoServerClient.writeEndpointConfig(
                new RESTEndpointConfig("geoserver", restURL,
                        "user", null));

        geoServerClient = GeoServerClient.getInstance();

    }

    @BeforeEach
    void setUp(TestInfo testInfo) {
        mockServer.clear("");
    }

    @AfterAll
    public static void stopServer() {
        mockServer.stop();
    }

    @AfterAll
    public static void closeMocks() {
        if (null != postGISClientFactoryMock) {
            postGISClientFactoryMock.closeOnDemand();
        }
    }

    @Test
    void testAddProjectionsToGeoserver() {
        HttpRequest reloadRequest = HttpRequest.request("/rest/reload")
                .withMethod("POST");
        mockServer.when(reloadRequest).respond(response().withStatusCode(200));

        String wktString = "PROJCRS[\"OSGB36 / British National Grid\",BASEGEOGCRS[\"OSGB36\",DATUM[\"Ordnance Survey of Great Britain 1936\",ELLIPSOID[\"Airy 1830\",6377563.396,299.3249646,LENGTHUNIT[\"metre\",1]]],PRIMEM[\"Greenwich\",0,ANGLEUNIT[\"degree\",0.0174532925199433]],ID[\"EPSG\",4277]],CONVERSION[\"British National Grid\",METHOD[\"Transverse Mercator\",ID[\"EPSG\",9807]],PARAMETER[\"Latitude of natural origin\",49,ANGLEUNIT[\"degree\",0.0174532925199433],ID[\"EPSG\",8801]],PARAMETER[\"Longitude of natural origin\",-2,ANGLEUNIT[\"degree\",0.0174532925199433],ID[\"EPSG\",8802]],PARAMETER[\"Scale factor at natural origin\",0.9996012717,SCALEUNIT[\"unity\",1],ID[\"EPSG\",8805]],PARAMETER[\"False easting\",400000,LENGTHUNIT[\"metre\",1],ID[\"EPSG\",8806]],PARAMETER[\"False northing\",-100000,LENGTHUNIT[\"metre\",1],ID[\"EPSG\",8807]]],CS[Cartesian,2],AXIS[\"(E)\",east,ORDER[1],LENGTHUNIT[\"metre\",1]],AXIS[\"(N)\",north,ORDER[2],LENGTHUNIT[\"metre\",1]],USAGE[SCOPE[\"Engineering survey, topographic mapping.\"],AREA[\"United Kingdom (UK) - offshore to boundary of UKCS within 49°45'N to 61°N and 9°W to 2°E; onshore Great Britain (England, Wales and Scotland). Isle of Man onshore.\"],BBOX[49.75,-9.01,61.01,2.01]],ID[\"EPSG\",27700]]";
        String srid = "ESPG:27700";

        geoServerClient.addProjectionsToGeoserver(wktString, srid);

        String expectedFile = srid + "=" + wktString + "\n";
        String remoteFilePath = "/opt/geoserver_data/user_projections/epsg.properties";
        assertFileContentsInContainer(expectedFile, remoteFilePath);

        verifyCalls(reloadRequest);
    }

    private void assertFileContentsInContainer(String expectedFile, String remoteFilePath) {
        try {
            DockerClient dockerClient = DockerClient.getInstance();
            String actualFile = new String(dockerClient.retrieveFile(dummyContainer.getContainerId(),
                    remoteFilePath));

            Assertions.assertEquals(expectedFile, actualFile);
        } catch (IOException e) {
            Assertions.fail(e);
        }
    }

    @Test
    void testCreateExistingGeoTiffLayer() {
        HttpRequest coverageStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + EXISTING_COVERAGE_STORE
                        + ".xml")
                .withMethod("GET");
        mockServer.when(coverageStoreCheck).respond(response().withStatusCode(200));

        geoServerClient.createGeoTiffLayer(EXISTING_WORKSPACE, EXISTING_COVERAGE_STORE, "postgres", "public",
                new GeoServerRasterSettings(), new MultidimSettings());

        verifyCalls(coverageStoreCheck);
    }

    @Test
    void testCreateNewGeoTiffLayer() {
        HttpRequest coverageStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + NEW_COVERAGE_STORE
                        + ".xml")
                .withMethod("GET");
        mockServer.when(coverageStoreCheck).respond(response().withStatusCode(404));

        HttpRequest addCoverageDataset = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + NEW_COVERAGE_STORE
                        + "/external.imagemosaic")
                .withMethod("PUT");
        mockServer.when(addCoverageDataset)
                .respond(response().withStatusCode(200).withBody("<coverageStore></coverageStore>"));

        HttpRequest addCoverageStore = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + NEW_COVERAGE_STORE
                        + "/coverages.xml")
                .withMethod("POST");
        mockServer.when(addCoverageStore).respond(response().withStatusCode(200));

        HttpRequest addRasterLayer = HttpRequest
                .request("/rest/layers/" + EXISTING_WORKSPACE + ":" + NEW_COVERAGE_STORE)
                .withMethod("PUT");
        mockServer.when(addRasterLayer).respond(response().withStatusCode(200));

        try {
            geoServerClient.createGeoTiffLayer(EXISTING_WORKSPACE, NEW_COVERAGE_STORE, "postgres", "public",
                    new GeoServerRasterSettings(), new MultidimSettings());
        } catch (RuntimeException ex) {
        }

        verifyCalls(coverageStoreCheck, addCoverageDataset, addCoverageStore, addRasterLayer);
    }

    @Test
    @Disabled("Needs to be implemented.")
    void testCreatePostGISDataStore() {

    }

    @Test
    @Disabled("Needs to be implemented.")
    void testCreatePostGISLayer() {

    }

    @Test
    void testCreateExistingWorkspace() {
        HttpRequest workspaceCheck = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE + ".xml")
                .withMethod("GET");
        mockServer.when(workspaceCheck).respond(response().withStatusCode(200));

        geoServerClient.createWorkspace(EXISTING_WORKSPACE);

        verifyCalls(workspaceCheck);
    }

    @Test
    void testCreateNewWorkspace() {
        HttpRequest workspaceCheck = HttpRequest.request("/rest/workspaces/" + NEW_WORKSPACE + ".xml")
                .withMethod("GET");
        mockServer.when(workspaceCheck).respond(response().withStatusCode(404));

        HttpRequest workspaceCreate = HttpRequest.request("/rest/workspaces").withMethod("POST")
                .withBody("<workspace><name>" + NEW_WORKSPACE + "</name></workspace>");
        mockServer.when(workspaceCreate).respond(response().withStatusCode(200));

        geoServerClient.createWorkspace(NEW_WORKSPACE);

        verifyCalls(workspaceCheck, workspaceCreate);
    }

    @Test
    void testGetInstance() {

        GeoServerClient tempClient1 = GeoServerClient.getInstance();
        Assertions.assertNotNull(tempClient1);

        GeoServerClient tempClient2 = GeoServerClient.getInstance();
        Assertions.assertNotNull(tempClient2);

        Assertions.assertSame(tempClient1, tempClient2);
    }

    @Test
    @Disabled("Needs to be implemented.")
    void testLoadIcons() {

    }

    @Test
    @Disabled("Needs to be implemented.")
    void testLoadOtherFiles() {

    }

    @Test
    @Disabled("Needs to be implemented.")
    void testLoadStyle() {

    }

    @Test
    void testReload() {
        HttpRequest reloadRequest = HttpRequest.request("/rest/reload")
                .withMethod("POST");
        mockServer.when(reloadRequest).respond(response().withStatusCode(200));

        geoServerClient.reload();

        verifyCalls(reloadRequest);
    }

    @Test
    @Disabled("Needs to be implemented.")
    void testRemoveWorkspace() {

    }

    private static void verifyCalls(RequestDefinition... expectations) throws AssertionError {

        HttpRequest[] recordedRequests = mockServer.retrieveRecordedRequests(null);

        Assertions.assertAll(Stream.concat(
                Stream.of(() -> Assertions.assertEquals(expectations.length, recordedRequests.length,
                        "Wrong number of calls.")),
                Stream.of(expectations)
                        .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()))
                        .entrySet().stream()
                        .map(entry -> () -> Assertions.assertEquals(entry.getValue(),
                                mockServer.retrieveRecordedRequests(entry.getKey()).length,
                                "Wrong number of calls with the following definition: '"
                                        + entry.getKey().toString()))));

    }
}
