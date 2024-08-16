package com.cmclinnovations.stack.clients.geoserver;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockserver.model.HttpResponse.response;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.AutoClose;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockserver.model.HttpRequest;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import com.cmclinnovations.stack.clients.mocks.MockGeoServer;
import com.cmclinnovations.stack.clients.mocks.MockPostGIS;
import com.cmclinnovations.stack.clients.mocks.MockServiceWithFileSystem;

@Testcontainers
public class GeoServerClientTest {

    private static final String EXISTING_WORKSPACE = "existingWorkspace";
    private static final String NEW_WORKSPACE = "newWorkspace";
    private static final String EXISTING_COVERAGE_STORE = "existingCoverageStore";
    private static final String NEW_COVERAGE_STORE = "newCoverageStore";

    @AutoClose("stop")
    public static MockGeoServer mockGeoServer;
    @AutoClose
    public static MockPostGIS mockPostGIS;

    public static GeoServerClient geoServerClient;

    @Container
    static MockServiceWithFileSystem dummyGeoServerContainer = new MockServiceWithFileSystem("geoserver");

    @BeforeAll
    static void setup() throws MalformedURLException {
        mockPostGIS = new MockPostGIS();
        mockPostGIS.addOverride(postGISClientMock -> doNothing().when(postGISClientMock).createDatabase(anyString()));

        mockGeoServer = new MockGeoServer();
        geoServerClient = GeoServerClient.getInstance();
    }

    @BeforeEach
    void reset() {
        mockGeoServer.clear("");
    }

    @Test
    void testAddProjectionsToGeoserver() {
        HttpRequest reloadRequest = HttpRequest.request("/rest/reload")
                .withMethod("POST");
        mockGeoServer.when(reloadRequest).respond(response().withStatusCode(200));

        String wktString = "PROJCRS[\"OSGB36 / British National Grid\",BASEGEOGCRS[\"OSGB36\",DATUM[\"Ordnance Survey of Great Britain 1936\",ELLIPSOID[\"Airy 1830\",6377563.396,299.3249646,LENGTHUNIT[\"metre\",1]]],PRIMEM[\"Greenwich\",0,ANGLEUNIT[\"degree\",0.0174532925199433]],ID[\"EPSG\",4277]],CONVERSION[\"British National Grid\",METHOD[\"Transverse Mercator\",ID[\"EPSG\",9807]],PARAMETER[\"Latitude of natural origin\",49,ANGLEUNIT[\"degree\",0.0174532925199433],ID[\"EPSG\",8801]],PARAMETER[\"Longitude of natural origin\",-2,ANGLEUNIT[\"degree\",0.0174532925199433],ID[\"EPSG\",8802]],PARAMETER[\"Scale factor at natural origin\",0.9996012717,SCALEUNIT[\"unity\",1],ID[\"EPSG\",8805]],PARAMETER[\"False easting\",400000,LENGTHUNIT[\"metre\",1],ID[\"EPSG\",8806]],PARAMETER[\"False northing\",-100000,LENGTHUNIT[\"metre\",1],ID[\"EPSG\",8807]]],CS[Cartesian,2],AXIS[\"(E)\",east,ORDER[1],LENGTHUNIT[\"metre\",1]],AXIS[\"(N)\",north,ORDER[2],LENGTHUNIT[\"metre\",1]],USAGE[SCOPE[\"Engineering survey, topographic mapping.\"],AREA[\"United Kingdom (UK) - offshore to boundary of UKCS within 49°45'N to 61°N and 9°W to 2°E; onshore Great Britain (England, Wales and Scotland). Isle of Man onshore.\"],BBOX[49.75,-9.01,61.01,2.01]],ID[\"EPSG\",27700]]";
        String srid = "ESPG:27700";

        geoServerClient.addProjectionsToGeoserver(wktString, srid);

        String expectedFile = srid + "=" + wktString + "\n";
        String remoteFilePath = "/opt/geoserver_data/user_projections/epsg.properties";
        dummyGeoServerContainer.assertFileContent(expectedFile, remoteFilePath);

        mockGeoServer.verifyCalls(reloadRequest);
    }

    @Test
    void testCreateExistingGeoTiffLayer() {
        HttpRequest coverageStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + EXISTING_COVERAGE_STORE
                        + ".xml")
                .withMethod("GET");
        mockGeoServer.when(coverageStoreCheck).respond(response().withStatusCode(200));

        geoServerClient.createGeoTiffLayer(EXISTING_WORKSPACE, EXISTING_COVERAGE_STORE, "postgres", "public",
                new GeoServerRasterSettings(), new MultidimSettings());

        mockGeoServer.verifyCalls(coverageStoreCheck);
    }

    @Test
    void testCreateNewGeoTiffLayer() {
        HttpRequest coverageStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + NEW_COVERAGE_STORE
                        + ".xml")
                .withMethod("GET");
        mockGeoServer.when(coverageStoreCheck).respond(response().withStatusCode(404));

        HttpRequest addCoverageDataset = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + NEW_COVERAGE_STORE
                        + "/external.imagemosaic")
                .withMethod("PUT");
        mockGeoServer.when(addCoverageDataset)
                .respond(response().withStatusCode(200).withBody("<coverageStore></coverageStore>"));

        HttpRequest addCoverageStore = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + NEW_COVERAGE_STORE
                        + "/coverages.xml")
                .withMethod("POST");
        mockGeoServer.when(addCoverageStore).respond(response().withStatusCode(200));

        HttpRequest addRasterLayer = HttpRequest
                .request("/rest/layers/" + EXISTING_WORKSPACE + ":" + NEW_COVERAGE_STORE)
                .withMethod("PUT");
        mockGeoServer.when(addRasterLayer).respond(response().withStatusCode(200));

        try {
            geoServerClient.createGeoTiffLayer(EXISTING_WORKSPACE, NEW_COVERAGE_STORE, "postgres", "public",
                    new GeoServerRasterSettings(), new MultidimSettings());
        } catch (RuntimeException ex) {
        }

        mockGeoServer.verifyCalls(coverageStoreCheck, addCoverageDataset, addCoverageStore, addRasterLayer);
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
        mockGeoServer.when(workspaceCheck).respond(response().withStatusCode(200));

        geoServerClient.createWorkspace(EXISTING_WORKSPACE);

        mockGeoServer.verifyCalls(workspaceCheck);
    }

    @Test
    void testCreateNewWorkspace() {
        HttpRequest workspaceCheck = HttpRequest.request("/rest/workspaces/" + NEW_WORKSPACE + ".xml")
                .withMethod("GET");
        mockGeoServer.when(workspaceCheck).respond(response().withStatusCode(404));

        HttpRequest workspaceCreate = HttpRequest.request("/rest/workspaces").withMethod("POST")
                .withBody("<workspace><name>" + NEW_WORKSPACE + "</name></workspace>");
        mockGeoServer.when(workspaceCreate).respond(response().withStatusCode(200));

        geoServerClient.createWorkspace(NEW_WORKSPACE);

        mockGeoServer.verifyCalls(workspaceCheck, workspaceCreate);
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
        mockGeoServer.when(reloadRequest).respond(response().withStatusCode(200));

        geoServerClient.reload();

        mockGeoServer.verifyCalls(reloadRequest);
    }

    @Test
    void testRemoveNonExistingWorkspace() {
        HttpRequest workspaceCheck = HttpRequest.request("/rest/workspaces/" + NEW_WORKSPACE + ".xml")
                .withMethod("GET");
        mockGeoServer.when(workspaceCheck).respond(response().withStatusCode(404));

        geoServerClient.removeWorkspace(NEW_WORKSPACE);

        mockGeoServer.verifyCalls(workspaceCheck);
    }

    @Test
    void testRemoveExistingWorkspace() {
        HttpRequest workspaceCheck = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE + ".xml")
                .withMethod("GET");
        mockGeoServer.when(workspaceCheck).respond(response().withStatusCode(200));

        HttpRequest stylesDelete = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE + "/styles.xml")
                .withMethod("GET");
        mockGeoServer.when(stylesDelete).respond(response().withStatusCode(200));

        HttpRequest workspaceDelete = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE)
                .withMethod("DELETE");
        mockGeoServer.when(workspaceDelete).respond(response().withStatusCode(200));

        geoServerClient.removeWorkspace(EXISTING_WORKSPACE);

        mockGeoServer.verifyCalls(workspaceCheck, stylesDelete, workspaceDelete);
    }

    @Test
    void testRemoveExistingWorkspaceWithStyles() {
        HttpRequest workspaceCheck = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE + ".xml")
                .withMethod("GET");
        mockGeoServer.when(workspaceCheck).respond(response().withStatusCode(200));

        HttpRequest stylesDelete = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE + "/styles.xml")
                .withMethod("GET");
        Assertions.assertDoesNotThrow(() -> {
            try (InputStream stylesFile = GeoServerClientTest.class.getResourceAsStream("styles.xml");
                    BufferedReader reader = new BufferedReader(new InputStreamReader(stylesFile))) {
                String styles = reader.lines().collect(Collectors.joining("\n"));
                mockGeoServer.when(stylesDelete).respond(response().withStatusCode(200).withBody(styles));
            }
        });

        HttpRequest style1Delete = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE + "/styles/pophatch")
                .withMethod("DELETE");
        mockGeoServer.when(style1Delete).respond(response().withStatusCode(200));

        HttpRequest style2Delete = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE + "/styles/point")
                .withMethod("DELETE");
        mockGeoServer.when(style2Delete).respond(response().withStatusCode(200));

        HttpRequest style3Delete = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE + "/styles/population")
                .withMethod("DELETE");
        mockGeoServer.when(style3Delete).respond(response().withStatusCode(200));

        HttpRequest workspaceDelete = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE)
                .withMethod("DELETE");
        mockGeoServer.when(workspaceDelete).respond(response().withStatusCode(200));

        geoServerClient.removeWorkspace(EXISTING_WORKSPACE);

        mockGeoServer.verifyCalls(workspaceCheck, stylesDelete, style1Delete, style2Delete, style3Delete,
                workspaceDelete);
    }
}
