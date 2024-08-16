package com.cmclinnovations.stack.clients.geoserver;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockserver.model.HttpResponse.response;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
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
import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.core.type.TypeReference;

@Testcontainers
public class GeoServerClientTest {

    private static final String SCHEMA_NAME = "public";
    private static final String DATABASE_NAME = "postgres";
    private static final String EXISTING_WORKSPACE = "existingWorkspace";
    private static final String NEW_WORKSPACE = "newWorkspace";
    private static final String EXISTING_COVERAGE_STORE = "existingCoverageStore";
    private static final String NEW_COVERAGE_STORE = "newCoverageStore";
    private static final String NEW_POSTGIS_STORE = "newPostgresStore";

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
    void testCreateGeoTiffLayerExisting() {
        HttpRequest coverageStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + EXISTING_COVERAGE_STORE
                        + ".xml")
                .withMethod("GET");
        mockGeoServer.when(coverageStoreCheck).respond(response().withStatusCode(200));

        geoServerClient.createGeoTiffLayer(EXISTING_WORKSPACE, EXISTING_COVERAGE_STORE, DATABASE_NAME, SCHEMA_NAME,
                new GeoServerRasterSettings(), new MultidimSettings());

        mockGeoServer.verifyCalls(coverageStoreCheck);
    }

    @Test
    void testCreateGeoTiffLayerNew() {
        HttpRequest coverageStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + NEW_COVERAGE_STORE + ".xml")
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
            geoServerClient.createGeoTiffLayer(EXISTING_WORKSPACE, NEW_COVERAGE_STORE, DATABASE_NAME, SCHEMA_NAME,
                    new GeoServerRasterSettings(), new MultidimSettings());
        } catch (RuntimeException ex) {
        }

        mockGeoServer.verifyCalls(coverageStoreCheck, addCoverageDataset, addCoverageStore, addRasterLayer);
    }

    @Test
    void testCreatePostGISDataStoreExisting() {
        HttpRequest postgresStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + NEW_POSTGIS_STORE + ".xml")
                .withMethod("GET");
        mockGeoServer.when(postgresStoreCheck).respond(response().withStatusCode(200));

        geoServerClient.createPostGISDataStore(EXISTING_WORKSPACE, NEW_POSTGIS_STORE, DATABASE_NAME, SCHEMA_NAME);

        mockGeoServer.verifyCalls(postgresStoreCheck);
    }

    @Test
    void testCreatePostGISDataStoreNew() {

        HttpRequest postgresStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + NEW_POSTGIS_STORE + ".xml")
                .withMethod("GET");
        mockGeoServer.when(postgresStoreCheck).respond(response().withStatusCode(404));

        HttpRequest postgresStoreCreate = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores.xml")
                .withMethod("POST");
        mockGeoServer.when(postgresStoreCreate).respond(response().withStatusCode(200));

        geoServerClient.createPostGISDataStore(EXISTING_WORKSPACE, NEW_POSTGIS_STORE, DATABASE_NAME, SCHEMA_NAME);

        mockGeoServer.verifyCalls(postgresStoreCheck, postgresStoreCreate);
    }

    @Test
    void testCreatePostGISLayerExisting() {
        HttpRequest postgresStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + DATABASE_NAME + ".xml")
                .withMethod("GET");
        mockGeoServer.when(postgresStoreCheck).respond(response().withStatusCode(200));

        String layerName = "layerName";
        HttpRequest postgresLayerCheck = HttpRequest
                .request("/rest/layers/" + EXISTING_WORKSPACE + ":" + layerName + ".xml")
                .withMethod("GET");
        mockGeoServer.when(postgresLayerCheck).respond(response().withStatusCode(200));

        geoServerClient.createPostGISLayer(EXISTING_WORKSPACE, DATABASE_NAME, layerName,
                new GeoServerVectorSettings());

        mockGeoServer.verifyCalls(postgresStoreCheck, postgresLayerCheck);
    }

    @Test
    void testCreatePostGISLayerExistingStore() {
        HttpRequest postgresStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + DATABASE_NAME + ".xml")
                .withMethod("GET");
        mockGeoServer.when(postgresStoreCheck).respond(response().withStatusCode(200));

        String layerName = "layerName";
        HttpRequest postgresLayerCheck = HttpRequest
                .request("/rest/layers/" + EXISTING_WORKSPACE + ":" + layerName + ".xml")
                .withMethod("GET");
        mockGeoServer.when(postgresLayerCheck).respond(response().withStatusCode(404));

        HttpRequest postgresLayerCreate = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + DATABASE_NAME + "/featuretypes")
                .withMethod("POST");
        mockGeoServer.when(postgresLayerCreate).respond(response().withStatusCode(200));

        HttpRequest postgresLayerConfigure = HttpRequest.request("/rest/layers/" + EXISTING_WORKSPACE + ":" + layerName)
                .withMethod("PUT");
        mockGeoServer.when(postgresLayerConfigure).respond(response().withStatusCode(200));

        geoServerClient.createPostGISLayer(EXISTING_WORKSPACE, DATABASE_NAME, layerName,
                new GeoServerVectorSettings());

        mockGeoServer.verifyCalls(postgresStoreCheck, postgresLayerCheck, postgresLayerCreate,
                postgresLayerConfigure);
    }

    @Test
    void testCreatePostGISLayerNew() {
        HttpRequest postgresStoreCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + DATABASE_NAME + ".xml")
                .withMethod("GET");
        mockGeoServer.when(postgresStoreCheck).respond(response().withStatusCode(404));

        HttpRequest postgresStoreCreate = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores.xml")
                .withMethod("POST");
        mockGeoServer.when(postgresStoreCreate).respond(response().withStatusCode(200));

        String layerName = "layerName";
        HttpRequest postgresLayerCheck = HttpRequest
                .request("/rest/layers/" + EXISTING_WORKSPACE + ":" + layerName + ".xml")
                .withMethod("GET");
        mockGeoServer.when(postgresLayerCheck).respond(response().withStatusCode(404));

        HttpRequest postgresLayerCreate = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + DATABASE_NAME + "/featuretypes")
                .withMethod("POST");
        mockGeoServer.when(postgresLayerCreate).respond(response().withStatusCode(200));

        HttpRequest postgresLayerConfigure = HttpRequest.request("/rest/layers/" + EXISTING_WORKSPACE + ":" + layerName)
                .withMethod("PUT");
        mockGeoServer.when(postgresLayerConfigure).respond(response().withStatusCode(200));

        geoServerClient.createPostGISLayer(EXISTING_WORKSPACE, DATABASE_NAME, layerName,
                new GeoServerVectorSettings());

        mockGeoServer.verifyCalls(postgresStoreCheck, postgresStoreCreate, postgresLayerCheck, postgresLayerCreate,
                postgresLayerConfigure);
    }

    @Test
    void testCreateWorkspaceExisting() {
        HttpRequest workspaceCheck = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE + ".xml")
                .withMethod("GET");
        mockGeoServer.when(workspaceCheck).respond(response().withStatusCode(200));

        geoServerClient.createWorkspace(EXISTING_WORKSPACE);

        mockGeoServer.verifyCalls(workspaceCheck);
    }

    @Test
    void testCreateWorkspaceNew() {
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
    void testLoadIcons() {
        Path iconPath = Assertions
                .assertDoesNotThrow(() -> Path.of(GeoServerClientTest.class.getResource("icons/icon1.gif").toURI()));
        Path baseDirectory = iconPath.getParent().getParent();

        geoServerClient.loadIcons(baseDirectory, "icons");

        byte[] allBytes = Assertions.assertDoesNotThrow(() -> Files.readAllBytes(iconPath));
        dummyGeoServerContainer.assertFileContent(allBytes, "/opt/geoserver_data/www/icons/icon1.gif");
    }

    @Test
    @Disabled("Disabled until fixed in PR #1300")
    void testLoadOtherFilesGood() throws IOException {

        try (InputStream is = GeoServerClientTest.class.getResourceAsStream("otherFiles.json")) {
            Path baseDirectory = Assertions
                    .assertDoesNotThrow(() -> Path.of(GeoServerClientTest.class.getResource("otherFiles").toURI()));
            List<GeoserverOtherStaticFile> otherFiles = Assertions
                    .assertDoesNotThrow(() -> JsonHelper.getMapper().readValue(is,
                            new TypeReference<List<GeoserverOtherStaticFile>>() {
                            }));

            geoServerClient.loadOtherFiles(baseDirectory, otherFiles);

            String staticDir = "/opt/geoserver_data/www/static_data/";
            Map<String, String> expected = Map.of(
                    "a2.txt", "a1",
                    "a3/a4.txt", "a1",
                    "d1/c1.txt", "c1",
                    "d2/c1.txt", "c1",
                    "d1/d1.txt", "d1",
                    "d2/d1.txt", "d1",
                    "d3", "d1",
                    "d4/d5.txt", "d1");

            dummyGeoServerContainer.assertDirContent(expected, staticDir);
        }
    }

    @Test
    void testLoadStyleExisting() throws IOException {
        String styleName = "styleName";
        HttpRequest styleCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/styles/" + styleName + ".xml")
                .withMethod("GET");
        mockGeoServer.when(styleCheck).respond(response().withStatusCode(200));

        String fileName = "styleFile.sld";
        Assertions.assertDoesNotThrow(() -> geoServerClient
                .loadStyle(JsonHelper.getMapper().readValue(
                        "{\"name\":\"" + styleName + "\", \"file\":\"" + fileName + "\" }",
                        GeoServerStyle.class), EXISTING_WORKSPACE));

        mockGeoServer.verifyCalls(styleCheck);
    }

    @Test
    void testLoadStyleNew() throws IOException {
        String styleName = "styleName";
        HttpRequest styleCheck = HttpRequest
                .request("/rest/workspaces/" + EXISTING_WORKSPACE + "/styles/" + styleName + ".xml")
                .withMethod("GET");
        mockGeoServer.when(styleCheck).respond(response().withStatusCode(404));

        HttpRequest postStyle = HttpRequest.request("/rest/workspaces/" + EXISTING_WORKSPACE + "/styles")
                // .withPathParameter("name", styleName)
                .withMethod("POST");
        mockGeoServer.when(postStyle).respond(response().withStatusCode(200));

        Path configDir = null;
        Path stylePath = null;
        try (InputStream styleIn = GeoServerClientTest.class.getResourceAsStream("point_simplepoint.sld")) {
            configDir = Assertions
                    .assertDoesNotThrow(() -> Files.createDirectories(Path.of("/inputs/config")));

            stylePath = configDir.resolve("styleFile.sld");
            Files.copy(styleIn, stylePath);

            Path fileName = stylePath.getFileName();
            Assertions.assertDoesNotThrow(() -> geoServerClient
                    .loadStyle(JsonHelper.getMapper().readValue(
                            "{\"name\":\"" + styleName + "\", \"file\":\"" + fileName + "\" }",
                            GeoServerStyle.class), EXISTING_WORKSPACE));
        } finally {
            if (null != stylePath) {
                Files.deleteIfExists(stylePath);
            }
            if (null != configDir) {
                Files.deleteIfExists(configDir);
                Files.deleteIfExists(configDir.getParent());
            }
        }
        mockGeoServer.verifyCalls(styleCheck, postStyle);
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
    void testRemoveWorkspaceNonExisting() {
        HttpRequest workspaceCheck = HttpRequest.request("/rest/workspaces/" + NEW_WORKSPACE + ".xml")
                .withMethod("GET");
        mockGeoServer.when(workspaceCheck).respond(response().withStatusCode(404));

        geoServerClient.removeWorkspace(NEW_WORKSPACE);

        mockGeoServer.verifyCalls(workspaceCheck);
    }

    @Test
    void testRemoveWorkspaceExisting() {
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
    void testRemoveWorkspaceExistingWithStyles() {
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
