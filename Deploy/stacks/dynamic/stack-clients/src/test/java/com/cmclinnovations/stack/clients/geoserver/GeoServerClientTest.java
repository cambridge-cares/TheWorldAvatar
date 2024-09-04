package com.cmclinnovations.stack.clients.geoserver;

import static com.cmclinnovations.stack.clients.mocks.MockHTTPService.Method.DELETE;
import static com.cmclinnovations.stack.clients.mocks.MockHTTPService.Method.GET;
import static com.cmclinnovations.stack.clients.mocks.MockHTTPService.Method.POST;
import static com.cmclinnovations.stack.clients.mocks.MockHTTPService.Method.PUT;
import static org.junit.jupiter.params.provider.Arguments.arguments;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockserver.model.HttpRequest.request;
import static org.mockserver.model.HttpResponse.response;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.AutoClose;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockserver.model.MediaType;
import org.mockserver.model.XmlBody;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import com.cmclinnovations.stack.clients.mocks.MockGeoServer;
import com.cmclinnovations.stack.clients.mocks.MockPostGIS;
import com.cmclinnovations.stack.clients.mocks.MockServiceWithFileSystem;
import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

@Testcontainers
public class GeoServerClientTest {

    private static final String RASTER_SCHEMA_NAME = "existing_raster_schema";
    private static final String VECTOR_SCHEMA_NAME = "existing_vector_schema";
    private static final String DATABASE_NAME = "existing_database";
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

    static ObjectMapper jsonMapper = JsonHelper.getMapper();

    @Container
    static MockServiceWithFileSystem dummyGeoServerContainer = new MockServiceWithFileSystem("geoserver");

    @BeforeAll
    static void setup() throws MalformedURLException {
        mockPostGIS = new MockPostGIS();
        mockPostGIS.addOverride(postGISClientMock -> doNothing().when(postGISClientMock).createDatabase(anyString()));
        mockPostGIS.addOverride(
                postGISClientMock -> doNothing().when(postGISClientMock).createSchema(anyString(), anyString()));

        mockGeoServer = new MockGeoServer();
        geoServerClient = GeoServerClient.getInstance();
    }

    @BeforeEach
    void reset() {
        mockGeoServer.reset();
    }

    @AfterEach
    void verifyCalls() {
        mockGeoServer.verifyCalls();
    }

    @Test
    void testAddProjectionsToGeoserver() {
        mockGeoServer.addExpectation(POST, "/rest/reload", 200);

        String wktString = "PROJCRS[\"OSGB36 / British National Grid\",BASEGEOGCRS[\"OSGB36\",DATUM[\"Ordnance Survey of Great Britain 1936\",ELLIPSOID[\"Airy 1830\",6377563.396,299.3249646,LENGTHUNIT[\"metre\",1]]],PRIMEM[\"Greenwich\",0,ANGLEUNIT[\"degree\",0.0174532925199433]],ID[\"EPSG\",4277]],CONVERSION[\"British National Grid\",METHOD[\"Transverse Mercator\",ID[\"EPSG\",9807]],PARAMETER[\"Latitude of natural origin\",49,ANGLEUNIT[\"degree\",0.0174532925199433],ID[\"EPSG\",8801]],PARAMETER[\"Longitude of natural origin\",-2,ANGLEUNIT[\"degree\",0.0174532925199433],ID[\"EPSG\",8802]],PARAMETER[\"Scale factor at natural origin\",0.9996012717,SCALEUNIT[\"unity\",1],ID[\"EPSG\",8805]],PARAMETER[\"False easting\",400000,LENGTHUNIT[\"metre\",1],ID[\"EPSG\",8806]],PARAMETER[\"False northing\",-100000,LENGTHUNIT[\"metre\",1],ID[\"EPSG\",8807]]],CS[Cartesian,2],AXIS[\"(E)\",east,ORDER[1],LENGTHUNIT[\"metre\",1]],AXIS[\"(N)\",north,ORDER[2],LENGTHUNIT[\"metre\",1]],USAGE[SCOPE[\"Engineering survey, topographic mapping.\"],AREA[\"United Kingdom (UK) - offshore to boundary of UKCS within 49°45'N to 61°N and 9°W to 2°E; onshore Great Britain (England, Wales and Scotland). Isle of Man onshore.\"],BBOX[49.75,-9.01,61.01,2.01]],ID[\"EPSG\",27700]]";
        String srid = "ESPG:27700";

        geoServerClient.addProjectionsToGeoserver(wktString, srid);

        String expectedFile = srid + "=" + wktString + "\n";
        String remoteFilePath = "/opt/geoserver_data/user_projections/epsg.properties";
        dummyGeoServerContainer.assertFileContent(expectedFile, remoteFilePath);
    }

    @Test
    void testCreateGeoTiffLayerExisting() {
        mockGeoServer.addExpectation(GET,
                "/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + EXISTING_COVERAGE_STORE + ".xml",
                200);

        geoServerClient.createGeoTiffLayer(EXISTING_WORKSPACE, EXISTING_COVERAGE_STORE, DATABASE_NAME,
                RASTER_SCHEMA_NAME, new GeoServerRasterSettings(), new MultidimSettings());
    }

    @Test
    void testCreateGeoTiffLayerNew() {
        mockGeoServer.addExpectation(GET,
                "/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + NEW_COVERAGE_STORE + ".xml",
                404);

        mockGeoServer.addExpectation(PUT,
                "/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + NEW_COVERAGE_STORE
                        + "/external.imagemosaic",
                200,
                request().withBody(Path.of("/geotiffs/existing_database/existing_raster_schema/newCoverageStore").toFile()
                        .toURI().toString()),
                response().withBody("<coverageStore></coverageStore>"));

        mockGeoServer.addExpectation(POST,
                "/rest/workspaces/" + EXISTING_WORKSPACE + "/coveragestores/" + NEW_COVERAGE_STORE + "/coverages.xml",
                200, request().withBody(
                        "<coverage><enabled>true</enabled><metadata /><keywords /><metadataLinks /><supportedFormats /><parameters><entry><string>AllowMultithreading</string><string>true</string></entry></parameters><name>newCoverageStore</name></coverage>"));

        mockGeoServer.addExpectation(PUT, "/rest/layers/" + EXISTING_WORKSPACE + ":" + NEW_COVERAGE_STORE, 200,
                request().withBody(
                        "<layer><enabled>true</enabled><styles /><authorityURLs /><identifiers /><metadata><entry key=\"advertised\">true</entry></metadata></layer>"));

        geoServerClient.createGeoTiffLayer(EXISTING_WORKSPACE, NEW_COVERAGE_STORE, DATABASE_NAME, RASTER_SCHEMA_NAME,
                new GeoServerRasterSettings(), new MultidimSettings());
    }

    @Test
    void testCreatePostGISDataStoreExisting() {
        mockGeoServer.addExpectation(GET,
                "/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + NEW_POSTGIS_STORE + ".xml", 200);

        geoServerClient.createPostGISDataStore(EXISTING_WORKSPACE, NEW_POSTGIS_STORE, DATABASE_NAME,
                VECTOR_SCHEMA_NAME);
    }

    @Test
    void testCreatePostGISDataStoreNew() {

        mockGeoServer.addExpectation(GET,
                "/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + NEW_POSTGIS_STORE + ".xml", 404);

        mockGeoServer.addExpectation(POST, "/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores.xml", 200,
                request().withBody(
                        "<dataStore><name>newPostgresStore</name><connectionParameters><entry key=\"dbtype\">postgis</entry><entry key=\"min connections\">1</entry><entry key=\"max connections\">10</entry><entry key=\"fetch size\">1000</entry><entry key=\"Connection timeout\">20</entry><entry key=\"Loose bbox\">true</entry><entry key=\"preparedStatements\">false</entry><entry key=\"Max open prepared statements\">50</entry><entry key=\"Estimated extends\">false</entry><entry key=\"host\">test-postgis</entry><entry key=\"port\">1234</entry><entry key=\"user\">user</entry><entry key=\"passwd\" /><entry key=\"database\">existing_database</entry><entry key=\"schema\">existing_vector_schema</entry><entry key=\"validate connections\">true</entry></connectionParameters><type>PostGIS</type></dataStore>"));

        geoServerClient.createPostGISDataStore(EXISTING_WORKSPACE, NEW_POSTGIS_STORE, DATABASE_NAME,
                VECTOR_SCHEMA_NAME);
    }

    @Test
    void testCreatePostGISLayerExisting() {

        String layerName = "layerName";
        mockGeoServer.addExpectation(GET, "/rest/layers/" + EXISTING_WORKSPACE + ":" + layerName + ".xml", 200);

        geoServerClient.createPostGISLayer(EXISTING_WORKSPACE, DATABASE_NAME, VECTOR_SCHEMA_NAME, layerName,
                new GeoServerVectorSettings());
    }

    @Test
    void testCreatePostGISLayerExistingStore() {
        String layerName = "layerName";
        mockGeoServer.addExpectation(GET, "/rest/layers/" + EXISTING_WORKSPACE + ":" + layerName + ".xml", 404);

        mockGeoServer.addExpectation(GET,
                "/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + DATABASE_NAME + ".xml", 200);

        mockGeoServer.addExpectation(POST,
                "/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + DATABASE_NAME + "/featuretypes", 200,
                request().withBody(XmlBody.xml(
                        "<featureType><enabled>true</enabled><metadata /><keywords /><metadataLinks /><attributes /><projectionPolicy>NONE</projectionPolicy><title>layerName</title><name>layerName</name></featureType>",
                        MediaType.TEXT_XML)));

        mockGeoServer.addExpectation(PUT, "/rest/layers/" + EXISTING_WORKSPACE + ":" + layerName, 200,
                request().withBody(
                        "<layer><enabled>true</enabled><styles /><authorityURLs /><identifiers /><metadata><entry key=\"advertised\">true</entry></metadata></layer>"));

        geoServerClient.createPostGISLayer(EXISTING_WORKSPACE, DATABASE_NAME, VECTOR_SCHEMA_NAME, layerName,
                new GeoServerVectorSettings());
    }

    private static Stream<Arguments> geoServerVectorSettingsProvider() throws IOException {
        Path dirPath = Path.of(Assertions.assertDoesNotThrow(
                () -> GeoServerClientTest.class.getResource("geoServerVectorSettings").toURI()));
        try (Stream<Path> files = Files.list(dirPath)) {
            List<Path> jsonFiles = files.filter(file -> file.getFileName().toString().endsWith(".json"))
                    .collect(Collectors.toList());
            return jsonFiles.stream().map(jsonFile -> arguments(
                    jsonFile.getFileName().toString(),
                    readGeoServerVectorSettings(jsonFile),
                    getExpectedXmlBody(jsonFile, "ft"),
                    getExpectedXmlBody(jsonFile, "cl")));
        }

    }

    private static GeoServerVectorSettings readGeoServerVectorSettings(Path jsonFile) {
        return Assertions
                .assertDoesNotThrow(() -> jsonMapper.readValue(jsonFile.toFile(), GeoServerVectorSettings.class));
    }

    private static final Pattern flattenXmlRegex = Pattern.compile("\r?\n?^[ \t]*", Pattern.MULTILINE);

    private static XmlBody getExpectedXmlBody(Path jsonFile, String suffix) {
        return XmlBody.xml(Assertions.assertDoesNotThrow(() -> {
            return flattenXmlRegex.matcher(Files.readString(jsonFile
                    .resolveSibling(jsonFile.getFileName().toString().replace(".json", "_" + suffix + ".xml"))))
                    .replaceAll("");
        }), MediaType.TEXT_XML);
    }

    @ParameterizedTest(name = "{index} {0}")
    @MethodSource("geoServerVectorSettingsProvider")
    void testCreatePostGISLayerNew(String testName, GeoServerVectorSettings geoServerVectorSettings,
            XmlBody featureTypesBody,
            XmlBody createLayerBody) throws IOException {

        mockGeoServer.addExpectation(POST, "/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores.xml", 200,
                request().withBody(
                        "<dataStore><name>existing_database</name><connectionParameters><entry key=\"dbtype\">postgis</entry><entry key=\"min connections\">1</entry><entry key=\"max connections\">10</entry><entry key=\"fetch size\">1000</entry><entry key=\"Connection timeout\">20</entry><entry key=\"Loose bbox\">true</entry><entry key=\"preparedStatements\">false</entry><entry key=\"Max open prepared statements\">50</entry><entry key=\"Estimated extends\">false</entry><entry key=\"host\">test-postgis</entry><entry key=\"port\">1234</entry><entry key=\"user\">user</entry><entry key=\"passwd\" /><entry key=\"database\">existing_database</entry><entry key=\"schema\">existing_vector_schema</entry><entry key=\"validate connections\">true</entry></connectionParameters><type>PostGIS</type></dataStore>"));

        mockGeoServer.addExpectation(GET,
                "/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + DATABASE_NAME + ".xml", 404);

        String layerName = "layerName";
        mockGeoServer.addExpectation(GET, "/rest/layers/" + EXISTING_WORKSPACE + ":" + layerName + ".xml", 404);

        mockGeoServer.addExpectation(POST,
                "/rest/workspaces/" + EXISTING_WORKSPACE + "/datastores/" + DATABASE_NAME + "/featuretypes",
                200, request().withBody(featureTypesBody));

        mockGeoServer.addExpectation(PUT, "/rest/layers/" + EXISTING_WORKSPACE + ":" + layerName, 200,
                request().withBody(createLayerBody));

        UpdatedGSVirtualTableEncoder virtualTable;
        String sql;
        if (null != (virtualTable = geoServerVectorSettings.getVirtualTable())
                && null != (sql = virtualTable.getSql())
                && sql.startsWith("@")) {
            moveConfigFileAndRun(sql.replaceAll(".*/", ""), filePath -> geoServerClient
                    .createPostGISLayer(EXISTING_WORKSPACE, DATABASE_NAME, VECTOR_SCHEMA_NAME, layerName,
                            geoServerVectorSettings));
        } else {
            geoServerClient.createPostGISLayer(EXISTING_WORKSPACE, DATABASE_NAME, VECTOR_SCHEMA_NAME, layerName,
                    geoServerVectorSettings);
        }
    }

    @Test
    void testCreateWorkspaceExisting() {
        mockGeoServer.addExpectation(GET, "/rest/workspaces/" + EXISTING_WORKSPACE + ".xml", 200);

        geoServerClient.createWorkspace(EXISTING_WORKSPACE);
    }

    @Test
    void testCreateWorkspaceNew() {
        mockGeoServer.addExpectation(GET, "/rest/workspaces/" + NEW_WORKSPACE + ".xml", 404);

        mockGeoServer.addExpectation(POST, "/rest/workspaces", 200, request()
                .withBody("<workspace><name>" + NEW_WORKSPACE + "</name></workspace>"));

        geoServerClient.createWorkspace(NEW_WORKSPACE);
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
        mockGeoServer.addExpectation(GET, "/rest/workspaces/" + EXISTING_WORKSPACE + "/styles/" + styleName + ".xml",
                200);

        String fileName = "styleFile.sld";
        Assertions.assertDoesNotThrow(() -> geoServerClient
                .loadStyle(JsonHelper.getMapper().readValue(
                        "{\"name\":\"" + styleName + "\", \"file\":\"" + fileName + "\" }",
                        GeoServerStyle.class), EXISTING_WORKSPACE));
    }

    @Test
    void testLoadStyleNew() throws IOException {
        String styleName = "styleName";
        mockGeoServer.addExpectation(GET, "/rest/workspaces/" + EXISTING_WORKSPACE + "/styles/" + styleName + ".xml",
                404);

        moveConfigFileAndRun("point_simplepoint.sld", stylePath -> finishLoadStyle(styleName, stylePath));
    }

    private void finishLoadStyle(String styleName, Path stylePath) {
        Assertions.assertDoesNotThrow(
                () -> mockGeoServer.addExpectation(POST, "/rest/workspaces/" + EXISTING_WORKSPACE + "/styles", 200,
                        request().withBody(Files.readString(stylePath))));

        Path fileName = stylePath.getFileName();
        Assertions.assertDoesNotThrow(() -> geoServerClient
                .loadStyle(JsonHelper.getMapper().readValue(
                        "{\"name\":\"" + styleName + "\", \"file\":\"" + fileName + "\" }",
                        GeoServerStyle.class), EXISTING_WORKSPACE));
    }

    private void moveConfigFileAndRun(String filename, Consumer<Path> function) throws IOException {
        Path configDir = null;
        Path filePath = null;
        try (InputStream styleIn = GeoServerClientTest.class.getResourceAsStream(filename)) {
            configDir = Assertions.assertDoesNotThrow(() -> Files.createDirectories(Path.of("/inputs/config")));

            filePath = configDir.resolve(filename);
            Files.copy(styleIn, filePath, StandardCopyOption.REPLACE_EXISTING);

            function.accept(filePath);

        } finally {
            if (null != filePath) {
                Files.deleteIfExists(filePath);
            }
            if (null != configDir) {
                Files.deleteIfExists(configDir);
                Files.deleteIfExists(configDir.getParent());
            }
        }
    }

    @Test
    void testReload() {
        mockGeoServer.addExpectation(POST, "/rest/reload", 200);

        geoServerClient.reload();
    }

    @Test
    void testRemoveWorkspaceNonExisting() {
        mockGeoServer.addExpectation(GET, "/rest/workspaces/" + NEW_WORKSPACE + ".xml", 404);

        geoServerClient.removeWorkspace(NEW_WORKSPACE);
    }

    @Test
    void testRemoveWorkspaceExisting() {
        mockGeoServer.addExpectation(GET, "/rest/workspaces/" + EXISTING_WORKSPACE + ".xml", 200);

        mockGeoServer.addExpectation(GET, "/rest/workspaces/" + EXISTING_WORKSPACE + "/styles.xml", 200);

        mockGeoServer.addExpectation(DELETE, "/rest/workspaces/" + EXISTING_WORKSPACE, 200);

        geoServerClient.removeWorkspace(EXISTING_WORKSPACE);
    }

    @Test
    void testRemoveWorkspaceExistingWithStyles() {
        mockGeoServer.addExpectation(GET, "/rest/workspaces/" + EXISTING_WORKSPACE + ".xml", 200);

        Assertions.assertDoesNotThrow(() -> {
            try (InputStream stylesFile = GeoServerClientTest.class.getResourceAsStream("styles.xml");
                    BufferedReader reader = new BufferedReader(new InputStreamReader(stylesFile))) {
                String styles = reader.lines().collect(Collectors.joining("\n"));
                mockGeoServer.addExpectation(GET, "/rest/workspaces/" + EXISTING_WORKSPACE + "/styles.xml", 200,
                        response().withBody(styles));
            }
        });

        mockGeoServer.addExpectation(DELETE, "/rest/workspaces/" + EXISTING_WORKSPACE + "/styles/pophatch", 200);

        mockGeoServer.addExpectation(DELETE, "/rest/workspaces/" + EXISTING_WORKSPACE + "/styles/point", 200);

        mockGeoServer.addExpectation(DELETE, "/rest/workspaces/" + EXISTING_WORKSPACE + "/styles/population", 200);

        mockGeoServer.addExpectation(DELETE, "/rest/workspaces/" + EXISTING_WORKSPACE, 200);

        geoServerClient.removeWorkspace(EXISTING_WORKSPACE);
    }
}
