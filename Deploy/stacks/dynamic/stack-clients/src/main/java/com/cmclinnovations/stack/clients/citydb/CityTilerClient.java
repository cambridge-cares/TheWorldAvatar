package com.cmclinnovations.stack.clients.citydb;

import java.io.ByteArrayOutputStream;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.clients.utils.TempDir;
import com.fasterxml.jackson.databind.JsonNode;

public class CityTilerClient extends ContainerClient {

    private static final Logger logger = LoggerFactory.getLogger(CityTilerClient.class);

    private static final String COLOUR_CONFIG_FILENAME = "citytiler_config.json";
    private static final String COLOUR_CONFIG_DIR = "/py3dtilers.git/py3dtilers/Color/";
    public static final String COLOUR_CONFIG_FILE = COLOUR_CONFIG_DIR + COLOUR_CONFIG_FILENAME;
    public static final String DEFAULT_COLOUR_CONFIG_FILE = COLOUR_CONFIG_DIR + "citytiler_config_default.json";

    private static CityTilerClient instance = null;

    public static CityTilerClient getInstance() {
        if (null == instance) {
            instance = new CityTilerClient();
        }
        return instance;
    }

    private final PostGISEndpointConfig postgreSQLEndpoint;

    private CityTilerClient() {
        postgreSQLEndpoint = readEndpointConfig(EndpointNames.POSTGIS, PostGISEndpointConfig.class);
    }

    /**
     * @param database Name of the database containing the 3D city data.
     * @param schema   Must be "citydb" due to lack of support for other values in
     *                 CityTiler tool.
     *                 See https://github.com/VCityTeam/py3dtilers/issues/112 for
     *                 details.
     * @param options  Command line options to pass to the CityTiler tool.
     */
    public void generateTiles(String database, String schema, CityTilerOptions options) {

        String containerId = getContainerId("citytiler");

        JsonNode colours = options.getColours();
        if (null == colours) {
            executeSimpleCommand(containerId, "cp",
                    CityTilerClient.DEFAULT_COLOUR_CONFIG_FILE, CityTilerClient.COLOUR_CONFIG_FILE);
        } else {
            sendFilesContent(containerId,
                    Map.of(COLOUR_CONFIG_FILE, colours.toString().getBytes()),
                    COLOUR_CONFIG_DIR);
        }

        try (TempDir configDir = makeRemoteTempDir(containerId)) {

            String configFilename = database + "-" + schema + ".yml";

            sendFilesContent(containerId,
                    Map.of(configFilename, generateConfigFileContent(database, schema).getBytes()),
                    configDir.toString());

            String crsIn = getCRSFromDatabase(database, schema);

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            ByteArrayOutputStream errorStream = new ByteArrayOutputStream();

            String execId = createComplexCommand(containerId, options.appendOtherArgs("citygml-tiler",
                    "--db_config_path", configDir.getPath().resolve(configFilename).toString(),
                    "--crs_in", crsIn,
                    "--crs_out", "EPSG:4978",
                    "--output_dir", Path.of("/3dtiles", database, schema).toString()))
                    .withOutputStream(outputStream)
                    .withErrorStream(errorStream)
                    .exec();

            handleErrors(errorStream, execId, logger);
        }

    }

    /**
     * @param database
     * @param schema   Not used here as the CityTiler tool assumes it's "citydb"
     */
    private String generateConfigFileContent(String database, String schema) {

        return Stream.of("PG_HOST: " + postgreSQLEndpoint.getHostName(),
                "PG_PORT: " + postgreSQLEndpoint.getPort(),
                "PG_NAME: " + database,
                "PG_USER: " + postgreSQLEndpoint.getUsername(),
                "PG_PASSWORD: " + postgreSQLEndpoint.getPassword())
                .collect(Collectors.joining("\n"));
    }

    private String getCRSFromDatabase(String database, String schema) {
        JSONObject crsComponents = StackClient.getRemoteRDBStoreClient(database)
                .executeQuery("SELECT auth_name, auth_srid\n" +
                        "FROM " + schema + ".database_srs srs, public.spatial_ref_sys ref\n" +
                        "WHERE srs.srid = ref.srid\n" +
                        "LIMIT 1")
                .getJSONObject(0);
        return crsComponents.getString("auth_name") + ":" + crsComponents.getInt("auth_srid");
    }

}