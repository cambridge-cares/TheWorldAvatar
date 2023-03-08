package com.cmclinnovations.stack.services;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.services.config.Connection;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.dockerjava.api.model.ContainerSpecConfig;
import com.github.odiszapc.nginxparser.NgxBlock;
import com.github.odiszapc.nginxparser.NgxComment;
import com.github.odiszapc.nginxparser.NgxParam;

public final class OntopService extends ContainerService {

    public static final String TYPE = "ontop";

    public static final String ONTOP_DB_NAME = "ONTOP_DB_NAME";

    private static final String ONTOP_DB_URL = "ONTOP_DB_URL";
    private static final String ONTOP_DB_DRIVER = "ONTOP_DB_DRIVER";
    private static final String ONTOP_DB_DRIVER_URL = "ONTOP_DB_DRIVER_URL";
    private static final String ONTOP_DB_USER = "ONTOP_DB_USER";
    private static final String ONTOP_DB_PASSWORD_FILE = "ONTOP_DB_PASSWORD_FILE";

    private static final String DEFAULT_PORT = "8080";

    private final OntopEndpointConfig endpointConfig;

    private Path postgresqlDriverScratchPath;

    public OntopService(String stackName, ServiceConfig config) {
        super(stackName, config);

        endpointConfig = new OntopEndpointConfig(
                config.getName(), getHostName(), DEFAULT_PORT,
                "", null);
    }

    @Override
    public void doPreStartUpConfiguration() {

        // Copy the PostgreSQL driver library from the stack-manager into the scratch
        // volume so that the Ontop container can copy it
        Pattern postgresqlDriverPattern = Pattern.compile(".*/postgresql-[0-9.]*\\.jar");
        try (Stream<Path> postgresqlDrivers = Files.find(Path.of("/app/lib/org/postgresql/postgresql/"), 2, (path,
                attributes) -> postgresqlDriverPattern.matcher(path.toString()).matches())) {
            Optional<Path> possiblePostgresqlDriver = postgresqlDrivers.findFirst();
            if (possiblePostgresqlDriver.isPresent()) {
                Path postgresqlDriver = possiblePostgresqlDriver.get();
                postgresqlDriverScratchPath = Path.of(StackClient.SCRATCH_DIR)
                        .resolve(postgresqlDriver.getFileName());
                Files.copy(postgresqlDriver, postgresqlDriverScratchPath, StandardCopyOption.REPLACE_EXISTING);
                setEnvironmentVariableIfAbsent(ONTOP_DB_DRIVER_URL, postgresqlDriverScratchPath.toString());
            }
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        ContainerSpec containerSpec = getContainerSpec();

        Optional<ContainerSpecConfig> dbConfigRef = containerSpec.getConfigs().stream().findFirst();
        if (dbConfigRef.isPresent()) {
            PostGISEndpointConfig postgreSQLEndpoint = readEndpointConfig(dbConfigRef.get().getConfigName(),
                    PostGISEndpointConfig.class);
            String databaseName = getEnvironmentVariable(ONTOP_DB_NAME);
            setEnvironmentVariableIfAbsent(ONTOP_DB_URL, postgreSQLEndpoint.getJdbcURL(databaseName));
            setEnvironmentVariableIfAbsent(ONTOP_DB_DRIVER, postgreSQLEndpoint.getJdbcDriver());
            setEnvironmentVariableIfAbsent(ONTOP_DB_DRIVER_URL, postgreSQLEndpoint.getJdbcDriverURL());
            setEnvironmentVariableIfAbsent(ONTOP_DB_USER, postgreSQLEndpoint.getUsername());
            setEnvironmentVariableIfAbsent(ONTOP_DB_PASSWORD_FILE, postgreSQLEndpoint.getPasswordFile());
        } else {
            checkEnvironmentVariableNonNull(ONTOP_DB_URL);
            checkEnvironmentVariableNonNull(ONTOP_DB_DRIVER);
            checkEnvironmentVariableNonNull(ONTOP_DB_DRIVER_URL);
            checkEnvironmentVariableNonNull(ONTOP_DB_USER);
            checkEnvironmentVariableNonNull(ONTOP_DB_PASSWORD_FILE);
        }

        setEnvironmentVariableIfAbsent("ONTOP_LAZY_INIT", "false");
        setEnvironmentVariableIfAbsent("ONTOP_CORS_ALLOWED_ORIGINS", "*");
        setEnvironmentVariableIfAbsent("ONTOP_DEBUG", "false");
        checkEnvironmentVariableNonNull(OntopClient.ONTOP_MAPPING_FILE);

        // Choose a preprocess command depending on where the PostgreSQL driver file is
        // coming from
        String driverURL = getEnvironmentVariable(ONTOP_DB_DRIVER_URL);
        String getDriverCommand = driverURL.startsWith("/")
                ? "mkdir -p /opt/ontop/jdbc && cp " + driverURL + " /opt/ontop/jdbc/"
                : "wget -P /opt/ontop/jdbc " + driverURL;
        containerSpec
                .withCommand(List.of("/bin/sh", "-c", getDriverCommand
                        + " && ./entrypoint.sh"));
    }

    @Override
    public void doPostStartUpConfiguration() {
        OntopClient.getInstance(StackClient.removeStackName(getConfig().getName())).updateOBDA(null);

        writeEndpointConfig(endpointConfig);

        // Remove the PostgreSQL driver file from the scratch volume
        if (null != postgresqlDriverScratchPath) {
            try {
                Files.deleteIfExists(postgresqlDriverScratchPath);
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    @Override
    public void addServerSpecificNginxSettingsToLocationBlock(NgxBlock locationBlock, Map<String, String> upstreams,
            Entry<String, Connection> endpoint) {

        if (locationBlock.getValue().endsWith("ui/")) {
            locationBlock.addEntry(new NgxComment("# List of MIME types to filter (text/http included by default)"));
            NgxParam subFilterTypesParam = new NgxParam();
            subFilterTypesParam.addValue("sub_filter_types");
            subFilterTypesParam.addValue("application/javascript");
            locationBlock.addEntry(subFilterTypesParam);

            locationBlock.addEntry(new NgxComment(
                    "# Fixes an issue where all Ontop UI instances were saving their SPARQL endpoint URL to the same key-value pair"));
            NgxParam subFilterParam = new NgxParam();
            subFilterParam.addValue("sub_filter");
            subFilterParam.addValue("'yasqe: {sparql: {endpoint: endpointUrl}}'");
            subFilterParam.addValue("'yasqe: {sparql: {endpoint: endpointUrl}}, persistencyPrefix: endpointUrl'");
            locationBlock.addEntry(subFilterParam);
        }
    }

}
