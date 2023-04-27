package com.cmclinnovations.stack.services;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

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
    private static final String ONTOP_DB_USER = "ONTOP_DB_USER";
    private static final String ONTOP_DB_PASSWORD_FILE = "ONTOP_DB_PASSWORD_FILE";

    private static final String DEFAULT_PORT = "8080";

    private final OntopEndpointConfig endpointConfig;

    private Path postgresqlDriverScratchPath;

    public OntopService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);

        endpointConfig = new OntopEndpointConfig(StackClient.removeStackName(config.getName()),
                getHostName(), DEFAULT_PORT, "", null);
    }

    @Override
    public void doPreStartUpConfiguration() {

        ContainerSpec containerSpec = getContainerSpec();

        Optional<ContainerSpecConfig> dbConfigRef = containerSpec.getConfigs().stream().findFirst();
        if (dbConfigRef.isPresent()) {
            PostGISEndpointConfig postgreSQLEndpoint = readEndpointConfig(dbConfigRef.get().getConfigName(),
                    PostGISEndpointConfig.class);
            String databaseName = getEnvironmentVariable(ONTOP_DB_NAME);
            setEnvironmentVariableIfAbsent(ONTOP_DB_URL, postgreSQLEndpoint.getJdbcURL(databaseName));
            setEnvironmentVariableIfAbsent(ONTOP_DB_DRIVER, postgreSQLEndpoint.getJdbcDriver());
            setEnvironmentVariableIfAbsent(ONTOP_DB_USER, postgreSQLEndpoint.getUsername());
            setEnvironmentVariableIfAbsent(ONTOP_DB_PASSWORD_FILE, postgreSQLEndpoint.getPasswordFile());
        } else {
            checkEnvironmentVariableNonNull(ONTOP_DB_URL);
            checkEnvironmentVariableNonNull(ONTOP_DB_DRIVER);
            checkEnvironmentVariableNonNull(ONTOP_DB_USER);
            checkEnvironmentVariableNonNull(ONTOP_DB_PASSWORD_FILE);
        }

        setEnvironmentVariableIfAbsent("ONTOP_LAZY_INIT", "false");
        setEnvironmentVariableIfAbsent("ONTOP_CORS_ALLOWED_ORIGINS", "*");
        setEnvironmentVariableIfAbsent("ONTOP_DEBUG", "false");
        checkEnvironmentVariableNonNull(OntopClient.ONTOP_MAPPING_FILE);

    }

    @Override
    public void doPostStartUpConfiguration() {
        OntopClient.getInstance(StackClient.removeStackName(getConfig().getName())).updateOBDA(null);

        writeEndpointConfig(endpointConfig);

    }

    /**
     * used after spinning up a new ontop container
     * 
     * @return
     */
    public OntopEndpointConfig getOntopEndpointConfig() {
        return this.endpointConfig;
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
