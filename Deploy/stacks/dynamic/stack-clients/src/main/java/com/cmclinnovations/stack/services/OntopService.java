package com.cmclinnovations.stack.services;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.NoSuchElementException;

import org.apache.commons.io.FilenameUtils;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.cmclinnovations.stack.clients.docker.DockerClient;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.services.config.Connection;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.dockerjava.api.model.ContainerSpecConfig;
import com.github.dockerjava.api.model.Mount;
import com.github.dockerjava.api.model.MountType;
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
    private static final String ONTOP_MAPPING_FILE = "ONTOP_MAPPING_FILE";
    private static final String ONTOP_ONTOLOGY_FILE = "ONTOP_ONTOLOGY_FILE";
    private static final String ONTOP_SPARQL_RULES_FILE = "ONTOP_SPARQL_RULES_FILE";

    private static final String DEFAULT_PORT = "8080";

    private final String containerName;
    private final OntopEndpointConfig endpointConfig;
    private final List<String> configFiles;
    private final List<String> configDirs;

    public OntopService(String stackName, ServiceConfig config) {
        super(stackName, config);

        containerName = StackClient.removeStackName(getConfig().getName());

        configFiles = List.of(getEnvironmentVariable(ONTOP_MAPPING_FILE), getEnvironmentVariable(ONTOP_ONTOLOGY_FILE),
                getEnvironmentVariable(ONTOP_SPARQL_RULES_FILE));

        configDirs = configFiles.stream().map(s -> Path.of(s).getParent().toString()).distinct()
                .collect(Collectors.toList());

        endpointConfig = new OntopEndpointConfig(containerName, getHostName(), DEFAULT_PORT);

        addEndpointConfig(endpointConfig);
    }

    @Override
    protected void doPreStartUpConfiguration() {

        ContainerSpec containerSpec = getContainerSpec();

        addConfigVolume(containerSpec);

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

        setEnvironmentVariableIfAbsent("ONTOP_LAZY_INIT", "true");
        setEnvironmentVariableIfAbsent("ONTOP_CORS_ALLOWED_ORIGINS", "*");
        setEnvironmentVariableIfAbsent("ONTOP_DEBUG", "false");
        checkEnvironmentVariableNonNull(OntopClient.ONTOP_MAPPING_FILE);

    }

    private void addConfigVolume(ContainerSpec containerSpec) {
        List<Mount> mounts = containerSpec.getMounts();
        if (null == mounts) {
            mounts = new ArrayList<>();
            containerSpec.withMounts(mounts);
        }
        for (String dir : configDirs) {
            mounts.add(new Mount()
                    .withSource(containerName)
                    .withTarget(dir)
                    .withType(MountType.VOLUME)
                    .withReadOnly(false));
        }
    }

    @Override
    public void doPostStartUpConfiguration() throws NoSuchElementException {
        DockerClient dockerClient = DockerClient.getInstance();
        String containerId = dockerClient.getContainerId(containerName);
        dockerClient.createComplexCommand(containerId, "chown", "ontop:ontop", String.join(" ", configDirs))
                .withUser("root");

        OntopClient ontopClient = OntopClient.getInstance();
        configFiles.forEach(f -> {
            if (!fileExists(f)) {
                String extension = FilenameUtils.getExtension(f);
                switch (extension) {
                    case "obda":
                        ontopClient.updateOBDA(null);
                        break;
                    case "toml":
                        ontopClient.uploadRules(List.of());
                        break;
                    default:
                        dockerClient.createComplexCommand(containerId, "touch", f).withUser("root").exec();
                        break;
                }
            }
        });
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
