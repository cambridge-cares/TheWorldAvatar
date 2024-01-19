package com.cmclinnovations.stack.services;

import java.util.Optional;

import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.dockerjava.api.model.ContainerSpecConfig;

public final class CityDBImpExpService extends ContainerService {

    public static final String TYPE = "citydbimpexp";

    private static final String CITYDB_HOST = "CITYDB_HOST";
    private static final String CITYDB_PORT = "CITYDB_PORT";
    private static final String CITYDB_NAME = "CITYDB_NAME";
    private static final String CITYDB_USERNAME = "CITYDB_USERNAME";
    private static final String CITYDB_PASSWORD = "CITYDB_PASSWORD";

    private static final String DEFAULT_USERNAME = "postgres";

    public CityDBImpExpService(String stackName, ServiceConfig config) {
        super(stackName, config);
    }

    @Override
    protected void doPreStartUpConfiguration() {
        ContainerSpec containerSpec = getContainerSpec();

        Optional<ContainerSpecConfig> dbConfigRef = containerSpec.getConfigs().stream().findFirst();
        if (dbConfigRef.isPresent()) {
            PostGISEndpointConfig postgreSQLEndpoint = readEndpointConfig(dbConfigRef.get().getConfigName(),
                    PostGISEndpointConfig.class);
            setEnvironmentVariableIfAbsent(CITYDB_HOST, postgreSQLEndpoint.getHostName());
            setEnvironmentVariableIfAbsent(CITYDB_PORT, postgreSQLEndpoint.getPort());
            setEnvironmentVariableIfAbsent(CITYDB_USERNAME, postgreSQLEndpoint.getUsername());
            setEnvironmentVariableIfAbsent(CITYDB_PASSWORD, postgreSQLEndpoint.getPassword());
        } else {
            checkEnvironmentVariableNonNull(CITYDB_HOST);
            setEnvironmentVariableIfAbsent(CITYDB_USERNAME, DEFAULT_USERNAME);
            checkEnvironmentVariableNonNull(CITYDB_PASSWORD);
        }
        setEnvironmentVariableIfAbsent(CITYDB_NAME, getEnvironmentVariable(CITYDB_USERNAME));
    }
}
