package com.cmclinnovations.services;

import java.io.IOException;
import java.util.Objects;

public class PostGISService extends ContainerService {

    static final String TYPE = "postgis";

    public PostGISService(String stackName, ServiceManager serviceManager, ServiceConfig config) throws IOException {
        super(stackName, serviceManager, config);

        String username = config.getUsername();
        Objects.requireNonNull(username,
                "PostgreSQL service '" + config.getName()
                        + "' requires a 'username' to be specified in its config file.");
        setEnvironmentVariable("POSTGRES_USER", username);

        String password = config.getPassword();
        Objects.requireNonNull(username,
                "PostgreSQL service '" + config.getName()
                        + "' requires a password, in a 'passwordFile', to be specified in its config file.");
        setEnvironmentVariable("POSTGRES_PASSWORD", password);
    }

}
