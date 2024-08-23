package com.cmclinnovations.stack.services;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public final class PostGISService extends ContainerService {

    /**
     *
     */
    private static final String POSTGRES_USER_KEY = "POSTGRES_USER";

    public static final String TYPE = "postgres";

    private static final Path PGPASS_FILE = Path.of("/root", ".pgpass");

    private static final String DEFAULT_USERNAME = "postgres";
    private static final String DEFAULT_PORT = "5432";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/postgis_password";

    private final PostGISEndpointConfig endpointConfig;

    public PostGISService(String stackName, ServiceConfig config) {
        super(stackName, config);

        setEnvironmentVariableIfAbsent(POSTGRES_USER_KEY, DEFAULT_USERNAME);
        setEnvironmentVariableIfAbsent("PGUSER", getEnvironmentVariable(POSTGRES_USER_KEY));
        setEnvironmentVariableIfAbsent("POSTGRES_PASSWORD_FILE", DEFAULT_PASSWORD_FILE);
        setEnvironmentVariableIfAbsent("PGPASSFILE", PGPASS_FILE.toString());

        endpointConfig = new PostGISEndpointConfig(
                EndpointNames.POSTGIS, getHostName(), DEFAULT_PORT,
                getEnvironmentVariable(POSTGRES_USER_KEY), getEnvironmentVariable("POSTGRES_PASSWORD_FILE"));

        addEndpointConfig(endpointConfig);
    }

    @Override
    public void doPostStartUpConfiguration() {
        writePGPASSFile();

        copyJDBCDriverToVolume();
    }

    private void writePGPASSFile() {
        sendFiles(Map.of(
                PGPASS_FILE.getFileName().toString(),
                ("localhost:"
                        + endpointConfig.getPort() + ":"
                        + "*:"
                        + endpointConfig.getUsername() + ":"
                        + endpointConfig.getPassword())
                        .getBytes()),
                PGPASS_FILE.getParent().toString());

        executeCommand("chmod", "0600", PGPASS_FILE.toString());
    }

    private void copyJDBCDriverToVolume() {

        // Copy the PostgreSQL driver library from the stack-manager into the scratch
        // volume so that the Ontop container can copy it
        Pattern postgresqlDriverPattern = Pattern.compile(".*/postgresql-[0-9.]*\\.jar");
        try (Stream<Path> postgresqlDrivers = Files.find(Path.of("/app/lib/org/postgresql/postgresql/"), 2, (path,
                attributes) -> postgresqlDriverPattern.matcher(path.toString()).matches())) {
            Optional<Path> possiblePostgresqlDriver = postgresqlDrivers.findFirst();
            if (possiblePostgresqlDriver.isPresent()) {
                Path postgresqlDriver = possiblePostgresqlDriver.get();
                Files.copy(postgresqlDriver,
                        Path.of("/jdbc").resolve(postgresqlDriver.getFileName()),
                        StandardCopyOption.REPLACE_EXISTING);
            }
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

}
