package com.cmclinnovations.stack.services;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import com.cmclinnovations.stack.clients.core.EndpointNames;
import com.cmclinnovations.stack.clients.superset.SupersetEndpointConfig;
import com.cmclinnovations.stack.clients.utils.FileUtils;
import com.cmclinnovations.stack.services.config.Connection;
import com.cmclinnovations.stack.services.config.ServiceConfig;
import com.github.dockerjava.api.model.ContainerSpec;
import com.github.odiszapc.nginxparser.NgxBlock;
import com.github.odiszapc.nginxparser.NgxComment;
import com.github.odiszapc.nginxparser.NgxParam;

public class SupersetService extends ContainerService {

    public static final String TYPE = "superset";

    private final SupersetEndpointConfig endpointConfig;

    protected static final List<String> BODY_SUBSTITUTIONS_PATH_LIST = Arrays.asList(
            "/static", "/chart/", "/dashboard/", "/dataset/", "/savedqueryview/", "/tablemodelview/",
            "/dashboardasync/", "/csstemplatemodelview/", "/csstemplateasyncmodelview/", "api/v1/", "/login", "/logout",
            "/superset/", "/csstemplatemodelview/", "/annotationlayer/", "/logmodelview/",
            "/rowlevelsecurityfiltersmodelview/", "/roles/", "/users/", "/profile/", "/databaseview/", "/tabstateview/",
            "/explore/", "/datasource/");
    protected static final List<String> SUB_FILTER_TYPES_LIST = Arrays.asList(
            "text/css", "text/javascript", "application/javascript", "application/json");
    public static final String LOCATION = "location";

    private static final String DEFAULT_USERNAME = "admin";
    private static final String DEFAULT_PORT = "8088";
    private static final String DEFAULT_PASSWORD_FILE = "/run/secrets/superset_password";
    private static final String DEFAULT_FIRSTNAME = "Superset";
    private static final String DEFAULT_LASTNAME = "Admin";
    private static final String DEFAULT_EMAIL = "admin@superset.com";
    private static final String DEFAULT_SECRET_KEY_FILE = "/run/secrets/superset_secret_key";
    private static final String DEFAULT_CREDENTIAL_PROVIDER = "db";

    public SupersetService(String stackName, ServiceConfig config) {
        super(stackName, config);

        setEnvironmentVariableIfAbsent("SUPERSET_USERNAME", DEFAULT_USERNAME);
        setEnvironmentVariableIfAbsent("SUPERSET_PASSWORD_FILE", DEFAULT_PASSWORD_FILE);
        setEnvironmentVariableIfAbsent("SUPERSET_FIRSTNAME", DEFAULT_FIRSTNAME);
        setEnvironmentVariableIfAbsent("SUPERSET_LASTNAME", DEFAULT_LASTNAME);
        setEnvironmentVariableIfAbsent("SUPERSET_EMAIL", DEFAULT_EMAIL);
        setEnvironmentVariableIfAbsent("SUPERSET_SECRET_KEY_FILE", DEFAULT_SECRET_KEY_FILE);
        setEnvironmentVariableIfAbsent("SUPERSET_CREDENTIAL_PROVIDER", DEFAULT_CREDENTIAL_PROVIDER);

        endpointConfig = new SupersetEndpointConfig(
                EndpointNames.SUPERSET, getHostName(), DEFAULT_PORT,
                getEnvironmentVariable("SUPERSET_USERNAME"), getEnvironmentVariable("SUPERSET_PASSWORD_FILE"),
                getEnvironmentVariable("SUPERSET_FIRSTNAME"), getEnvironmentVariable("SUPERSET_LASTNAME"),
                getEnvironmentVariable("SUPERSET_EMAIL"), getEnvironmentVariable("DEFAULT_SECRET_KEY_FILE"),
                getEnvironmentVariable("SUPERSET_CREDENTIAL_PROVIDER"));

        addEndpointConfig(endpointConfig);
    }

    @Override
    public void addServerSpecificNginxSettingsToLocationBlock(NgxBlock locationBlock,
            Map<String, String> upstreams, Entry<String, Connection> endpoint) {
        Connection connection = endpoint.getValue();
        URI externalPath = connection.getExternalPath();

        locationBlock.addEntry(new NgxComment("# "));

        locationBlock.addEntry(new NgxComment("# Substitution of redirect \"Location\" header"));
        NgxParam proxyRedirectParam = new NgxParam();
        proxyRedirectParam.addValue("proxy_redirect");
        proxyRedirectParam.addValue("~^(http://|https://|)([^/]*/)(?!"
                + FileUtils.fixSlashs(externalPath.getPath(), false, true) + ")(.*)");
        proxyRedirectParam
                .addValue("$scheme://$http_host/" + FileUtils.fixSlashs(externalPath.getPath(), false, true) + "$3");
        locationBlock.addEntry(proxyRedirectParam);

        locationBlock.addEntry(new NgxComment("# List of MIME types to filter (text/http included by default)"));
        NgxParam subFilterTypesParam = new NgxParam();
        subFilterTypesParam.addValue("sub_filter_types");
        SUB_FILTER_TYPES_LIST.stream().forEach(subFilterTypesParam::addValue);
        locationBlock.addEntry(subFilterTypesParam);

        locationBlock
                .addEntry(new NgxComment("# Sub_filter_once on by default and needed multiple times in same file"));
        NgxParam subFilterOnceParam = new NgxParam();
        subFilterOnceParam.addValue("sub_filter_once");
        subFilterOnceParam.addValue("off");
        locationBlock.addEntry(subFilterOnceParam);

        locationBlock.addEntry(new NgxComment("# Prevents zipping of response as that would prevent subfiltering"));
        NgxParam proxySetHeaderParam = new NgxParam();
        proxySetHeaderParam.addValue("proxy_set_header");
        proxySetHeaderParam.addValue("Accept-Encoding");
        proxySetHeaderParam.addValue("\"\"");
        locationBlock.addEntry(proxySetHeaderParam);

        locationBlock.addEntry(new NgxComment("# Substitution expressions for response body"));
        BODY_SUBSTITUTIONS_PATH_LIST.stream().forEach(subPath -> {
            NgxParam subFilterParam = new NgxParam();
            subFilterParam.addValue("sub_filter");
            subFilterParam.addValue("\"" + subPath + "\"");
            Boolean hasLeadingSlash = subPath.charAt(0) == '/';
            subFilterParam.addValue("\""
                    + FileUtils.fixSlashs(externalPath.getPath(), hasLeadingSlash, !hasLeadingSlash) + subPath + "\"");
            // TODO: need to add in extra path here if on front of other nginx
            locationBlock.addEntry(subFilterParam);
        });
    }

    @Override
    protected void doPreStartUpConfiguration() {
        writeSupersetFlaskConfig();
    }

    private void writeSupersetFlaskConfig() {
        ContainerSpec containerSpec = getContainerSpec();

        try (InputStream supersetConfig = SupersetService.class.getResourceAsStream("superset/superset_config.py");
                InputStreamReader inputStreamReader = new InputStreamReader(supersetConfig);
                BufferedReader bufferedReader = new BufferedReader(inputStreamReader)) {
            String fileText = bufferedReader.lines().collect(Collectors.joining(System.lineSeparator()));
            containerSpec.withCommand(List.of("/bin/sh", "-c",
                    "echo \"" + fileText + "\" > pythonpath/superset_config.py" + " && /usr/bin/run-server.sh"));
        } catch (IOException ex) {
            throw new RuntimeException("Failed to load \"superset_config.py\" file.", ex);
        }
    }

    @Override
    public void doPostStartUpConfiguration() {
        makeUser(endpointConfig);

        executeCommand("sh", "-c",
                "pip install sqlalchemy==1.3.24 && pip install psycopg2 && superset db upgrade && superset init");
    }

    private void makeUser(SupersetEndpointConfig endpointConfig) {
        executeCommand("superset", "fab", "create-admin", "--username", endpointConfig.getUsername(), "--firstname",
                endpointConfig.getFirstName(), "--lastname", endpointConfig.getLastName(), "--email",
                endpointConfig.getEmail(), "--password", endpointConfig.getPassword());
    }
}