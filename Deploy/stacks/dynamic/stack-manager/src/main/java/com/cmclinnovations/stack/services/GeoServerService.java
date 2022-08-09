package com.cmclinnovations.stack.services;

import java.net.MalformedURLException;
import java.net.URL;

import com.cmclinnovations.stack.clients.core.RESTEndpointConfig;
import com.cmclinnovations.stack.services.config.ServiceConfig;

public final class GeoServerService extends ContainerService {

    public static final String TYPE = "geoserver";

    private static final String ADMIN_USERNAME = "admin";
    private static final String DEFAULT_ADMIN_PASSWORD_FILE = "/run/secrets/geoserver_password";

    public GeoServerService(String stackName, ServiceManager serviceManager, ServiceConfig config) {
        super(stackName, serviceManager, config);
    }

    @Override
    public void doPostStartUpConfiguration() {
        String passwordFile = getEnvironmentVariable("ADMIN_PASSWORD_FILE");
        if (null == passwordFile) {
            passwordFile = DEFAULT_ADMIN_PASSWORD_FILE;
        }

        try {
            RESTEndpointConfig geoserverEndpointConfig = new RESTEndpointConfig("geoserver",
                    new URL("http", getHostName(), 8080, "/geoserver/"),
                    ADMIN_USERNAME, passwordFile);

            writeEndpointConfig(geoserverEndpointConfig);

            String password = geoserverEndpointConfig.getPassword();

            executeCommand("bash", "-c",
                    "ADMIN_HEADER=$(echo -n 'admin:geoserver' | base64) && curl -H \"Authorization: basic $ADMIN_HEADER\" -X PUT http://localhost:8080/geoserver/rest/security/self/password -H 'accept: application/json' -H 'content-type: application/json' -d '{\"newPassword\": \""
                            + password + "\"}'");

            executeCommand("bash", "-c", "ADMIN_HEADER=$(echo -n \"admin:" + password
                    + "\" | base64) && curl -H \"Authorization: basic $ADMIN_HEADER\" -X PUT http://localhost:8080/geoserver/rest/settings -H 'accept: application/json' -H  'content-type: application/json' -d '{\"global\":{\"settings\":{\"proxyBaseUrl\":\"${X-Forwarded-Proto}:\\/\\/${X-Forwarded-Host}\\/geoserver\"},\"useHeadersProxyURL\":true}}'");

        } catch (MalformedURLException ex) {
            throw new RuntimeException("Failed to construct URL for GeoServer config file.", ex);
        }
    }

}
