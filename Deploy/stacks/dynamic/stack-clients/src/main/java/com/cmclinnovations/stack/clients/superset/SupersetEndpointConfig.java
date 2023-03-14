package com.cmclinnovations.stack.clients.superset;

import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Objects;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
import com.cmclinnovations.swagger.superset.ApiException;
import com.cmclinnovations.swagger.superset.api.SecurityApi;
import com.cmclinnovations.swagger.superset.model.InlineResponse20050;
import com.cmclinnovations.swagger.superset.model.SecurityLoginBody;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
public class SupersetEndpointConfig extends PasswordEndpointConfig {

    private final String hostName;
    private final String port;
    private final String username;
    private final String firstName;
    private final String lastName;
    private final String email;
    private final String secretKeyFile;
    private final String credentialProvider;

    private final String url;

    public SupersetEndpointConfig(String name, String hostName, String port, String username, String passwordFile,
            String firstName, String lastName, String email, String secretKeyFile, String credentialProvider,
            String url) {
        super(name, passwordFile);
        this.hostName = hostName;
        this.port = port;
        this.username = username;
        this.firstName = firstName;
        this.lastName = lastName;
        this.email = email;
        this.secretKeyFile = secretKeyFile;
        this.credentialProvider = credentialProvider;
        this.url = url;
    }

    public SupersetEndpointConfig(String name, String hostName, String port, String username, String passwordFile,
            String firstName, String lastName, String email, String secretKeyFile, String credentialProvider) {
        this(name, hostName, port, username, passwordFile, firstName, lastName, email, secretKeyFile,
                credentialProvider, null);
    }

    public SupersetEndpointConfig(String name, String hostName, String port, String username, String passwordFile,
            String firstName, String lastName, String email, String secretKeyFile) {
        this(name, hostName, port, username, passwordFile, firstName, lastName, email, secretKeyFile, null, null);
    }

    protected SupersetEndpointConfig() {
        this(null, null, null, null, null, null, null, null, null, null);
    }

    public String getHostName() {
        return hostName;
    }

    public String getPort() {
        return port;
    }

    public String getUsername() {
        return username;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public String getEmail() {
        return email;
    }

    public String getSecretKeyFile() {
        return secretKeyFile;
    }

    @JsonIgnore
    public String getSecretKey() {
        final String secretKey;
        if (null == secretKeyFile) {
            secretKey = "";
        } else {
            try (BufferedReader infile = Files.newBufferedReader(Paths.get(secretKeyFile))) {
                if (null == (secretKey = infile.readLine())) {
                    throw new IllegalArgumentException("The secret key file '" + secretKeyFile
                            + "' specified for the container '" + getName() + "' is empty.");
                }
            } catch (Exception ex) {
                throw new IllegalArgumentException("The secret key file '" + secretKeyFile
                        + "' specified for the container '" + getName() + "' could not be read.", ex);
            }
        }
        return secretKey;
    }

    public String getUrl() {
        if (null == url) {
            Objects.requireNonNull(
                    "If the 'url' is not explicitly specified then a namespace must be specified in the code.");
            return "http://" + hostName + ":" + port;
        } else {
            return url;
        }
    }

    @JsonIgnore
    public String getAccessToken() {
        SecurityApi securityApi = new SecurityApi(SupersetClient.getInstance().getApiClient());

        SecurityLoginBody securityLoginBody = new SecurityLoginBody();
        securityLoginBody.setUsername(getUsername());
        securityLoginBody.setPassword(getPassword());
        securityLoginBody.setProvider(getCredentialProvider());

        try {
            InlineResponse20050 response = securityApi.apiV1SecurityLoginPost(securityLoginBody);
            return (String) response.getAccessToken();
        } catch (ApiException ex) {
            throw new RuntimeException("Exception occured when logging in to generate an access token for the API.",
                    ex);
        }
    }

    public String getCredentialProvider() {
        return credentialProvider;
    }

}