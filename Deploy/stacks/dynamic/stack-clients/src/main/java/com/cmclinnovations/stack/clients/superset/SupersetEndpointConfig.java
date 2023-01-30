package com.cmclinnovations.stack.clients.superset;

import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Objects;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
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
    private final String credentialProvider;
    private final String accessTokenFile;

    private final String url;

    public SupersetEndpointConfig(String name, String hostName, String port, String username, String passwordFile,
            String firstName, String lastName, String email, String credentialProvider, String accessTokenFile,
            String url) {
        super(name, passwordFile);
        this.hostName = hostName;
        this.port = port;
        this.username = username;
        this.firstName = firstName;
        this.lastName = lastName;
        this.email = email;
        this.credentialProvider = credentialProvider;
        this.accessTokenFile = accessTokenFile;
        this.url = url;
    }

    public SupersetEndpointConfig(String name, String hostName, String port, String username, String passwordFile,
            String firstName, String lastName, String email, String credentialProvider, String accessTokenFile) {
        this(name, hostName, port, username, passwordFile,
                firstName, lastName, email, credentialProvider, accessTokenFile, null);
    }

    public SupersetEndpointConfig(String name, String hostName, String port, String username, String passwordFile,
            String firstName, String lastName, String email) {
        this(name, hostName, port, username, passwordFile,
                firstName, lastName, email, null, null, null);
    }

    protected SupersetEndpointConfig() {
        this(null, null, null, null, null, null, null, null, null, null, null);
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

    public String getUrl() {
        if (null == url) {
            Objects.requireNonNull(
                    "If the 'url' is not explicitly specified then a namespace must be specified in the code.");
            return "http://" + hostName + ":" + port;
        } else {
            return url;
        }
    }

    public String getAccessTokenFile() {
        return accessTokenFile;
    }

    @JsonIgnore
    public String getAccessToken() {
        final String accessToken;
        if (null == accessTokenFile) {
            accessToken = "";
        } else {
            try (BufferedReader infile = Files.newBufferedReader(Paths.get(accessTokenFile))) {
                if (null == (accessToken = infile.readLine())) {
                    throw new IllegalArgumentException("The access token file '" + accessTokenFile
                            + "' specified for the container '" + getName() + "' is empty.");
                }
            } catch (Exception ex) {
                throw new IllegalArgumentException("The access token file '" + accessTokenFile
                        + "' specified for the container '" + getName() + "' could not be read.", ex);
            }
        }
        return accessToken;
    }

    public String getCredentialProvider() {
        return credentialProvider;
    }

}