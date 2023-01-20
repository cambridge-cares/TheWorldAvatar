package com.cmclinnovations.stack.clients.superset;

import java.util.Objects;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
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

    private final String url;

    protected SupersetEndpointConfig() {
        this(null, null, null, null, null, null, null, null);
    }

    public SupersetEndpointConfig(String name, String hostName, String port, String username, String passwordFile,
            String firstName, String lastName, String email) {
        super(name, passwordFile);
        this.hostName = hostName;
        this.port = port;
        this.username = username;
        this.firstName = firstName;
        this.lastName = lastName;
        this.email = email;

        this.url = null;
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
}