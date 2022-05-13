package com.cmclinnovations.services;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class ServiceConfig {

    private final String name;
    private final String host;
    private final Integer port;
    private final String path;
    private final String username;
    private final String passwordFile;

    // Docker specific settings
    private final String image;
    private final String containerName;

    public ServiceConfig() {
        name = null;
        host = null;
        port = null;
        path = null;
        username = null;
        passwordFile = null;

        image = null;
        containerName = null;
    }

    public String getName() {
        return name;
    }

    public String getHost() {
        return host;
    }

    public Integer getPort() {
        return port;
    }

    public String getPath() {
        return path;
    }

    public String getUsername() {
        return username;
    }

    public String getPasswordFile() {
        return passwordFile;
    }

    public String getPassword() throws IOException {
        final String password;
        if (null == passwordFile) {
            password = "";
        } else {
            try (BufferedReader infile = Files.newBufferedReader(Paths.get(passwordFile))) {
                if (null == (password = infile.readLine())) {
                    throw new IllegalArgumentException("The password file '" + passwordFile
                            + "' specified for the container '" + name + "' is empty.");
                }
            }
        }
        return password;
    }

    public String getImage() {
        return image;
    }

    public String getContainerName() {
        return (null != containerName) ? containerName : name;
    }

}
