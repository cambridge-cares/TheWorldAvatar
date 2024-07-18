package com.cmclinnovations.stack.clients.core;

import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.fasterxml.jackson.annotation.JsonIgnore;

public class PasswordEndpointConfig extends AbstractEndpointConfig {

    private final String passwordFile;

    protected PasswordEndpointConfig(String name, String passwordFile) {
        super(name);
        this.passwordFile = passwordFile;
    }

    public String getPasswordFile() {
        return passwordFile;
    }

    @JsonIgnore
    public String getPassword() {
        final String password;
        if (null == passwordFile) {
            password = "";
        } else {
            try (BufferedReader infile = Files.newBufferedReader(Paths.get(passwordFile))) {
                if (null == (password = infile.readLine())) {
                    throw new IllegalArgumentException("The password file '" + passwordFile
                            + "' specified for the container '" + getName() + "' is empty.");
                }
            } catch (Exception ex) {
                throw new IllegalArgumentException("The password file '" + passwordFile
                        + "' specified for the container '" + getName() + "' could not be read.", ex);
            }
        }
        return password;
    }

}