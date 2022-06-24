package com.cmclinnovations.apis;

import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.fasterxml.jackson.annotation.JsonIgnore;

public abstract class PasswordEndpointConfig extends AbstractEndpointConfig {

    protected PasswordEndpointConfig(String name) {
        super(name);
    }

    public abstract String getPasswordFile();

    @JsonIgnore
    public String getPassword() {
        final String passwordFile = getPasswordFile();
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