package com.cmclinnovations.apis;

import java.io.BufferedReader;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.fasterxml.jackson.annotation.JsonIgnore;

public abstract class AbstractEndpointConfig {

    private final String name;

    protected AbstractEndpointConfig(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}