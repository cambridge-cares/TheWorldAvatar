package com.cmclinnovations.stack.clients.citydb;

import java.io.IOException;
import java.net.URL;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

class CityTilerColours {

    private final JsonNode config;

    public CityTilerColours() {
        config = null;
    }

    /**
     * Constructor for the external form (just the fileName as a string)
     */
    @JsonCreator
    public CityTilerColours(String fileName) {
        try {
            this.config = new ObjectMapper().readTree(new URL(fileName));
        } catch (IOException e) {
            throw new RuntimeException("Failed to read in CityTiler colour config file from '" + fileName + "'.");
        }
    }

    /**
     * Constructor for the explicit form
     */
    @JsonCreator
    public CityTilerColours(JsonNode config) {
        this.config = config;
    }

    public JsonNode getJson() {
        return config;
    }

}
