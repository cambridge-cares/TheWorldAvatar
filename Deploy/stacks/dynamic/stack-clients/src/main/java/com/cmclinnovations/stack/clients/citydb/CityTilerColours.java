package com.cmclinnovations.stack.clients.citydb;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.databind.JsonNode;

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
        if (null != fileName && fileName.startsWith("@")) {
            String filePath = fileName.substring(1);
            try {
                this.config = JsonHelper.getMapper().readTree(Files.readString(Path.of(filePath)));
            } catch (IOException e) {
                throw new RuntimeException("Failed to read in CityTiler colour config file from '" + fileName + "'.");
            }
        } else {
            throw new RuntimeException("Invalid path for CityTiler colour config file: '" + fileName);
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
