package com.cmclinnovations.stack.clients.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.annotation.Nonnull;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.json.JsonMapper;

public class JsonHelper {

    private JsonHelper() {
    }

    @Nonnull
    public static final ObjectMapper getMapper() {
        return JsonMapper.builder().findAndAddModules().build();
    }

    public static final String handleFileValues(String value) {
        if (null != value && value.startsWith("@")) {
            String file = value.substring(1);
            try {
                value = Files.readString(Path.of(file));
            } catch (IOException ex) {
                throw new RuntimeException(
                        "Failed to read file '" + Path.of(file).toAbsolutePath().toString() + "'.", ex);
            }
        }
        return value;
    }
}
