package com.cmclinnovations.stack.clients.utils;

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
}
