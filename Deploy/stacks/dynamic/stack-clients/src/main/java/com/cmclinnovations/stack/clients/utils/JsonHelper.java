package com.cmclinnovations.stack.clients.utils;

import javax.annotation.Nonnull;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;

public class JsonHelper {

    private JsonHelper() {
    }

    @Nonnull
    public static final ObjectMapper getMapper() {
        ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new Jdk8Module());
        return mapper;
    }
}
