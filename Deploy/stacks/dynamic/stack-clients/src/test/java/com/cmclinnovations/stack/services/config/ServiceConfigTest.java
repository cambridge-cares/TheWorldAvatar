package com.cmclinnovations.stack.services.config;

import java.io.IOException;
import java.io.InputStream;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;

public class ServiceConfigTest {

    private static ObjectMapper objectMapper;
    private static ServiceConfig serviceConfig;

    @BeforeClass
    public static void readFromFile() throws IOException {
        objectMapper = JsonHelper.getMapper();

        try (InputStream resourceAsStream = ServiceConfigTest.class.getResourceAsStream("testService.json")) {
            serviceConfig = objectMapper.readValue(resourceAsStream, ServiceConfig.class);
        }
    }

    @Test
    public void testGetContainerName() {
        Assert.assertEquals("service1", serviceConfig.getName());
    }

}
