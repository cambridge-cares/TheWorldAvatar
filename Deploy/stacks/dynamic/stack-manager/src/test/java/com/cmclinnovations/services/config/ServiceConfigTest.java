package com.cmclinnovations.services.config;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

public class ServiceConfigTest {

    private static ObjectMapper objectMapper;
    private static ServiceConfig serviceConfig;

    @BeforeClass
    public static void readFromFile() throws IOException, URISyntaxException {
        objectMapper = new ObjectMapper();
        try (BufferedReader reader = Files
                .newBufferedReader(Paths.get(ServiceConfigTest.class.getResource("testService.json").toURI()))) {
            serviceConfig = objectMapper.readValue(reader, ServiceConfig.class);
        }
    }

    @Test
    public void testGetContainerName() {
        Assert.assertEquals("service1", serviceConfig.getName());
    }

}
