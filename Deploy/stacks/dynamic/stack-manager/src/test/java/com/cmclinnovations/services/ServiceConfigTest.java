package com.cmclinnovations.services;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import com.cmclinnovations.services.config.ServiceConfig;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

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

    @Test
    public void testGetPassword() throws IOException {
        Assert.assertEquals("Password123", serviceConfig.getPassword());
    }

    @Test
    public void testGetPasswordFile() {
        Assert.assertEquals(
                "src/test/resources/com/cmclinnovations/services/testPasswordFile",
                serviceConfig.getPasswordFile());
    }

    @Test
    public void testGetUsername() {
        Assert.assertEquals("user", serviceConfig.getUsername());

    }

}
