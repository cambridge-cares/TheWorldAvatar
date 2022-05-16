package com.cmclinnovations.services;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

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
    public void testGetHost() {
        Assert.assertEquals("machine1", serviceConfig.getHost());
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
    public void testGetPort() {
        PortMapping portMapping = serviceConfig.getPorts().get("first");
        Assert.assertEquals(1234, portMapping.getInternalPort().intValue());
        Assert.assertEquals(5678, portMapping.getExternalPort().intValue());
    }

    @Test
    public void testGetUsername() {
        Assert.assertEquals("user", serviceConfig.getUsername());

    }

}
