package com.cmclinnovations.services;

import java.io.IOException;
import java.nio.file.Path;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

public class ServiceManagerTest {
    private static ServiceManager manager;

    @BeforeClass
    public static void constructFromFiles() throws IOException {
        manager = new ServiceManager();
        manager.loadConfigs(Path.of("src", "test", "resources", "com", "cmclinnovations", "services"));
    }

    @Test
    public void testGetServiceConfig() {
        Assert.assertNotNull(manager.getServiceConfig("testService"));
        Assert.assertNotNull(manager.getServiceConfig("testService2"));
    }
}
