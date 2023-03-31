package com.cmclinnovations.stack.services;

import java.io.IOException;
import java.net.URISyntaxException;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

public class ServiceManagerTest {
    private static ServiceManager manager;

    @BeforeClass
    public static void constructFromFiles() throws IOException, URISyntaxException {

        manager = new ServiceManager();
    }

    @Test
    public void testGetServiceConfig() {
        Assert.assertNotNull(manager.getServiceConfig("postgis"));
        Assert.assertNotNull(manager.getServiceConfig("blazegraph"));
    }
}
