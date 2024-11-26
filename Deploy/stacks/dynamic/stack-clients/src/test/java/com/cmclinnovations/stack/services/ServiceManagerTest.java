package com.cmclinnovations.stack.services;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

public class ServiceManagerTest {
    private static ServiceManager manager;

    @BeforeClass
    public static void constructFromFiles() {

        manager = new ServiceManager();
    }

    @Test
    public void testGetServiceConfig() {
        Assert.assertNotNull(manager.getServiceConfig("postgis"));
        Assert.assertNotNull(manager.getServiceConfig("blazegraph"));
    }
}
