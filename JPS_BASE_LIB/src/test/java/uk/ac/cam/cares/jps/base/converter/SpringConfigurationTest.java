package uk.ac.cam.cares.jps.base.converter;

import static org.junit.Assert.assertNotNull;

import org.junit.Test;

public class SpringConfigurationTest {

    @Test
    public void PropertySourcesPlaceholderConfigurerTest(){
        assertNotNull(SpringConfiguration.propertyConfigInDev());

    }
}
