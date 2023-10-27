package uk.ac.cam.cares.jps.base.converter;

import org.junit.Test;
import static org.junit.Assert.assertNotNull;

public class SpringConfigurationTest {

    @Test
    public void PropertySourcesPlaceholderConfigurerTest(){
        assertNotNull(SpringConfiguration.propertyConfigInDev());

    }
}
