package uk.ac.cam.cares.jps.base.converter.test;

import org.junit.Test;
import uk.ac.cam.cares.jps.base.util.converter.SpringConfiguration;
import static org.junit.Assert.assertNotNull;

public class SpringConfigurationTest {

    @Test
    public void PropertySourcesPlaceholderConfigurerTest(){
        assertNotNull(SpringConfiguration.propertyConfigInDev());

    }
}
