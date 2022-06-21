package uk.ac.cam.cares.jps.agent.configuration.test;

import junit.framework.TestCase;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import uk.ac.cam.cares.jps.agent.configuration.gPROMSAgentConfiguration;

public class gPROMSAgentConfigurationTest extends TestCase {

    public void testNewGPROMSAgentProperty() {
        gPROMSAgentConfiguration configuration = null;
        try {
            configuration = new gPROMSAgentConfiguration();
        } finally {
            assertNotNull(configuration);
        }
    }

    public void testPropertyConfigInDev() {
        PropertySourcesPlaceholderConfigurer propertyConfig = null;
        try {
            propertyConfig = gPROMSAgentConfiguration.propertyConfigInDev();
        } finally {
            assertNotNull(propertyConfig);
        }
    }

}
