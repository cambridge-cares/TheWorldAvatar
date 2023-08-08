package uk.ac.cam.cares.jps.agent.email;

import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;
import static uk.ac.cam.cares.jps.agent.email.Config.KEY_SMTP_HOST;
import static uk.ac.cam.cares.jps.agent.email.Config.KEY_SMTP_PASS;
import static uk.ac.cam.cares.jps.agent.email.Config.KEY_SMTP_PORT;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

/**
 * Contains unit tests for the Config class.
 *
 * @author Michael Hillman (mdhillman<@>cmclinnovations.com)
 */
@TestMethodOrder(OrderAnnotation.class)
public class ConfigTest {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(ConfigTest.class);

    /**
     * Attempts to read the example properties file.
     */
    @Test
    public void readSampleProperties() {

        // Mock the config to return a set file location
        Config mockConfig = spy(Config.class);
        when(mockConfig.getPropertyFileLocation()).thenReturn("./data/example-properties.txt");

        try  {
            // Try loading the config
            mockConfig.readProperties();

            // Test some properties
            Assertions.assertEquals(
                "my-email-server.com", 
                mockConfig.getProperty(KEY_SMTP_HOST), 
                "'smtp.host' property did not match expected result!"
            );
            Assertions.assertEquals(
                "123",
                mockConfig.getProperty(KEY_SMTP_PORT),
                "'smtp.port' property did not match expected result!"
            );
            Assertions.assertEquals(
                "MyEmailPassword",
                mockConfig.getProperty(KEY_SMTP_PASS),
                "'smtp.pass' property did not match expected result!"
            );

        } catch(Exception exception) {
            LOGGER.error(exception);
            Assertions.fail("Could not read example.properties file.");
        }
    }

}
// End of class.