package uk.ac.cam.cares.jps.agent.email;

import com.github.stefanbirkner.systemlambda.SystemLambda;
import static uk.ac.cam.cares.jps.agent.email.EmailAgentConfiguration.KEY_SMTP_HOST;
import static uk.ac.cam.cares.jps.agent.email.EmailAgentConfiguration.KEY_SMTP_PASS;
import static uk.ac.cam.cares.jps.agent.email.EmailAgentConfiguration.KEY_SMTP_PORT;
import static uk.ac.cam.cares.jps.agent.email.EmailAgentConfiguration.KEY_TO_ADDRESS;
import static uk.ac.cam.cares.jps.agent.email.EmailAgentConfiguration.KEY_WHITE_IPS;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

/**
 * Contains unit tests for the EmailAgentConfiguration class.
 *
 * @author Michael Hillman
 */
@TestMethodOrder(OrderAnnotation.class)
public class EmailAgentConfigurationTest {

    /**
     * Attempts to read the example properties file.
     */
    @Test
    @Order(1)
    public void readSampleProperties() {
        try {
            SystemLambda.withEnvironmentVariable("EMAIL_AGENT_PROPERTIES", "./data/example-properties.txt").execute(() -> {
                EmailAgentConfiguration.readProperties();
            });
        } catch (Exception exception) {
            Assertions.fail("Could not read example.properties file.");
        }
    }

    /**
     * Check that some of the property values aren't null.
     */
    @Test
    @Order(2)
    public void checkPropertyValues() {
        System.out.println("INFO: Running checkPropertyValues()...");

        Assertions.assertNotNull(
                EmailAgentConfiguration.getProperty(KEY_SMTP_HOST),
                "Could not read sample " + KEY_SMTP_HOST + " property."
        );

        Assertions.assertNotNull(
                EmailAgentConfiguration.getProperty(KEY_SMTP_PORT),
                "Could not read sample " + KEY_SMTP_PORT + " property."
        );

        Assertions.assertNotNull(
                EmailAgentConfiguration.getProperty(KEY_SMTP_PASS),
                "Could not read sample " + KEY_SMTP_PASS + " property."
        );
    }

    /**
     * Checks that some property values can be parsed as arrays.
     */
    @Test
    @Order(3)
    public void checkPropertyArrays() {
        System.out.println("INFO: Running checkPropertyArrays()...");

        Assertions.assertTrue(
                EmailAgentConfiguration.getPropertyAsArray(KEY_TO_ADDRESS, ",").length > 1,
                "Could not read sample " + KEY_TO_ADDRESS + " property as an array with more than 1 value."
        );

        Assertions.assertTrue(
                EmailAgentConfiguration.getPropertyAsArray(KEY_WHITE_IPS, ",").length > 1,
                "Could not read sample " + KEY_WHITE_IPS + " property as an array with more than 1 value."
        );
    }
}
// End of class.
