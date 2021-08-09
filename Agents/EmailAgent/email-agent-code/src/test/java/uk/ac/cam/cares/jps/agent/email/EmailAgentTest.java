package uk.ac.cam.cares.jps.agent.email;

import uk.ac.cam.cares.jps.agent.email.mock.MockHttpServletRequest;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Paths;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * Tests the functionality of the EmailAgent class.
 *
 * @author Michael Hillman
 */
public class EmailAgentTest {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(EmailAgentTest.class);

    /**
     * Sample request data (should be valid).
     */
    private static JSONObject SAMPLE_REQUEST_GOOD;

    /**
     * Sample request data (should be invalid).
     */
    private static JSONObject SAMPLE_REQUEST_BAD;

    /**
     * Initialisation before any of the tests run.
     */
    @BeforeAll
    public static void setup() {
        // Read the JSON file mocking a HTTP request
        try {
            String goodContent = Files.readString(Paths.get("./data/sample-request-good.json"));
            SAMPLE_REQUEST_GOOD = new JSONObject(goodContent);
            Assertions.assertNotNull(SAMPLE_REQUEST_GOOD, "Could not read/parse sample JSON request!");

            String badContent = Files.readString(Paths.get("./data/sample-request-bad.json"));
            SAMPLE_REQUEST_BAD = new JSONObject(badContent);
            Assertions.assertNotNull(SAMPLE_REQUEST_BAD, "Could not read/parse sample JSON request!");

        } catch (IOException | JSONException exception) {
            Assertions.fail("Could not read sample request from JSON file!", exception);
        }

        // Read the properties file (developer expected to provide it at the following location, 
        // it should NOT be committed).
        try {
            EmailAgentConfiguration.readProperties("./data/example-properties.txt");
        } catch (IOException ioException) {
            Assertions.fail("Could not read properties file!", ioException);
        }
    }

    /**
     * Using the good sample request file and the properties file (to be provided by the developer),
     * this attempts to send an email using the EmailHandler class.
     *
     * Note: This test has been disabled as it requires a properties file with real SMTP credentials
     * to be present. At the time of writing, this is difficult to include in automated testing
     * environments.
     */
    //@Test
    public void sendGoodTestEmail() {
        LOGGER.debug("Running sendGoodTest()...");

        // New agent
        EmailAgent agent = new EmailAgent();

        // Pass in request and get result
        JSONObject result = agent.processRequestParameters(SAMPLE_REQUEST_GOOD, new MockHttpServletRequest());

        boolean hasStatus = !result.isNull("status");
        Assertions.assertTrue(hasStatus, "Expected a 'status' field from JSON result!");

        String returnCode = result.get("status").toString();
        Assertions.assertEquals("200", returnCode, "Expected JSON status to be '200'!");
    }

    /**
     * Using the bad sample request file and the properties file (to be provided by the developer),
     * this attempts to send an email using the EmailHandler class.
     */
    @Test
    public void sendBadTestEmail() {
        LOGGER.debug("Running sendBadTestEmail()...");

        // New agent
        EmailAgent agent = new EmailAgent();

        // Note: This should really through a BadRequestException, but for some reason the JPS
        // Base Library actually throws a RuntimeException
        Assertions.assertThrows(RuntimeException.class, () -> {
            // Pass in request and get result
            agent.processRequestParameters(SAMPLE_REQUEST_BAD, new MockHttpServletRequest());
            Assertions.fail("Bad request passed in, expected an Exception to be thrown!");
        }, "Expected a BadRequestException to be thrown!");
    }

    /**
     * Tests the ping functionality of the EmailAgent.
     */
    @Test
    public void testPing() {
        LOGGER.debug("Running testPing()...");
        
        // New agent
        EmailAgent agent = new EmailAgent();

        // Ping request
        JSONObject request = new JSONObject();
        request.put("ping", "true");

        // Pass in request and get result
        JSONObject result = agent.processRequestParameters(request, new MockHttpServletRequest());

        boolean hasStatus = !result.isNull("status");
        Assertions.assertTrue(hasStatus, "Expected a 'status' field from JSON result!");

        String returnCode = result.get("status").toString();
        Assertions.assertEquals("200", returnCode, "Expected JSON status to be '200'!");

        boolean hasDescription = !result.isNull("description");
        Assertions.assertTrue(hasDescription, "Expected a 'description' field from JSON result!");
    }

    /**
     * Tests the ability of the InetAddress class to detect local IPs, this is esoteric so required
     * a quick test.
     */
    @Test
    public void setInetAddress() {
        LOGGER.debug("Running setInetAddress()...");
        
        // Should be reported as local addresses
        String[] localIPs = new String[]{
            "localhost",
            "127.0.0.1",
            "192.168.0.1",
            "172.16.0.0",
            "10.0.0.0",
            "0:0:0:0:0:0:0:1"
        };

        for (String localIP : localIPs) {
            try {
                InetAddress address = InetAddress.getByName(localIP);
                boolean isLocal = address.isLinkLocalAddress()
                        || address.isLoopbackAddress()
                        || address.isSiteLocalAddress();

                Assertions.assertTrue(isLocal, "Expected IP address '" + localIP + "' to be reported as local!");
            } catch (UnknownHostException exception) {
                Assertions.fail("Not a valid IP address!", exception);
            }
        }
    }

}
// End of class.
