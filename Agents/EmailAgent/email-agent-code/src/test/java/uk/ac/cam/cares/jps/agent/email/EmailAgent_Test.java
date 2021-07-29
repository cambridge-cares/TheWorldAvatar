package uk.ac.cam.cares.jps.agent.email;

import uk.ac.cam.cares.jps.agent.email.mock.MockHttpServletRequest;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Paths;
import javax.ws.rs.BadRequestException;
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
public class EmailAgent_Test {

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
            EmailAgentConfiguration.readProperties("./data/email-agent.properties");
        } catch (IOException ioException) {
            Assertions.fail("Could not read properties file!", ioException);
        }
    }

    /**
     * Using the good sample request file and the properties file (to be provided by the developer),
     * this attempts to send an email using the EmailHandler class.
     *
     * Note that this test will generate and email and send it if the properties file has been
     * correctly configured to point towards an SMTP server.
     */
    @Test
    public void sendGoodTestEmail() {
        System.out.println("INFO: Running sendGoodTestEmail()...");

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
        System.out.println("INFO: Running sendBadTestEmail()...");

        // New agent
        EmailAgent agent = new EmailAgent();

        // Check that a bad request throws the expected exception
        Assertions.assertThrows(BadRequestException.class, () -> {
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
        System.out.println("INFO: Running testPing()...");

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
     * Proves that the isAnyLocalAddress() method of the InetAddress interface is not sufficient
     * enough to determine a reliable local IP address.
     */
    //@Test - Only needed as a proof during development.
    public void setInetAddress() {
        // Should be reported as local addresses
        String[] localIPs = new String[]{
            "localhost",
            "127.0.0.1",
            "198.168.0.1",
            "172.16.0.0",
            "10.0.0.0",
            "0:0:0:0:0:0:0:1"
        };

        for (String localIP : localIPs) {
            try {
                boolean isLocal = InetAddress.getByName(localIP).isAnyLocalAddress();
                Assertions.assertTrue(isLocal, "Expected IP address '" + localIP + "' to be reported as local!");
            } catch (UnknownHostException exception) {
                Assertions.fail("Not a valid IP address!", exception);
            }
        }
    }

}
// End of class.
