package uk.ac.cam.cares.jps.agent.email;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Enumeration;

import javax.mail.Message;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response;

import org.apache.http.client.methods.HttpGet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentMatchers;
import org.springframework.mock.web.MockHttpServletResponse;

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
    private static String SAMPLE_REQUEST_GOOD;

    /**
     * Sample request data (should be invalid).
     */
    private static String SAMPLE_REQUEST_BAD;

    /**
     * Sample configuration object.
     */
    private static Config CONFIG;

    /**
     * Initialisation before any of the tests run.
     */
    @BeforeAll
    public static void setup() {
        // Read the JSON file mocking a HTTP request
        try {
            SAMPLE_REQUEST_GOOD = Files.readString(Paths.get("./data/sample-request-good.json"));
            Assertions.assertNotNull(SAMPLE_REQUEST_GOOD, "Could not read/parse sample JSON request!");

            SAMPLE_REQUEST_BAD = Files.readString(Paths.get("./data/sample-request-bad.json"));
            Assertions.assertNotNull(SAMPLE_REQUEST_BAD, "Could not read/parse sample JSON request!");

        } catch (IOException | JSONException exception) {
            Assertions.fail("Could not read sample request from JSON file!", exception);
        }

        // Read the properties file (developer expected to provide it at the following location, 
        // it should NOT be committed).
        try {
            CONFIG = spy(Config.class);
            when(CONFIG.getPropertyFileLocation()).thenReturn("./data/example-properties.txt");
            CONFIG.readProperties();

            LOGGER.info("Finished performing test setup.");
        } catch (IOException ioException) {
            Assertions.fail("Could not read properties file!", ioException);
        }
    }

    /**
     * Using a valid request format, this attempts to send an email using the 
     * agent and expects an OK response.Note that this test utilises a mock Handler
     * instance, so no actual email is transmitted.
     */
    @Test
    public void sendGoodTestEmail() {
        LOGGER.debug("Running sendGoodTestEmail()...");

        try {
            // Mock HTTP request
            HttpServletRequest request = spy(HttpServletRequest.class);
            when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
            when(request.getRequestURI()).thenReturn("http://fake-website.com/email-agent/send");
            when(request.getRemoteAddr()).thenReturn("localhost");
            when(request.getParameter(ArgumentMatchers.any())).thenReturn(SAMPLE_REQUEST_GOOD);

            // Mock HTTP response
            HttpServletResponse response = spy(MockHttpServletResponse.class);
            StringWriter strWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(strWriter);
            when(response.getWriter()).thenReturn(printWriter);

            // Mock Handler (so no real emails are sent)
            Handler handler = spy(Handler.class);
            handler.setConfig(CONFIG);

            doNothing().when(handler).sendEmail(
                ArgumentMatchers.any(Message.class),
                ArgumentMatchers.any(HttpServletResponse.class)
            );

            // Run agent
            EmailAgent agent = new EmailAgent(CONFIG, handler);
            agent.doGet(request, response);

            // Check results
            Assertions.assertEquals(
                Response.Status.OK.getStatusCode(),
                response.getStatus(), 
                "Returned HTTP code did not match the expected value!"
            );

            Assertions.assertTrue(
                strWriter.toString().contains("Request forwarded"),
                "Response body did not contain expected string!"
            );
        } catch(Exception exception) {
            Assertions.fail(exception);
        }
    }

    /**
     * Using an invalid request format, this attempts to send an email using the 
     * agent and expects a BAD REQUEST response.
     */
    @Test
    public void sendBadTestEmail() {
        LOGGER.debug("Running sendBadTestEmail()...");

        try {
            // Mock HTTP request
            HttpServletRequest request = spy(HttpServletRequest.class);
            when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
            when(request.getRequestURI()).thenReturn("http://fake-website.com/email-agent/send");
            when(request.getRemoteAddr()).thenReturn("localhost");
            when(request.getParameter("query")).thenReturn(SAMPLE_REQUEST_BAD);

            // Mock HTTP response
            HttpServletResponse response = spy(MockHttpServletResponse.class);
            StringWriter strWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(strWriter);
            when(response.getWriter()).thenReturn(printWriter);

            // Run agent
            EmailAgent agent = new EmailAgent(CONFIG, null);
            agent.doGet(request, response);

            // Check results
            Assertions.assertEquals(
                Response.Status.BAD_REQUEST.getStatusCode(),
                response.getStatus(), 
                "Returned HTTP code did not match the expected value!"
            );

            Assertions.assertTrue(
                strWriter.toString().contains("Must specify"),
                "Response body did not contain expected string!"
            );
        } catch(Exception exception) {
            Assertions.fail(exception);
        }
    }

    /**
     * Tests the ping functionality of the EmailAgent using "localhost" as the sender,
     * should pass the whitelist test.
     */
    @Test
    public void testStatusGood() {
        LOGGER.debug("Running testStatusGood()...");
        
        try {
            // Mock HTTP request
            HttpServletRequest request = spy(HttpServletRequest.class);
            when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
            when(request.getRequestURI()).thenReturn("http://fake-website.com/email-agent/status");
            when(request.getRemoteAddr()).thenReturn("localhost");

            // Mock HTTP response
            HttpServletResponse response = spy(MockHttpServletResponse.class);
            StringWriter strWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(strWriter);
            when(response.getWriter()).thenReturn(printWriter);

            // Run agent as if from HTTP
            EmailAgent agent = new EmailAgent(CONFIG, null);
            agent.doGet(request, response);
            
            Assertions.assertEquals(
                Response.Status.OK.getStatusCode(),
                response.getStatus(), 
                "Returned HTTP code did not match the expected value!"
            );

            Assertions.assertTrue(
                strWriter.toString().contains("Ready to serve"),
                "Response body did not contain expected string!"
            );
        } catch(Exception exception) {
            Assertions.fail(exception);
        }
    }

    /**
     * Tests the ping functionality of the EmailAgent using "69.69.69.69" as the sender,
     * should fail the whitelist test.
     */
    @Test
    public void testStatusBad() {
        LOGGER.debug("Running testStatusBad()...");
        
        try {
            // Mock HTTP request
            HttpServletRequest request = spy(HttpServletRequest.class);
            when(request.getMethod()).thenReturn(HttpGet.METHOD_NAME);
            when(request.getRequestURI()).thenReturn("http://fake-website.com/email-agent/status");
            when(request.getRemoteAddr()).thenReturn("69.69.69.69");

            // Mock HTTP response
            HttpServletResponse response = spy(MockHttpServletResponse.class);
            StringWriter strWriter = new StringWriter();
            PrintWriter printWriter = new PrintWriter(strWriter);
            when(response.getWriter()).thenReturn(printWriter);

            // Run agent as if from HTTP
            EmailAgent agent = new EmailAgent(CONFIG, null);
            agent.doGet(request, response);
            
            // Check results
            Assertions.assertEquals(
                Response.Status.FORBIDDEN.getStatusCode(),
                response.getStatus(), 
                "Returned HTTP code did not match the expected value!"
            );

            Assertions.assertTrue(
                strWriter.toString().contains("Unauthorised"),
                "Response body did not contain expected string!"
            );
        } catch(Exception exception) {
            Assertions.fail(exception);
        }
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
