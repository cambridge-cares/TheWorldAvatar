package com.cmclinnovations.email;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import org.json.JSONObject;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * Tests the functionality of the EmailHandler class.
 *
 * @author Michael Hillman
 */
public class EmailHandler_Test {

    /**
     * Sample request data
     */
    private static JSONObject SAMPLE_REQUEST;

    /**
     * Initialisation before any of the tests run.
     */
    @BeforeAll
    public static void setup() {
        // Read the JSON file mocking a HTTP request
        try {
            String content = Files.readString(Paths.get("test-data/sample-request.json"));
            SAMPLE_REQUEST = new JSONObject(content);

            Assertions.assertNotNull(SAMPLE_REQUEST, "Could not read/parse sample JSON request!");

        } catch (Exception exception) {
            Assertions.fail("Could not read sample request from JSON file!");
        }

        // Read the properties file (developer expected to provide it at the following location, 
        // it should NOT be committed).
        try {
            EmailAgentConfiguration.readProperties("test-data/email-agent.properties");
        } catch (IOException ioException) {
            Assertions.fail("Could not read properties file!");
        }
    }

    /**
     * Using the sample request file (to be provided by the developer), this attempts to send an
     * email using the EmailHandler class.
     */
    @Test
    public void sendTestEmail() {
        // Get expected properties
        String subject = SAMPLE_REQUEST.getString("subject");
        String body = SAMPLE_REQUEST.getString("body");

        // Try sending an email
        JSONObject result = EmailHandler.submitEmail(subject, body);
        Assertions.assertNotNull(result, "Exepcted a JSON response from EmailHandler class!");
        
        String status = result.get("status").toString();
        Assertions.assertNotNull(status, "Expected a 'status' field from JSON result!");
        
        boolean success = status.equals("200");
        Assertions.assertTrue(success, "Expected JSON status to be '200'!");
    }

}
// End of class.
