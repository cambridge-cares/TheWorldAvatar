package uk.ac.cam.cares.jps.base.email;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;

/**
 * Tests the functionality of the EmailSender class.
 * See the EmailAgent source code for details on how to test
 *
 * @author Michael Hillman (mdhillman<@>cmclinnovations.com)
 */
public class EmailSenderTest {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(EmailSenderTest.class);

    /**
     * Base URL of remote EmailAgent to use in test.
     */
    private static final String EMAIL_AGENT_URL = "http://localhost:8080/email_agent";

    /**
     * Tests that an email can be submitted to the EmailSender and is written to a
     * local log file.
     */
    @Test
    public void writeToFile() {
        // Initialise new EmailSender
        EmailSender sender = new EmailSender("http://fake-website/email-agent/");

        // Use a junk environment variable so that the EmailSender cannot reach a remote
        // EmailAgent instance and falls back to creating a log file.
        try {

            // Email contents
            String subject = "Test email from the EmailSender_Test.writeToFile() method.";
            String body = "This test email should fail and get written to a local log file.";

            // Attempt to send an email
            Optional<Path> logFile = sender.sendEmail(subject, body);

            // Should return a log file
            Assertions.assertTrue(logFile.isPresent(), "Expected an log file to be returned!");

            // Should exist on disk
            Assertions.assertTrue(Files.exists(logFile.get()), "Expected to find log file on disk!");

            // Remove the generated log file
            Assertions.assertTrue(Files.deleteIfExists(logFile.get()), "Could not delete the generated log file!");

        } catch (Exception exception) {
            Assertions.fail("Could not mock environment variables for unit test!", exception);
        }
    }

    /**
     * Attempts to contact a remote EmailAgent instance and submit a message.
     */
    @Test
    @Disabled("See class documentation")
    public void submitJob() {
        // Configure a message
        String subject = "Testing the EmailAgent";
        String body = "This is a test of the EmailAgent's functionality to recieve job submissions "
                + "and forward them to a remote SMTP server for delivery. If you're not the originator of "
                + "this job, please ignore this message.";

        // Use JPS base library code to submit job
        EmailSender sender = new EmailSender(EMAIL_AGENT_URL);

        try {
            Optional<Path> logFile = sender.sendEmail(subject, body);
            LOGGER.info("Email sent successfully, please check recipient's mailbox.");
            if (logFile.isPresent()) {
                Files.deleteIfExists(logFile.get());
            }
        } catch (Exception exception) {
            LOGGER.error("Could not contact remote EmailAgent!", exception);
        }
    }

    /**
     * Attempts to contact a remote EmailAgent instance and get the status
     */
    @Test
    @Disabled("See class documentation")
    public void submitPing() {
        // Use JPS base library code to submit job
        EmailSender sender = new EmailSender(EMAIL_AGENT_URL);

        try {
            boolean reachable = sender.isReachable();
            LOGGER.info("Pinged the remote EmailAgent, status was {}", reachable);
        } catch (Exception exception) {
            LOGGER.error("Could not contact remote EmailAgent!", exception);
        }
    }
}
// End of class.
