package uk.ac.cam.cares.jps.base.email;

import com.github.stefanbirkner.systemlambda.SystemLambda;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Tests the functionality of the EmailSender class.
 *
 * @author Michael Hillman
 */
public class EmailSender_Test {

    /**
     * Tests that an email can be submitted to the EmailSender and is passed on to the remote
     * EmailAgent instance.
     *
     * Note: This test requires that you have set the EMAIL_AGENT_URL environment variable to point
     * towards a running instance of the EmailAgent service.
     */
    @Test
    //@Ignore("Will not pass unless EmailAgent is running and environment variables have been set.")
    public void sendEmail() {
        EmailSender sender = new EmailSender();

        try {
            // Email contents
            String subject = "Automated email from jps-base-lib unit tests.";
            String body = "Is the user has configured their local environment correctly, then this email"
                    + "should be forwarded onto the EmailAgent instance for submission.";

            // Attempt to send an email
            Optional<Path> logFile = sender.sendEmail(subject, body);

            // Should NOT return a log file
            Assertions.assertTrue(logFile.isEmpty(), "Did not expect a log file to be returned!");
        } catch (Exception exception) {
            Assertions.fail("Could not mock environment variables for unit test!", exception);
        }
    }

    /**
     * Tests that an email can be submitted to the EmailSender and is written to a local log file.
     */
    //@Test
    public void writeToFile() {
        // Initialise new EmailSender
        EmailSender sender = new EmailSender();

        // Use a junk environment variable so that the EmailSender cannot reach a remote
        // EmailAgent instance and falls back to creating a log file.
        try {
            SystemLambda.withEnvironmentVariable("EMAIL_AGENT_URL", "foobar").execute(() -> {
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
            });
        } catch (Exception exception) {
            Assertions.fail("Could not mock environment variables for unit test!", exception);
        }
    }
}
// End of class.
