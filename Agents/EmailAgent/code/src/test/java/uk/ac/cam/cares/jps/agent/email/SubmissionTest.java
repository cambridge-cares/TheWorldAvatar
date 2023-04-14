package uk.ac.cam.cares.jps.agent.email;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.junit.Ignore;
import org.junit.jupiter.api.MethodOrderer.OrderAnnotation;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;

import uk.ac.cam.cares.jps.base.email.EmailSender;

/**
 * This class contains a disabled unit test that has been added as a demonstration
 * of how a developer can contact a remote EmailAgent instance within their own
 * agent to submit an email for delivery.
 * 
 * If desired, settings can be changed here, and the @Ignore annotation removed
 * to actually run the test and submit an email. Please remember NOT to commit 
 * any of these temporary changes though.
 *
 * @author Michael Hillman (mdhillman<@>cmclinnovations.com)
 */
public class SubmissionTest {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(SubmissionTest.class);

    /**
     * Base URL of remote EmailAgent to use in test.
     */
    private static final String EMAIL_AGENT_URL = "http://fake-website.com/email-agent";
    
    /**
     * Attempts to contact a remote EmailAgent instance and submit a message.
     */
    @Test
    @Ignore("See class documentation")
    public void submitJob() {
        // Configure a message
        String subject = "Testing the EmailAgent";
        String body = """
            This is a test of the EmailAgent's functionality to recieve job submissions and forward them
            to a remote SMTP server for delivery. If you're not the originator of this job, please ignore
            this message.
        """;
         
        // Use JPS base library code to submit job
        EmailSender sender = new EmailSender(EMAIL_AGENT_URL);

        try {
            sender.sendEmail(subject, body);
            LOGGER.info("Email sent successfully, please check recipient's mailbox.");
        } catch(Exception exception) {
            LOGGER.error("Could not contact remote EmailAgent!", exception);
        }
    }

}
// End of class.