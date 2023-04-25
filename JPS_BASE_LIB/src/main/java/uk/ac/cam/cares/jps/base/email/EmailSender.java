package uk.ac.cam.cares.jps.base.email;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.URL;
import java.net.UnknownHostException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Optional;
import java.util.Properties;

import org.json.JSONObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

/**
 * This class handles sending HTTP requests to a remote EmailAgent instance to facilitate the
 * sending of an automated email.
 *
 * @author Michael Hillman (mdhillman<@>cmclinnovations.com)
 */
public class EmailSender {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(EmailSender.class);

    /**
     * Contains meta-data to append to email content.
     */
    private final Properties metadata;

    /**
     * Base URL for remote EmailAgent instance.
     */
    private String emailAgentURL;

    /**
     * Denotes if the remote EmailAgent is reachable and available.
     */
    private boolean isAgentReachable = false;

    /**
     * Create a new EmailSender instance.
     */
    public EmailSender() {
        this(null);
    }

    /**
     * Create a new EmailSender instance.
     * 
     * @param emailAgentURL URL of remote EmailAgent instance.
     */
    public EmailSender(String emailAgentURL) {
        this.emailAgentURL = emailAgentURL;
        this.metadata = new Properties();

        // Can we reach the remote EmailAgent?
        determineAgentLocation();
        this.isAgentReachable = isReachable();

        // Generate meta-data to add to email
        gatherMetaData();
    }

    /**
     * Attempts to send a HTTP request to the remote EmailAgent instance using the input content.
     *
     * Note: if the remote EmailAgent cannot be reached, or returns an error code, then the message
     * content is written to a local file instead.
     *
     * @param subject email subject
     * @param body email body
     *
     * @return Optionally returns location of log file (if written to).
     *
     * @throws Exception if not initialised, email cannot be sent or written to local file.
     */
    public Optional<Path> sendEmail(String subject, String body) throws Exception {
        // Append the meta-data to the email body
        StringBuilder strBuilder = new StringBuilder(body);
        strBuilder.append("<br><br>");
        strBuilder.append("The following meta-data was gathered from the machine triggering this notification:");
        strBuilder.append("<br>");

        for (Object key : metadata.keySet()) {
            strBuilder.append("&#9;").append(key).append(": ").append(metadata.get(key));
            strBuilder.append("<br>");
        }

        // Determine whether to send email or write to file
        if (!isAgentReachable) {
            // Write to file.
            LOGGER.warn("Could not reach remote EmailAgent, writing message to a local file instead.");
            return Optional.of(writeToFile(subject, strBuilder.toString()));

        } else {
            // Send HTTP request
            try {
                makeRequest(subject, strBuilder.toString());
                return Optional.empty();
            } catch (Exception exception) {
                return Optional.of(writeToFile(subject, strBuilder.toString()));
            }
        }
    }

    /**
     * Makes the HTTP request to the remote EmailAgent.
     *
     * @param subject email subject
     * @param body email body
     */
    private void makeRequest(String subject, String body) {
        JSONObject request = new JSONObject();
        request.put("subject", subject);
        request.put("body", body);

        try {
            // Make the HTTP request
            String submitURL = (emailAgentURL.endsWith("/")) ? emailAgentURL + "send" : emailAgentURL + "/send";
            String result = AgentCaller.executeGetWithURLAndJSON(submitURL, request.toString());

            if (result.contains("Request forwarded")) {
                // Success
                LOGGER.info("EmailAgent reports successful request, check recipient mailbox.");

            } else {
                // Failure
                JSONObject resultJSON = new JSONObject(result);
                LOGGER.warn("Remote EmailAgent instance reports issues, cannot send email!");
                LOGGER.warn(resultJSON.get("description"));
            }
        } catch (Exception exception) {
            LOGGER.error("Exception when attempting to contact remote EmailAgent instance, will write to file instead.", exception);
        }
    }

    /**
     * Writes the email content to file.
     *
     * @param subject email subject.
     * @param body email body.
     *
     * @throws IOException if file could not be written to.
     */
    private Path writeToFile(String subject, String body) throws IOException {
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss-SSSS");
        String datetime = dateFormat.format(new Date());

        String fileName = "EmailSender_" + datetime + ".log";
        Path logFile = Paths.get(fileName);

        try ( BufferedWriter writer = new BufferedWriter(new FileWriter(logFile.toFile()))) {
            writer.append("Subject: ").append(subject);
            writer.append("\n");
            writer.append("Body:\n");
            writer.append(body);
        }

        LOGGER.info("Message written to file at {}", logFile.toAbsolutePath());
        return logFile;
    }

    /**
     * Attempts to contact the remote EmailAgent, returns true if it can be reached and responds as
     * available.
     *
     * @return remote EmailAgent is reachable.
     */
    public boolean isReachable() {
        try {
            // Make the HTTP request
            String statusURL = (emailAgentURL.endsWith("/")) ? emailAgentURL + "status" : emailAgentURL + "/status";
            String result = AgentCaller.executeGetWithURL(statusURL);

            // Check result contents
            if (result.contains("Ready to serve")) {
                return true;
            }
        } catch (Exception exception) {
            LOGGER.error("Exception when trying to determine if remote EmailAgent is reachable.", exception);
        }
        return false;
    }

    /**
     * If not already passed in, attempts to read the EmailAgent's URL from an environment variable
     * (that should have been set using Docker).
     */
    private void determineAgentLocation() {
        if(emailAgentURL == null) {
            String variable = System.getenv("EMAIL_AGENT_URL");

            if (variable == null || variable.trim().isEmpty()) {
                LOGGER.error("Could not find EMAIL_AGENT_URL variable, cannot continue");
                isAgentReachable = false;
            } else {
                emailAgentURL = variable;
                LOGGER.info("Found EMAIL_AGENT_URL variable, remote location is: {}", emailAgentURL);
            }
        }
    }

    /**
     * Gathers meta-data what will be appended to the email content.
     */
    private void gatherMetaData() {
        // Hostname
        String hostname = null;
        try {
            hostname = InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException exception) {
            hostname = "Unknown";
        } finally {
            metadata.put("Hostname", hostname);
        }

        // Local IP
        String localIP = null;
        try {
            localIP = InetAddress.getLocalHost().getHostAddress();
        } catch (UnknownHostException exception) {
            localIP = "Unknown";
        } finally {
            metadata.put("Local IP Address", localIP);
        }

        // Public IP
        String publicIP = null;
        try {
            publicIP = getPublicIP();
        } catch (Exception exception) {
            publicIP = "Unknown";
        } finally {
            metadata.put("Public IP Address", publicIP);
        }

        // Submission time
        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSSS");
        String datetime = dateFormat.format(new Date());
        metadata.put("Submission Time", datetime);
    }

    /**
     * Uses a remote service to get the current public IP.
     *
     * @return public IP
     */
    private String getPublicIP() throws Exception {
        URL url = new URL("http://checkip.amazonaws.com");

        try ( BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()))) {
            return reader.readLine();
        }
    }

}
// End of class.
