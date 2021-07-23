package uk.ac.cam.cares.jps.base.email;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import org.slf4j.LoggerFactory;

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
import org.slf4j.Logger;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;

/**
 * This class handles sending HTTP requests to a remote EmailAgent instance to facilitate the
 * sending of an automated email.
 *
 * @author Michael Hillman
 */
public class EmailSender {

    /**
     * For error output.
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(EmailSender.class);

    /**
     * Fallback URL for the remote email agent.
     */
    private static final String DEFAULT_AGENT_URL = "http://kg.cmclinnovations.com/agents/email-agent";

    /**
     * Contains meta-data to append to email content.
     */
    private final Properties metadata;

    /**
     * URL for remote EmailAgent instance.
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
     * Note: if this machine is not running on the same network as the EmailAgent instance (or the
     * remote EmailAgent cannot be reached), then the same content is written to a local file
     * instead.
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
            strBuilder.append("&#9;" + key.toString() + ": " + metadata.get(key).toString());
            strBuilder.append("<br>");
        }

        // Determine whether to send email or write to file
        if (!isAgentReachable) {
            // Write to file.
            LOGGER.info("Not running at CMCL/EmailAgent is unavailable, will write email to file instead.");
            return Optional.of(writeToFile(subject, strBuilder.toString()));

        } else {
            // Send HTTP request
            makeRequest(subject, strBuilder.toString());
        }

        return Optional.empty();
    }

    /**
     * Makes the HTTP request to the remote EmailAgent.
     *
     * @param subject email subject
     * @param body email body
     *
     * @throws Exception if request could not be made.
     */
    private void makeRequest(String subject, String body) throws Exception {
        JSONObject request = new JSONObject();
        request.put("subject", subject);
        request.put("body", body);

        try {
            // Make the HTTP request
            String result = AgentCaller.executeGetWithURLAndJSON(emailAgentURL, request.toString());

            if (result.contains("200") && result.contains("success")) {
                // Success
                LOGGER.info("EmailAgent reports successful request, will send email.");

            } else {
                // Failure
                JSONObject resultJSON = new JSONObject(result);

                LOGGER.error("EmailAgent reports issues, cannot send email!");
                LOGGER.error("  Status: " + resultJSON.get("status"));
                LOGGER.error("  Description: " + resultJSON.get("description"));
            }
        } catch (Exception exception) {
            LOGGER.warn("Could not contact remote EmailAgent instance.", exception);
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
            writer.append("Subject: " + subject);
            writer.append("\n");
            writer.append("Body:\n");
            writer.append(body);
        }

        LOGGER.info("Email content written to file at: " + logFile.toAbsolutePath().toString());
        return logFile;
    }

    /**
     * Attempts to contact the remote EmailAgent, returns true if it can be reached and responds as
     * available.
     *
     * @return remote EmailAgent is reachable.
     */
    private boolean isReachable() {
        try {
            // Make the HTTP request
            String result = AgentCaller.executeGetWithURLAndJSON(emailAgentURL, "{ \"ping\": \"true\" }");

            // Check result contents
            if (result.contains("200")) {
                return true;
            }
        } catch (Exception exception) {
            LOGGER.warn("Could not contact remote EmailAgent instance.", exception);
        }
        return false;
    }

    /**
     * Attempts to read the EmailAgent's URL from an environment variable (that should have been set
     * using Docker). The DEFAULT_AGENT_URL is used if this cannot be found.
     *
     * TODO - In future, this could be replaced with an Agent discovery solution, or could contact
     * the routing table to determine the correct location.
     */
    private void determineAgentLocation() {
        String variable = System.getenv("EMAIL_AGENT_URL");

        if (variable == null || variable.isBlank()) {
            emailAgentURL = DEFAULT_AGENT_URL;
            LOGGER.warn("Could not find EMAIL_AGENT_URL variable, using fallback URL: " + emailAgentURL);
        } else {
            emailAgentURL = variable;
            LOGGER.info("Found EMAIL_AGENT_URL variable, remote location is: " + emailAgentURL);
        }
    }

    /**
     * Gathers meta-data what will be appended to the email content.
     */
    private void gatherMetaData() {
        // Hostname
        String hostname = "Unknown";
        try {
            hostname = InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException exception) {
            LOGGER.warn("Could not determine host name.");
        } finally {
            metadata.put("Hostname", hostname);
        }

        // Local IP
        String localIP = "Unknown";
        try {
            localIP = InetAddress.getLocalHost().getHostAddress();
        } catch (UnknownHostException exception) {
            LOGGER.warn("Could not determine local IP address.");
        } finally {
            metadata.put("Local IP Address", localIP);
        }

        // Public IP
        String publicIP = "Unknown";
        try {
            publicIP = getPublicIP();
        } catch (Exception exception) {
            LOGGER.warn("Could not determine public IP address.");
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
