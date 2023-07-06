package uk.ac.cam.cares.jps.agent.email;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Properties;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * This class handles reading the configuration file for the EmailAgent and provides methods to
 * retrieve property values by their keys.
 *
 * @author Michael Hillman (mdhillman<@>cmclinnovations.com)
 */
public class Config {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(Config.class);

    /**
     * Name of the environment variable pointing to the location of the properties file.
     */
    private static final String PROPERTIES_ENV = "EMAIL_AGENT_PROPERTIES";

    /**
     * Key for SMTP host setting.
     */
    public static final String KEY_SMTP_HOST = "smtp.host";

    /**
     * Key for SMTP password.
     */
    public static final String KEY_SMTP_PASS = "smtp.pass";

    /**
     * Key for SMTP port setting.
     */
    public static final String KEY_SMTP_PORT = "smtp.port";

    /**
     * Key for SSL setting.
     */
    public static final String KEY_SSL_ENABLE = "smtp.ssl.enable";

    /*
     * Key for StartTLS setting.
     */
    public static final String KEY_STARTTLS_ENABLE = "smtp.starttls.enable";

    /**
     * Key for SMTP auth setting.
     */
    public static final String KEY_SMTP_AUTH = "smtp.auth";

    /**
     * Key for Subject prefix setting.
     */
    public static final String KEY_SUBJECT_PREFIX = "subject.prefix";

    /**
     * Key for destination address.
     */
    public static final String KEY_TO_ADDRESS = "to.address";

    /**
     * Key for source address.
     */
    public static final String KEY_FROM_ADDRESS = "from.address";

    /**
     * Key for white list status.
     */
    public static final String KEY_WHITE_ONLY = "whitelist.enabled";

    /**
     * Key for white list IPs
     */
    public static final String KEY_WHITE_IPS = "whitelist.ips";

    /**
     * Live properties object.
     */
    private Properties properties;

    /**
     * Constructor.
     */
    public Config() {
        // Empty
    }

    /**
     * Uses the environment variable to get the location of the property file.
     */
    public String getPropertyFileLocation() {
        return System.getenv(PROPERTIES_ENV);
    }
    
    /**
     * Returns the property represented by the input key as a string (or null if not found).
     *
     * @param key property key
     *
     * @return property value (or null)
     *
     * @throws IllegalStateException if properties has not been read
     */
    public String getProperty(String key) {
        if (properties == null) {
            throw new IllegalStateException("Properties file has not been loaded!");
        }

        return properties.getProperty(key, null);
    }

    /**
     * Returns the property represented by the input key as token separated array (or null if not
     * found).
     *
     * @param key property key
     * @param delimiter token to split on
     *
     * @return property value array (or null)
     *
     * @throws IllegalStateException if properties has not been read
     */
    public String[] getPropertyAsArray(String key, String delimiter) {
        if (properties == null) {
            throw new IllegalStateException("Properties file has not been loaded!");
        }

        if (properties.containsKey(key)) {
            return properties.getProperty(key).split(delimiter);
        }
        return new String[0];
    }

    /**
     * Reads the default properties file.
     *
     * @throws IOException if properties file cannot be read.
     */
    public void readProperties() throws IOException {
        properties = new Properties();

        String propertyFileLocation = getPropertyFileLocation();
        LOGGER.info("Reading properties file at: {}", propertyFileLocation);

        try ( FileInputStream file = new FileInputStream(propertyFileLocation)) {
            properties.load(file);

            // Log if the white list mode is enabled (default is not set should be false)
            boolean whitelistOn = Boolean.parseBoolean(getProperty(KEY_WHITE_ONLY));

            if (whitelistOn) {
                String[] allowedIPs = this.getPropertyAsArray(KEY_WHITE_IPS, ",");

                if (allowedIPs != null) {
                    LOGGER.info("Whitelist enabled, only approving requests from local machine and following IPs...");
                    LOGGER.info("{}", String.join(", ", allowedIPs));
                }
            } else {
                LOGGER.warn("Whitelist disabled, accepting all requests.");
            }
        }
    }

    /**
     * Reads the input properties file.
     *
     * @param propertiesFile location of properties file
     *
     * @throws IOException if properties file cannot be read.
     */
    public void readProperties(String propertiesFile) throws IOException {
        properties = new Properties();

        try ( FileInputStream file = new FileInputStream(propertiesFile)) {
            properties.load(file);
        }
    }

}
// End of clas.
