package uk.ac.cam.cares.jps.agent.email;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.time.temporal.ChronoUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.Appender;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.appender.ConsoleAppender;
import org.apache.logging.log4j.core.appender.RollingFileAppender;
import org.apache.logging.log4j.core.config.Configuration;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import uk.ac.cam.cares.jps.base.util.SysStreamHandler;

/**
 * Test logging functionality.
 *
 * @author Michael Hillman
 */
public class LoggingTest {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(LoggingTest.class);

    /**
     * Checks to see that the root Log4J logger is configured as the properties files denotes.
     * This should log to the console AND to a file in the ~/.jps/logs directory.
     * 
     * @throws IOException
     */
    @Test
    public void checkLogging() throws IOException {
        LoggerContext loggerContext = (LoggerContext) LogManager.getContext();
        Assertions.assertNotNull(loggerContext, "Current LoggerContext is null.");
        
        Configuration loggerConfig = loggerContext.getConfiguration();
        Assertions.assertNotNull(loggerConfig, "Current configuration is null.");

        // Check for file appender
        Appender fileAppender = loggerConfig.getAppender("RollingFile");
        Assertions.assertNotNull(fileAppender, "Expected to find an appender named 'RollingFile'.");
        Assertions.assertTrue(fileAppender instanceof RollingFileAppender, "Expecting 'RollingFile' appender to be an instance of RollingFileAppender class.");

        // Check for console appender
        Appender consoleAppender = loggerConfig.getAppender("Console");
        Assertions.assertNotNull(consoleAppender, "Expected to find an appender named 'Console'.");
        Assertions.assertTrue(consoleAppender instanceof ConsoleAppender, "Expecting 'Console' appender to be an instance of ConsoleAppender class.");

        // Redirect the system streams
        SysStreamHandler.redirectToLoggers();
        
        // Output some logging statements
        LOGGER.debug("This is a DEBUG level test message, from the LoggingTest class.");
        LOGGER.info("This is an INFO level test message, from the LoggingTest class.");
        LOGGER.warn("This is a WARN level test message, from the LoggingTest class.");
        LOGGER.error("This is an ERROR level test message, from the LoggingTest class.");
        LOGGER.fatal("This is a FATAL level test message, from the LoggingTest class.");
        System.out.println("This is a System.out test message, from the LoggingTest class.");
        System.err.println("This is a System.err test message, from the LoggingTest class.");

        // Test that the log file exists
        Path logFile = Paths.get(System.getProperty("user.home"), "/.jps/logs/jps.log");
        Assertions.assertTrue(Files.exists(logFile), "Could not find expected log file on disk!");

        // Check that the file has been modified within 30 seconds
        BasicFileAttributes attrs = Files.readAttributes(logFile, BasicFileAttributes.class);
        Instant modifiedTime = attrs.lastModifiedTime().toInstant();
        Instant now = Instant.now();
        long seconds = Math.abs(ChronoUnit.SECONDS.between(modifiedTime, now));

        Assertions.assertTrue(seconds < 30, "Log file has not been updated in last 30 seconds!");
    }

}
// End of class.
