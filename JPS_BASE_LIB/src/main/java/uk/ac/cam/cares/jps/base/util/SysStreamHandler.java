package uk.ac.cam.cares.jps.base.util;

import java.io.PrintStream;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.Level;

/**
 * Handles standard streams for logging.
 *
 * @author Michael Hillman
 */
public class SysStreamHandler {

    /**
     * Logger.
     */
    private static final Logger LOGGER = LogManager.getLogger(SysStreamHandler.class);

    /**
     * Redirects the Standard Out stream to a Log4j2 logger using the DEBUG level, and the Standard
     * Err stream to a Log4j2 logger using the ERROR level.
     */
    public static void redirectToLoggers() {
        System.setOut(createLoggingProxy(System.out, Level.DEBUG));
        System.setErr(createLoggingProxy(System.err, Level.ERROR));
    }

    /**
     * Create a PrintStream that redirects to the logger.
     *
     * @param realPrintStream original print stream.
     * @param level logger level.
     *
     * @return print stream that redirects to logger.
     */
    private static PrintStream createLoggingProxy(PrintStream realPrintStream, Level level) {
        return new PrintStream(realPrintStream) {
            @Override
            public void print(final String string) {
                LOGGER.log(level, string);
            }

            @Override
            public void println(final String string) {
                LOGGER.log(level, string);
            }
        };
    }

}
// End of class.
