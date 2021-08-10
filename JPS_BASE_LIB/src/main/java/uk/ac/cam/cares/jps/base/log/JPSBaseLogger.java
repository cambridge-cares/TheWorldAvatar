package uk.ac.cam.cares.jps.base.log;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Centralised logger handling.
 *
 * Deprecated, use Log4J2 directly.
 */
@Deprecated
public class JPSBaseLogger {

    /**
     * Log a debug statement.
     *
     * @param sender source.
     * @param message statement.
     *
     * @deprecated Use Log4J2 directly instead.
     */
    @Deprecated
    public static void debug(Object sender, String message) {
        if (sender == null || message == null) return;

        Logger logger = LogManager.getLogger(sender.getClass());
        logger.debug(message);
    }

    /**
     * Log an info statement.
     *
     * @param sender source.
     * @param message statement.
     *
     * @deprecated Use Log4J2 directly instead.
     */
    @Deprecated
    public static void info(Object sender, String message) {
        if (sender == null || message == null) return;

        Logger logger = LogManager.getLogger(sender.getClass());
        logger.info(message);
    }

    /**
     * Log an error statement.
     *
     * @param sender source.
     * @param message statement.
     *
     * @deprecated Use Log4J2 directly instead.
     */
    @Deprecated
    public static void error(Object sender, String message) {
        if (sender == null || message == null) return;

        Logger logger = LogManager.getLogger(sender.getClass());
        logger.error(message);
    }

    /**
     * Log an error.
     *
     * @param sender source.
     * @param exc thrown exception.
     *
     * @deprecated Use Log4J2 directly instead.
     */
    @Deprecated
    public static void error(Object sender, Exception exc) {
        if (sender == null || exc == null) return;

        Logger logger = LogManager.getLogger(sender.getClass());
        logger.error(exc.getMessage(), exc);
    }
    
}
// End of class.
