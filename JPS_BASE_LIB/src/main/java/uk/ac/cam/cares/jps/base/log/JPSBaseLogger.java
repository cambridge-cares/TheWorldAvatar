package uk.ac.cam.cares.jps.base.log;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JPSBaseLogger {

	private static Logger logger = LoggerFactory.getLogger(JPSBaseLogger.class);
	
	public static void debug(Object sender, String message) {
		logger.debug(sender.getClass().getSimpleName() + " " + message);
	}
	
	public static void info(Object sender, String message) {
		logger.info(sender.getClass().getSimpleName() + " " + message);
	}
	
	public static void error(Object sender, String message) {
		logger.error(sender.getClass().getSimpleName() + " " + message);
	}
	
	public static void error(Object sender, Exception exc) {
		logger.error(sender.getClass().getSimpleName() + " " + exc.getMessage() + "\n" + exc.getStackTrace());
	}
}
