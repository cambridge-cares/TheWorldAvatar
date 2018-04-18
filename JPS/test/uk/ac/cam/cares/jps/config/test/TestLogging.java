package uk.ac.cam.cares.jps.config.test;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import junit.framework.TestCase;

/**
 * This class does not contain test methods for asserting results concerning logging 
 * but only demonstrates how to use the logging framework
 * and what are the effects of different configuration options for logging.<br>
 * <br>
 * The logging library slf4j is only used as a facade. Logging itself is implemented and 
 * configured with the logging framework Log4j2. Nevertheless, only use slf4j classes for logging in your code.
 * 
 * @author Andreas
 *
 */
public class TestLogging extends TestCase {
	
	// Use the class as parameter to be able to change the log level in the log4j2 configuration file 
	// for certain packages or projects in a fine-grained manner and to log the class name in the log file
	// If you use e.g ...getLogger("") instead you can only configure the root logger and will have no information
	// about the logging class
	private Logger logger = LoggerFactory.getLogger(TestLogging.class);

	public void testLogLevelsWithSlf4j() {
		
		// DEBUG: designates fine-grained informational events that are most useful to debug an application.
		// This log level is used for debugging on local developer machine or for analyzing an occurred error on production system
		// (the log level can be changed in the log4j2 configuration file without restarting the application)
		logger.debug("This is a DEBUG message");
		
		// INFO: highlight the progress of the application at coarse-grained level 
		// The configured log level on production system is usually INFO
		// INFO messages help you to get a glue what the application has been done (e.g. just before an error occurred)
		// there are always trade-offs: 
		// a) if you log too much on INFO log level then the log files become very large and you might overlook important messages 
		//    in the log file  that help you understanding an occurred error
		// b) if you log too little on INFO log level then it is hard to understand the context of an occurred error
		logger.info("This is a INFO message");
		
		// WARN: designates potentially harmful situations
		logger.warn("This is a WARN message");
		
		// ERROR: designates either error events that might still allow the application to continue running or 
		// very severe error events that will presumably lead the application to abort.
		logger.error("This is a ERROR message");
	}
	
	public void testExceptionLoggingWithSlf4() {
		
		int someImportantParameter = 10;
		
		try {
			if (someImportantParameter > 5) {
				throw new RuntimeException("This is a runtime exception");
			}
		} catch (Exception exc) {
			// logging without stack trace; usually not recommended
			// But if your code manages the one or other caught exception very well, 
			// then sometimes logging on INFO level is enough
			// e.g. if the user is asked for entering a file name and the file is existing,
			// then the user is notified and asked to enter the file name again. 
			logger.error(exc.getMessage() + " and no stack trace is logged");
			// logging with stack trace; recommended
			logger.error(exc.getMessage(), exc);
			// Usually the INFO log level is configured on production system and only coarse-grained events are logged. 
			// If an error occurred, it can be helpful to log some more context information 
			// to ease up the analysis of an error, e.g. the values of input parameters. 
			logger.error("someImportantParameter = " + someImportantParameter);
		}		
	}
	
	public void testLogLevelEnabledLoggingWithSlf4() {
		
		List<Integer> list = new ArrayList<Integer>();
		for (int i=0; i<=9; i++) {
			list.add(i);
		}
		
		// On production system the INFO log level is usually configured and DEBUG messages are not logged.
		// To save resources sometimes it is better to check the log level before creating complicated messages.
		// Disadvantage: additional if line and additional call of isDebugEnabled().
		if (logger.isDebugEnabled()) {
			String message = "This is a very "
					+ "very long message "
					+ "which consists of a lot of values, "
					+ "e.g. list = " + list;
			logger.debug(message);
		} else {
			logger.info("Nothing was logged");
		}
	}
}
