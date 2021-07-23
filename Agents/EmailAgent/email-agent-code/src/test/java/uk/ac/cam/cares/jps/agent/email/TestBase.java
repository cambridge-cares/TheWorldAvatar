package uk.ac.cam.cares.jps.agent.email;

import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;

/**
 * This class acts as parent for all unit tests and initialises the LOG4J system.
 *
 * Note, for logging to work when running the unit tests, this class must be initialised BEFORE any
 * other Log4J objects. This means that if any classes that contain a Logger instance (and do not
 * extend TestBase) get initialised first, then the logging will fail.
 *
 * @author Michael Hillman
 */
public class TestBase {

    static {
        Logger root = Logger.getRootLogger();
        root.setLevel(Level.DEBUG);
        root.addAppender(new ConsoleAppender(
                new PatternLayout("%d{HH:mm:ss.SSS} (%c{1})[%t] %p - %m%n")
        ));
        
        System.out.println("LOG4J has been initialised.");
    }

}
// End of class.
