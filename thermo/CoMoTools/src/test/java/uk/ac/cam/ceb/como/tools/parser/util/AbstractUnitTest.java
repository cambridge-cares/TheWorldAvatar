package uk.ac.cam.ceb.como.tools.parser.util;

import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public abstract class AbstractUnitTest {

    /**
     * logger for all subclasses.
     */
    protected Logger logger = Logger.getLogger(getClass().getName());

    /**
     * Log method/name/title using logger.info
     * @param title Title of new test section
     */
    public void printTestTitle(String title) {
        logger.info(">>>>> TEST : " + title + " <<<<<");
    }
}
