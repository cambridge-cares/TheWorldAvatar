package uk.ac.cam.cares.jps.base.query;

import java.net.URI;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

public class ResourcePathConverter {

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(ResourcePathConverter.class);

    private static ResourcePathConverter instance = null;

    public static synchronized ResourcePathConverter getInstance() {
        if (instance == null) {
            instance = new ResourcePathConverter();
        }
        return instance;
    }

    public static String convert(String path) {

        //TODO-AE SC URGENT 20191021 CHANGE BACK this will work on claudius but not anymore locally --> configurable solution?
        // maybe change back not necessary any more, because of the solution below
        if ((!AgentLocator.isJPSRunningForTest())) {
            LOGGER.info(path);
            return path;
        }
        String scenarioUrl = JPSContext.getScenarioUrl();
//		if (!AgentLocator.isJPSRunningForTest()) {
        if (scenarioUrl != null) {

            LOGGER.info(("scenarioURL = " + scenarioUrl + path));
            return path;
        }

        // i.e. the code is not running on claudius 
        String address = KeyValueManager.getServerAddress();
        String converted = path;
        LOGGER.info("IT GOES HERE IN RPC,CONVERTED= " + converted);
        if (path.contains("http://www.theworldavatar.com/ontology")) {
            // don't convert
        } else if (path.contains("http://www.theworldavatar.com")) {
            converted = path.replace("http://www.theworldavatar.com", address);
            LOGGER.info("converted resource path " + path + " to " + converted);
        } else if (path.contains("http://www.jparksimulator.com")) {
            converted = path.replace("http://www.jparksimulator.com", address);
            LOGGER.info("convert(): converted resource path " + path + " to " + converted);
        }

        return converted;
    }

    public static String convertToLocalPath(String path) {

        URI uri = AgentCaller.createURI(path);
        String root = KeyValueManager.get("absdir.root");
//		if ((path.startsWith("C:")) || (!AgentLocator.isJPSRunningForTest())) {
//			LOGGER.info(path);
//			return path;
//		}
        String converted = root + uri.getPath();

        LOGGER.info("convertToLocalPath(): converted resource path " + path + " to " + converted);
        return converted;
    }
}
