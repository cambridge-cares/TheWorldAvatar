package com.cmclinnovations.featureinfo;

import java.io.IOException;
import java.io.PrintWriter;
import java.time.Instant;
import java.util.List;
import java.util.Map;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

import com.cmclinnovations.featureinfo.queries.AbstractQuery;

import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

public class ManagerTest {
    
    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(ManagerTest.class);

    /**
     * InfoMANAGER instance to test.
     */
    private static Manager MANAGER;
    
    /**
     * Setup to run before all tests.
     */
    @BeforeClass
    public static void setup() {
        // Initialise MANAGER with sample data (take from TheWorldAvatar.com/blazegraph)
        MANAGER = new Manager(
            "http://theworldavatar.com/ontology/ontocredo/ontoaw.owl#assetAAWAAB",
            "http://lee:3838/blazegraph/namespace/???/sparql"
        );

        try {
            MANAGER.readProperties();
        } catch(Exception exception) {
            Assertions.fail(exception);
        }
    }

    /**
     * Check that the class of a feature can be determined.
     */
    @Test
    public void testGetFeatureClass() {
        LOGGER.debug("Querying for feature classes...");

        try {
            String[] classes = MANAGER.getFeatureClasses();
            System.out.println("Classes:");
            System.out.println(String.join(", ", classes));

            Assertions.assertNotNull(classes, "Could not determine class(es) for feature!");
            Assertions.assertTrue(classes.length > 0, "No classes reported for feature!");

        } catch(IOException ioExcep) {
            Assertions.fail(ioExcep);
        }
    }

    /**
     * Tests that the query files can be read.
     */
    @Test
    public void testGetQueryHandler() {
        LOGGER.debug("Testing that a query handler can be produced...");

        try {
            String[] classes = MANAGER.getFeatureClasses();
            System.out.println("Classes:");
            System.out.println(String.join(", ", classes));

            for(String clazz : classes) {
                AbstractQuery handler = MANAGER.setupQueryHandler();
                Assertions.assertNotNull(handler, "Could not find query for '" + clazz + "'' class!");
            }
        } catch(Exception excep) {
            Assertions.fail(excep);
        }
    }

    /**
     * Tests that metadata can be queried from the KG.
     */
    @Test
    public void testGetMetadata() {
        LOGGER.debug("Testing that metadata from the KG can be retrieved...");

        try {
            Map<String, List<String>> result = MANAGER.getMetadata();
            Assertions.assertNotNull(result, "No result from KG!");
            Assertions.assertTrue(result.keySet().size() > 0, "No entries in result!");

            JSONObject converted = MANAGER.toJSON(result);
            System.out.println(converted.toString(2));
            Assertions.assertNotNull(converted, "No result from covnersion to JSON!");
            Assertions.assertTrue(converted.keySet().size() > 0, "No JSON entries in result!");

        } catch(Exception excep) {
            Assertions.fail(excep);
        }
    }

    /**
     * 
     */
    @Test
    public void testGetTimeseries() {
        LOGGER.debug("Testing if timeseries data can be retrieved...");

        String measurementIRI = "http://environment.data.gov.uk/flood-monitoring/id/measures/3401TH-level-downstage-i-15_min-mASD";
        try {
            TimeSeries<Instant> result = MANAGER.getTimeseriesObject(measurementIRI);
            Assertions.assertNotNull(result, "Could not retrieve timeseries data for feature!");
            System.out.println(result);

        } catch(Exception excep) {
            Assertions.fail(excep);
        }
    }

    /**
     * 
     */
    @Test
    public void testGrabAll() {
        LOGGER.debug("Testing if both metadata and timeseries data can be retrieved...");

        try {
            JSONObject result = MANAGER.grabAll();
            Assertions.assertNotNull(result, "Could not retrieve meta OR time data for feature!");
            Assertions.assertNotNull(result.getJSONObject("meta"), "Could not retrieve metadata for feature!");

            PrintWriter myFile = new PrintWriter("./test-output.json", "UTF-8");
            myFile.println(result.toString(2));
            myFile.close();

            System.out.println("--- TEST DONE ---");
        } catch(Exception excep) {
            Assertions.fail(excep);
        }
    }


}
