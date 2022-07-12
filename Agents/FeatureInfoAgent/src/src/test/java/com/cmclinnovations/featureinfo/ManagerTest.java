package com.cmclinnovations.featureinfo;

import java.io.IOException;
import java.time.Instant;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import org.junit.BeforeClass;
import org.junit.Ignore;
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
        // MANAGER = new Manager(
        //     "http://environment.data.gov.uk/flood-monitoring/id/stations/7015",
        //     "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/flood_ontoems/sparql"
        // );
        MANAGER = new Manager(
            "http://environment.data.gov.uk/flood-monitoring/id/stations/3401TH",
            "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/flood_ontoems/sparql"
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
    @Ignore
    @Test
    public void testGetFeatureClass() {
        LOGGER.debug("Querying for feature classes...");

        try {
            String[] classes = MANAGER.getFeatureClasses();
            System.out.println(String.join(", ", classes));

            Assertions.assertNotNull(classes, "Could not determine class(es) for feature!");
            Assertions.assertTrue(classes.length > 0, "No classes reported for feature!");
            Assertions.assertTrue(Arrays.asList(classes).contains("ReportingStation"), "Expected classes for this case to include 'WaterLevelReportingStation'");

        } catch(IOException ioExcep) {
            Assertions.fail(ioExcep);
        }
    }

    /**
     * Tests that the query files can be read.
     */
    @Ignore
    @Test
    public void testGetQueryHandler() {
        LOGGER.debug("Testing that a query handler can be produced...");

        try {
            String[] classes = new String[]{"ReportingStation"};

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
    @Ignore
    @Test
    public void testGetMetadata() {
        LOGGER.debug("Testing that metadata from the KG can be retrieved...");

        try {
            Map<String, List<String>> result = MANAGER.getMetadata();
            Assertions.assertNotNull(result, "No result from KG!");
            Assertions.assertTrue(result.keySet().size() > 0, "No JSON entries in result!");

        } catch(Exception excep) {
            Assertions.fail(excep);
        }
    }

    /**
     * 
     */
    @Ignore
    @Test
    public void testGetTimeseries() {
        LOGGER.debug("Testing if timeseries data can be retrieved...");

        Set<String> measurementIRIs = new HashSet<>();
        measurementIRIs.add("http://environment.data.gov.uk/flood-monitoring/id/measures/3401TH-level-downstage-i-15_min-mASD");
        measurementIRIs.add("http://environment.data.gov.uk/flood-monitoring/id/measures/3401TH-level-stage-i-15_min-mASD");

        try {
            TimeSeries<Instant> result = MANAGER.getTimeseries("http://environment.data.gov.uk/flood-monitoring/id/measures/3401TH-level-downstage-i-15_min-mASD");
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
            Assertions.assertNotNull(result.getString("meta"), "Could not retrieve metadata for feature!");
        } catch(Exception excep) {
            Assertions.fail(excep);
        }
    }


}
