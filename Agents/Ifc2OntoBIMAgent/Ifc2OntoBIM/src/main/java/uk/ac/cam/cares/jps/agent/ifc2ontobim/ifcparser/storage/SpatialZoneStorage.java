package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.HashMap;

/**
 * A class storing all the mappings for each zone instance's IfcOwl IRI
 * and their OntoBIM representation counterpart generated in the agent.
 *
 * @author qhouyee
 */
public class SpatialZoneStorage {
    // Single instance of this class
    private static SpatialZoneStorage single_instance = null;
    private final HashMap<String, String> zones;
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private static final String NON_EXISTING_ERROR = " does not exist in mappings!";

    /**
     * Private Constructor initialising the maps.
     */
    private SpatialZoneStorage() {
        this.zones = new HashMap<>();
    }

    /**
     * A method to retrieve the singleton instance.
     *
     * @return The singleton instance of SpatialZoneStorage.
     */
    public static SpatialZoneStorage Singleton() {
        if (single_instance == null) {
            single_instance = new SpatialZoneStorage();
        }
        return single_instance;
    }

    /**
     * A method to reset the singleton instance before running the code.
     */
    public static void resetSingleton() {
        single_instance = new SpatialZoneStorage();
    }

    /**
     * Retrieve the spatial zone's IRI associated with the IfcOwl IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public String getZone(String iri) {
        if (this.zones.containsKey(iri)) {
            return this.zones.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * A method to store the mappings between the zones IfcOwl and OntoBIM IRI.
     *
     * @param iri  The data IRI generated from IfcOwl.
     * @param zone The spatial zone ontoBIM IRI.
     */
    public void add(String iri, String zone) {
        this.zones.put(iri, zone);
    }
}
