package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.HashMap;

/**
 * A class storing all the mappings for the units and their instances.
 *
 * @author qhouyee
 */
public class UnitStorage {
    // Single instance of this class
    private static UnitStorage single_instance = null;
    private final HashMap<String, String> units;
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private static final String NON_EXISTING_ERROR = " does not exist in mappings!";

    /**
     * Private Constructor initialising the maps.
     */
    private UnitStorage() {
        this.units = new HashMap<>();
    }

    /**
     * A method to retrieve the singleton instance.
     *
     * @return This singleton instance.
     */
    public static UnitStorage Singleton() {
        if (single_instance == null) {
            single_instance = new UnitStorage();
        }
        return single_instance;
    }

    /**
     * A method to reset the singleton instance before running the code.
     */
    public static void resetSingleton() {
        single_instance = new UnitStorage();
    }

    /**
     * Retrieve the unit's IRI associated with the unit of interest, eg meter.
     *
     * @param unit The unit required.
     */
    public String getIri(String unit) {
        if (this.units.containsKey(unit)) {
            return this.units.get(unit);
        } else {
            LOGGER.error(unit + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(unit + NON_EXISTING_ERROR);
        }
    }

    /**
     * A method to store new mappings.
     *
     * @param unit    The unit of interest, eg meter.
     * @param unitIri The unit IRI.
     */
    public void add(String unit, String unitIri) {
        this.units.put(unit, unitIri);
    }
}
