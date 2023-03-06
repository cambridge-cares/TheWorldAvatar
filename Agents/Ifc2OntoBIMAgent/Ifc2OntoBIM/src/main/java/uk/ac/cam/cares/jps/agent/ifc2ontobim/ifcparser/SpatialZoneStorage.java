package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.IfcProjectRepresentation;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.zone.*;
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
    private final HashMap<String, IfcProjectRepresentation> project;
    private final HashMap<String, IfcSiteRepresentation> sites;
    private final HashMap<String, IfcBuildingRepresentation> buildings;
    private final HashMap<String, IfcStoreyRepresentation> storeys;
    private final HashMap<String, IfcRoomRepresentation> rooms;
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private static final String NON_EXISTING_ERROR = " does not exist in mappings!";

    /**
     * Private Constructor initialising the maps.
     */
    private SpatialZoneStorage() {
        this.project = new HashMap<>();
        this.sites = new HashMap<>();
        this.buildings = new HashMap<>();
        this.storeys = new HashMap<>();
        this.rooms = new HashMap<>();
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
     * Retrieve the site's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public IfcProjectRepresentation getProject(String iri) {
        if (this.project.containsKey(iri)) {
            return this.project.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * Retrieve the site's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public IfcSiteRepresentation getSite(String iri) {
        if (this.sites.containsKey(iri)) {
            return this.sites.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * Retrieve the building's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public IfcBuildingRepresentation getBuilding(String iri) {
        if (this.buildings.containsKey(iri)) {
            return this.buildings.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * Retrieve the storey's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public IfcStoreyRepresentation getStorey(String iri) {
        if (this.storeys.containsKey(iri)) {
            return this.storeys.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * Retrieve the room's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public IfcRoomRepresentation getRoom(String iri) {
        if (this.rooms.containsKey(iri)) {
            return this.rooms.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * An overloaded method to store the data IRI
     * and its associated zone's Java object as mappings.
     *
     * @param iri  The data IRI generated from IfcOwl.
     * @param project The IfcProjectRepresentation object generated from the iri.
     */
    public void add(String iri, IfcProjectRepresentation project) {
        this.project.put(iri, project);
    }

    /**
     * An overloaded method to store the data IRI
     * and its associated zone's Java object as mappings.
     *
     * @param iri  The data IRI generated from IfcOwl.
     * @param site The IfcSiteRepresentation object generated from the iri.
     */
    public void add(String iri, IfcSiteRepresentation site) {
        this.sites.put(iri, site);
    }

    /**
     * An overloaded method to store the data IRI
     * and its associated zone's Java object as mappings.
     *
     * @param iri      The data IRI generated from IfcOwl.
     * @param building The IfcBuildingRepresentation object generated from the iri.
     */
    public void add(String iri, IfcBuildingRepresentation building) {
        this.buildings.put(iri, building);
    }

    /**
     * An overloaded method to store the data IRI
     * and its associated zone's Java object as mappings.
     *
     * @param iri    The data IRI generated from IfcOwl.
     * @param storey The IfcStoreyRepresentation object generated from the iri.
     */
    public void add(String iri, IfcStoreyRepresentation storey) {
        this.storeys.put(iri, storey);
    }

    /**
     * An overloaded method to store the data IRI
     * and its associated zone's Java object as mappings.
     *
     * @param iri  The data IRI generated from IfcOwl.
     * @param room The IfcRoomRepresentation object generated from the iri.
     */
    public void add(String iri, IfcRoomRepresentation room) {
        this.rooms.put(iri, room);
    }
}
