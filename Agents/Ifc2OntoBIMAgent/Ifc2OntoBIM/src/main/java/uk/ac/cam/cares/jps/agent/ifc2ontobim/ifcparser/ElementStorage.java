package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.rdf.model.Statement;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Floor;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Roof;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Wall;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ModelRepresentation3D;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.HashMap;
import java.util.LinkedHashSet;

/**
 * A class storing all the mappings for each element instance's IfcOwl IRI
 * to their Model representation 3D object generated in the agent. It also
 * stores the mappings for Assembly classes like Walls.
 *
 * @author qhouyee
 */
public class ElementStorage {
    // Single instance of this class
    private static ElementStorage single_instance = null;
    private static HashMap<String, ModelRepresentation3D> geometries;
    private static HashMap<String, Wall> walls;
    private static HashMap<String, Floor> floors;
    private static HashMap<String, Roof> roofs;
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private static final String NON_EXISTING_ERROR = " does not exist in mappings!";

    /**
     * Standard Constructor initialising the maps.
     */
    private ElementStorage() {
        geometries = new HashMap<>();
        walls = new HashMap<>();
        floors = new HashMap<>();
        roofs = new HashMap<>();
    }

    /**
     * A method to retrieve the singleton instance.
     *
     * @return The singleton instance of SpatialZoneStorage.
     */
    public static ElementStorage Singleton() {
        if (single_instance == null) {
            single_instance = new ElementStorage();
        }
        return single_instance;
    }

    /**
     * A method to reset the singleton instance before running the code.
     */
    public static void resetSingleton() {
        single_instance = new ElementStorage();
    }

    /**
     * Clears all the existing geometry mappings saved.
     */
    public void clear() {
        geometries = new HashMap<>();
    }

    /**
     * Retrieve the model representation's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public ModelRepresentation3D getModelRep(String iri) {
        if (geometries.containsKey(iri)) {
            return geometries.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * Retrieve the wall's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public Wall getWall(String iri) {
        if (walls.containsKey(iri)) {
            return walls.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * Retrieve the floor's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public Floor getFloor(String iri) {
        if (floors.containsKey(iri)) {
            return floors.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * Retrieve the roof's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public Roof getRoof(String iri) {
        if (roofs.containsKey(iri)) {
            return roofs.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * A method to store the element IRI and its associated 3D model representation Java object as mappings.
     *
     * @param iri      The element's IRI generated from IfcOwl.
     * @param modelRep The ModelRepresentation3D object generated from the iri.
     */
    public void add(String iri, ModelRepresentation3D modelRep) { geometries.put(iri, modelRep); }

    /**
     * An overloaded method to store the element IRI and its associated 3D model representation Java object as mappings.
     *
     * @param iri      The element's IRI generated from IfcOwl.
     * @param wall     The Wall object generated from the iri.
     */
    public void add(String iri, Wall wall) {
        walls.put(iri, wall);
    }

    /**
     * An overloaded method to store the element IRI and its associated 3D model representation Java object as mappings.
     *
     * @param iri      The element's IRI generated from IfcOwl.
     * @param floor    The Floor object generated from the iri.
     */
    public void add(String iri, Floor floor) {
        floors.put(iri, floor);
    }

    /**
     * An overloaded method to store the element IRI and its associated 3D model representation Java object as mappings.
     *
     * @param iri      The element's IRI generated from IfcOwl.
     * @param roof    The Floor object generated from the iri.
     */
    public void add(String iri, Roof roof) {
        roofs.put(iri, roof);
    }

    /**
     * Check if the mappings contain this IRI for the model representation 3D.
     *
     * @param iri The element's IRI generated from IfcOwl.
     */
    public boolean containsModelRepIri(String iri) {
        return geometries.containsKey(iri);
    }

    /**
     * Iterate through all the available model representation 3D and construct their statements.
     *
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void constructModelRepStatements(LinkedHashSet<Statement> statementSet) {
        for (ModelRepresentation3D modelRepInst: geometries.values()){
            modelRepInst.addModelRepresentation3DStatements(statementSet);
        }
    }
}
