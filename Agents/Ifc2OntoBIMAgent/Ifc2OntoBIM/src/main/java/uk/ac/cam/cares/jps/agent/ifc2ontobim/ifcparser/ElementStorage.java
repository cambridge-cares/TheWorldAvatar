package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.rdf.model.Statement;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Floor;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Roof;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Stair;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Wall;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ModelRepresentation3D;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.*;

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
    private static Map<String, ModelRepresentation3D> geometries;
    private static Map<String, Wall> walls;
    private static Map<String, Floor> floors;
    private static Map<String, Roof> roofs;
    private static Map<String, Stair> stairs;
    private static Set<String> stairComponents;
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
        stairs = new HashMap<>();
        stairComponents= new HashSet<>();
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
     * Retrieve the stair's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public Stair getStair(String iri) {
        if (stairs.containsKey(iri)) {
            return stairs.get(iri);
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
     * @param iri     The element's IRI generated from IfcOwl.
     * @param roof    The Roof object generated from the iri.
     */
    public void add(String iri, Roof roof) {
        roofs.put(iri, roof);
    }

    /**
     * An overloaded method to store the element IRI and its associated 3D model representation Java object as mappings.
     *
     * @param iri      The element's IRI generated from IfcOwl.
     * @param stair    The Stair object generated from the iri.
     */
    public void add(String iri, Stair stair) {
        stairs.put(iri, stair);
    }

    /**
     * An overloaded method to store the IRI for any stair subcomponents.
     *
     * @param stairComponentIri  The element's IRI generated from IfcOwl.
     */
    public void add(String stairComponentIri) { stairComponents.add(stairComponentIri); }

    /**
     * Check if the mappings contain this IRI for the model representation 3D.
     *
     * @param iri The element's IRI generated from IfcOwl.
     */
    public boolean containsModelRepIri(String iri) {
        return geometries.containsKey(iri);
    }

    /**
     * Check if there is mappings for this stair assembly.
     *
     * @param iri The element's IRI generated from IfcOwl.
     */
    public boolean containsStairAssemblyIri(String iri) {
        return stairs.containsKey(iri);
    }

    /**
     * Check if the stair subcomponents has already been created.
     *
     * @param iri The element's IRI generated from IfcOwl.
     */
    public boolean containsStairSubComponentIri(String iri) {
        return stairComponents.contains(iri);
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
