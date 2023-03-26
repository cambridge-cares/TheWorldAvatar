package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage;

import org.apache.jena.rdf.model.Statement;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.*;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.*;
import java.util.function.Function;

/**
 * A class storing all the mappings for each modelling parameter's
 * IfcOwl IRI and their respective object generated in the agent.
 *
 * @author qhouyee
 */
public class ModellingOperatorStorage {
    // Single instance of this class
    private static ModellingOperatorStorage single_instance = null;
    private static Map<String, CartesianPoint> pointMappings;
    private static Queue<CartesianPoint> points;
    private static Map<String, DirectionVector> dirMappings;
    private static Queue<DirectionVector> directions;
    private static Queue<LocalPlacement> placements;
    private static Queue<CartesianTransformationOperator> operators;
    private static Queue<GeometricRepresentationSubContext> subContexts;
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private static final String NON_EXISTING_ERROR = " does not exist in mappings!";

    /**
     * Standard Constructor initialising the maps.
     */
    private ModellingOperatorStorage() {
        pointMappings = new HashMap<>();
        points = new ArrayDeque<>();
        dirMappings = new HashMap<>();
        directions = new ArrayDeque<>();
        placements = new ArrayDeque<>();
        operators = new ArrayDeque<>();
        subContexts = new ArrayDeque<>();
    }

    /**
     * A method to retrieve the singleton instance.
     *
     * @return The singleton instance of SpatialZoneStorage.
     */
    public static ModellingOperatorStorage Singleton() {
        if (single_instance == null) {
            single_instance = new ModellingOperatorStorage();
        }
        return single_instance;
    }

    /**
     * A method to reset the singleton instance before running the code.
     */
    public static void resetSingleton() {
        single_instance = new ModellingOperatorStorage();
    }

    /**
     * A method to store the modelling parameter's IRI and its associated Java object as mappings.
     *
     * @param iri   The modelling parameter's IRI generated from IfcOwl.
     * @param point The CartesianPoint object generated from the iri.
     */
    public void add(String iri, CartesianPoint point) {
        pointMappings.put(iri, point);
    }

    /**
     * A method to store the modelling parameter's IRI and its associated Java object as mappings.
     *
     * @param iri       The modelling parameter's IRI generated from IfcOwl.
     * @param direction The DirectionVector object generated from the iri.
     */
    public void add(String iri, DirectionVector direction) {
        dirMappings.put(iri, direction);
    }

    /**
     * A method to store the modelling parameter's IRI and its associated Java object as mappings.
     *
     * @param placement The LocalPlacement object generated from the iri.
     */
    public void add(LocalPlacement placement) {
        placements.offer(placement);
    }

    /**
     * A method to store the modelling parameter's IRI and its associated Java object as mappings.
     *
     * @param operator The CartesianTransformationOperator object generated from the iri.
     */
    public void add(CartesianTransformationOperator operator) {
        operators.offer(operator);
    }

    /**
     * A method to store the modelling parameter's IRI and its associated Java object as mappings.
     *
     * @param subContext The GeometricRepresentationSubContext object generated from the iri.
     */
    public void add(GeometricRepresentationSubContext subContext) {
        subContexts.offer(subContext);
    }

    /**
     * Retrieve the cartesian point's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public CartesianPoint getPoint(String iri) {
        if (pointMappings.containsKey(iri)) {
            CartesianPoint point = pointMappings.get(iri);
            // When the point is retrieved, it means that it is used and should be constructed later
            points.offer(point);
            return point;
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * Retrieve the direction vector's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public DirectionVector getDirectionVector(String iri) {
        if (dirMappings.containsKey(iri)) {
            DirectionVector direction = dirMappings.get(iri);
            // When the direction is retrieved, it means that it is used and should be constructed later
            directions.offer(direction);
            return direction;
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * Replace any duplicate objects in the mappings with one single object if they exist.
     */
    public void replaceDuplicates() {
        replaceDuplicateMappings(pointMappings, CartesianPoint::getCoordinates);
        replaceDuplicateMappings(dirMappings, DirectionVector::getDirRatios);
    }

    /**
     * Replace any duplicate objects in the mappings with one single object if they exist.
     */
    private <T> void replaceDuplicateMappings(Map<String, T> mappings, Function<T, Double[]> getArray) {
        // Create an ordered list of all keys
        ArrayList<String> keyList = new ArrayList<>(mappings.keySet());
        // Iterate through the list with two pointers to check for duplicates
        for (int left = 0; left < keyList.size(); left++) {
            // Right pointer must always start at least one from left pointer
            int right = left + 1;
            String leftObject = keyList.get(left);
            while (right < keyList.size()) {
                String rightObject = keyList.get(right);
                // If their arrays are equal, replace the right object with the left object, but keep the key
                if (Arrays.equals(getArray.apply(mappings.get(leftObject)), getArray.apply(mappings.get(rightObject)))) {
                    mappings.put(rightObject, mappings.get(leftObject));
                }
                right++;
            }
        }
    }

    /**
     * Check if the mappings contain this IRI.
     *
     * @param iri The modelling parameter's IRI generated from IfcOwl.
     */
    public boolean containsIri(String iri) {
        return pointMappings.containsKey(iri) || dirMappings.containsKey(iri);
    }

    /**
     * Iterate through all the available queues and construct their statements.
     *
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void constructAllStatements(LinkedHashSet<Statement> statementSet) {
        while (!points.isEmpty()) {
            points.poll().constructStatements(statementSet);
        }
        while (!directions.isEmpty()) {
            directions.poll().constructStatements(statementSet);
        }
        while (!placements.isEmpty()) {
            placements.poll().constructStatements(statementSet);
        }
        while (!operators.isEmpty()) {
            operators.poll().constructStatements(statementSet);
        }
        while (!subContexts.isEmpty()) {
            subContexts.poll().constructStatements(statementSet);
        }
    }
}
