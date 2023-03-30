package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage;

import org.apache.jena.rdf.model.Statement;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.Polyline;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.HashMap;
import java.util.LinkedHashSet;

/**
 * A class storing all the mappings for each geometry instance's IfcOwl IRI
 * and their OntoBIM representation counterpart generated in the agent.
 *
 * @author qhouyee
 */
public class GeometryStorage {
    // Single instance of this class
    private static GeometryStorage single_instance = null;
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);
    private static final String NON_EXISTING_ERROR = " does not exist in mappings!";
    private final HashMap<String, Polyline> polylines;

    /**
     * Private Constructor initialising the maps.
     */
    private GeometryStorage() {
        this.polylines = new HashMap<>();
    }

    /**
     * A method to retrieve the singleton instance.
     *
     * @return The singleton instance of GeometryStorage.
     */
    public static GeometryStorage Singleton() {
        if (single_instance == null) {
            single_instance = new GeometryStorage();
        }
        return single_instance;
    }

    /**
     * A method to reset the singleton instance before running the code.
     */
    public static GeometryStorage resetSingleton() {
        single_instance = new GeometryStorage();
        return single_instance;
    }

    /**
     * A method to store the data IRI and its associated geometry's Java object as mappings.
     *
     * @param iri      The data IRI generated from IfcOwl.
     * @param polyline The Polyline object generated from the iri.
     */
    public void add(String iri, Polyline polyline) {
        this.polylines.put(iri, polyline);
    }

    /**
     * Check if the mappings contain this IRI.
     *
     * @param iri The IRI generated from IfcOwl.
     */
    public boolean containsIri(String iri) {
        return this.polylines.containsKey(iri);
    }

    /**
     * Retrieve the site's Java object associated with the IRI input.
     *
     * @param iri The data IRI generated from IfcOwl.
     */
    public Polyline getPolyline(String iri) {
        if (this.polylines.containsKey(iri)) {
            return this.polylines.get(iri);
        } else {
            LOGGER.error(iri + NON_EXISTING_ERROR);
            throw new JPSRuntimeException(iri + NON_EXISTING_ERROR);
        }
    }

    /**
     * Iterate through all the available geometries and construct their statements.
     *
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void constructGeomStatements(LinkedHashSet<Statement> statementSet) {
        for (Polyline line : this.polylines.values()) {
            line.constructStatements(statementSet);
        }
    }
}
