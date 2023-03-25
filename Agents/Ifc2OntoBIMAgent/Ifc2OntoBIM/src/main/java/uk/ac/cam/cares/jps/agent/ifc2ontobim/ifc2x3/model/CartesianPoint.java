package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.NamespaceMapper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StringUtils;

import java.util.LinkedHashSet;
import java.util.UUID;

/**
 * A class representing the CartesianPoint concept in OntoBIM.
 *
 * @author qhouyee
 */
public class CartesianPoint {
    private final String iri;
    private final Double[] coordinates;

    /**
     * Standard Constructor initialising the necessary and optional inputs.
     *
     * @param xCoordinate A field for the x-coordinate.
     * @param yCoordinate A field for the y-coordinate.
     * @param zCoordinate An optional field for the z-coordinate.
     */
    public CartesianPoint(String xCoordinate, String yCoordinate, String zCoordinate) {
        String prefix = NamespaceMapper.getBaseNameSpace();
        this.iri = prefix + OntoBimConstant.CARTESIAN_POINT_CLASS + OntoBimConstant.UNDERSCORE + UUID.randomUUID();
        // Initialise the array and add coordinates
        this.coordinates = new Double[3];
        this.coordinates[0] = Double.valueOf(xCoordinate);
        this.coordinates[1] = Double.valueOf(yCoordinate);
        if (zCoordinate != null) {
            this.coordinates[2] = Double.valueOf(zCoordinate);
        }
    }

    public String getIri() {
        return this.iri;
    }

    public Double[] getCoordinates() {
        return this.coordinates;
    }


    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.getIri(), OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_CARTESIAN_POINT_CLASS);
        StatementHandler.addStatementWithNumberLiteral(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_X_COORDINATE, this.coordinates[0]);
        StatementHandler.addStatementWithNumberLiteral(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_Y_COORDINATE, this.coordinates[1]);
        StatementHandler.addOptionalStatement(statementSet, this.getIri(), OntoBimConstant.BIM_HAS_Z_COORDINATE, this.coordinates[2]);
    }
}
