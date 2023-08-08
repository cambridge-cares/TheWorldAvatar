package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;

/**
 * A class representing the CartesianTransformationOperator concept in OntoBIM.
 *
 * @author qhouyee
 */
public class CartesianTransformationOperator {
    private final String iri;
    private final String pointIRI;
    private final String xAxisDirIRI;
    private final String yAxisDirIRI;
    private final Double scaleFactor;

    /**
     * Standard Constructor initialising the necessary and optional inputs.
     *
     * @param iri               The instance IRI in IfcOwl.
     * @param pointIri          This transformer's local origin cartesian point IRI.
     * @param scaleFactor       An optional field for this transformer's scale factor.
     * @param xAxisDirectionIri An optional field for this transformer's x-axis direction IRI.
     * @param yAxisDirectionIri An optional field for this transformer's y-axis direction IRI.
     */
    public CartesianTransformationOperator(String iri, String pointIri, String scaleFactor, String xAxisDirectionIri, String yAxisDirectionIri) {
        this.iri = StatementHandler.createInstanceFromIRI(iri, OntoBimConstant.CART_TRANS_OPERATOR_CLASS);
        this.pointIRI = pointIri;
        this.xAxisDirIRI = xAxisDirectionIri;
        this.yAxisDirIRI = yAxisDirectionIri;
        if (scaleFactor != null) {
            this.scaleFactor = Double.valueOf(scaleFactor);
        } else {
            this.scaleFactor = null;
        }
    }

    /**
     * Generate and add the statements required for this Class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_CART_TRANS_OPERATOR_CLASS);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_LOCAL_ORIGIN, this.pointIRI);
        StatementHandler.addOptionalStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_SCALE, this.scaleFactor);
        StatementHandler.addOptionalStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_DERIVED_X_AXIS, this.xAxisDirIRI);
        StatementHandler.addOptionalStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_DERIVED_Y_AXIS, this.yAxisDirIRI);
    }
}
