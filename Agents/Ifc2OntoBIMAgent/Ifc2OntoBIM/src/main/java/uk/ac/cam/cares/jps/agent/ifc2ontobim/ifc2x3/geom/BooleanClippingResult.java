package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom;

import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.StatementHandler;

import java.util.LinkedHashSet;

/**
 * A class representing the BooleanClippingResult concept in OntoBIM.
 *
 * @author qhouyee
 */
public class BooleanClippingResult {
    private final String iri;
    private final String operator;
    private final String firstOperandIRI;
    private final String secOperandIRI;

    /**
     * Standard Constructor initialising the common inputs.
     *
     * @param instance        This instance's IRI in the IfcOwl triples.
     * @param operator        The boolean clipping result operation types, eg Difference or Union.
     * @param firstOperandIri The first geometry operand for this clipping operation.
     * @param secOperandIri   The second geometry operand for this clipping operation.
     */
    public BooleanClippingResult(String instance, String operator, String firstOperandIri, String secOperandIri) {
        this.iri = StatementHandler.createInstanceFromIRI(instance, OntoBimConstant.CLIPPING_RESULT_CLASS);
        this.operator = operator;
        this.firstOperandIRI = StatementHandler.createGeometryInstanceFromIRI(firstOperandIri);
        this.secOperandIRI = StatementHandler.createGeometryInstanceFromIRI(secOperandIri);
    }

    /**
     * Generate and add the statements required for this class to the statement set input.
     *
     * @param statementSet The set containing the new ontoBIM triples.
     */
    public void constructStatements(LinkedHashSet<Statement> statementSet) {
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.RDF_TYPE, OntoBimConstant.BIM_CLIPPING_RESULT_CLASS);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_BOOLEAN_OPERATOR, this.operator);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_FIRST_OPERAND, this.firstOperandIRI);
        StatementHandler.addStatement(statementSet, this.iri, OntoBimConstant.BIM_HAS_SEC_OPERAND, this.secOperandIRI);
    }
}
