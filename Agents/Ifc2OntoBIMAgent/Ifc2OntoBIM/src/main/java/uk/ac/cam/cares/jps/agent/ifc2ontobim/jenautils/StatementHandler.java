package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

import org.apache.jena.rdf.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.util.LinkedHashSet;

/**
 * Handles the generation of new statements.
 *
 * @author qhouyee
 */
public class StatementHandler {
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);

    /**
     * Overloaded method to generate a statement with subject, predicate, and object nodes,
     * and append it to the statement set.
     *
     * @param statementSet Set of statements to append the newly generated statement.
     * @param subject      The subject node with a valid URI.
     * @param predicate    The predicate node with a valid URI.
     * @param object       The object node with a valid URI or a string literal.
     */
    public static void addStatement(LinkedHashSet<Statement> statementSet, String subject, String predicate, String object) {
        addStatement(statementSet, subject, predicate, object, true);
    }

    /**
     * Generate a statement with the given subject, predicate, and object, and append it to the statement set.
     *
     * @param statementSet Set of statements to append the newly generated statement.
     * @param subject      The subject node with a valid URI.
     * @param predicate    The predicate node with a valid URI.
     * @param object       The object node with a valid URI or a string literal.
     * @param isObjInst    A boolean indicating if the object is a node or a string.
     */
    public static void addStatement(LinkedHashSet<Statement> statementSet, String subject, String predicate, String object, boolean isObjInst) {
        // Validates the subject and predicate URI
        validateUri(subject);
        validateUri(predicate);
        // Generate the resources and properties for the statement
        Model model = ModelFactory.createDefaultModel();
        Resource subj = ResourceFactory.createResource(subject);
        Property pred = ResourceFactory.createProperty(predicate);
        RDFNode obj;
        if (isObjInst) {
            // For instances
            validateUri(object);
            obj = ResourceFactory.createResource(object);
        } else {
            // For string literals
            obj = ResourceFactory.createPlainLiteral(object);
        }
        Statement newStatement = model.createStatement(subj, pred, obj);
        statementSet.add(newStatement);
    }

    /**
     * Generate a statement with the given subject, predicate, and double literal, and append it to the statement set.
     *
     * @param statementSet Set of statements to append the newly generated statement.
     * @param subject      The subject node with a valid URI.
     * @param predicate    The predicate node with a valid URI.
     * @param object       A double literal.
     */
    public static void addStatementWithDoubleLiteral(LinkedHashSet<Statement> statementSet, String subject, String predicate, Double object) {
        // Validates the subject and predicate URI
        validateUri(subject);
        validateUri(predicate);
        // Generate the resources and properties for the statement
        Model model = ModelFactory.createDefaultModel();
        Resource subj = ResourceFactory.createResource(subject);
        Property pred = ResourceFactory.createProperty(predicate);
        // Typed literals can be generated if given the right data type
        RDFNode obj = ResourceFactory.createTypedLiteral(object);
        Statement newStatement = model.createStatement(subj, pred, obj);
        statementSet.add(newStatement);
    }

    /**
     * Validates the URI of the node.
     *
     * @param node The node to validate.
     */
    private static void validateUri(String node) {
        if (!(node.startsWith("http://") || node.startsWith("https://"))) {
            LOGGER.error("Invalid node format for " + node + ". Valid formats should start with http:// or https://");
            throw new JPSRuntimeException("Invalid node format for " + node + ". Valid formats should start with http:// or https://");
        }
    }
}
