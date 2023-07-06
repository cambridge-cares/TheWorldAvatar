package uk.ac.cam.cares.jps.agent.ifc2ontobim.utils;

import org.apache.jena.rdf.model.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.Ifc2OntoBIMAgent;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
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
     * Creates a new geometry instance from a required IRI while retaining the numerical identifier.
     * This is only intended to target geometry instances.
     *
     * @param iri IRI of instance to be renamed
     */
    public static String createGeometryInstanceFromIRI(String iri) {
        // Note the string retrieved is at the first index of Ifc, and will still include fc
        String bimClass = StringUtils.getStringAfterLastCharacterOccurrence(iri, "Ifc");
        // Remove the fc using the method below
        bimClass = StringUtils.getStringAfterFirstCharacterOccurrence(bimClass, "c");
        bimClass = StringUtils.getStringBeforeLastCharacterOccurrence(bimClass, StringUtils.UNDERSCORE);
        return createInstanceFromIRI(iri, bimClass);
    }

    /**
     * Creates a new instance from a required IRI while retaining the numerical identifier.
     *
     * @param iri      IRI of instance to be renamed
     * @param bimClass The new class name in the OntoBIM ontology.
     */
    public static String createInstanceFromIRI(String iri, String bimClass) {
        String identifier = StringUtils.getStringAfterLastCharacterOccurrence(iri, StringUtils.UNDERSCORE);
        return NamespaceMapper.getBaseNameSpace() + bimClass + OntoBimConstant.UNDERSCORE + identifier;
    }

    /**
     * Creates a new instance from an optional IRI while retaining the numerical identifier.
     *
     * @param iri      IRI of instance to be renamed
     * @param bimClass The new class name in the OntoBIM ontology.
     */
    public static String createInstanceFromOptionalIRI(String iri, String bimClass) {
        if (iri != null) {
            return createInstanceFromIRI(iri, bimClass);
        }
        return null;
    }

    /**
     * Add a statement with the given subject, predicate, and object to the statement set.
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
     * An overloaded method to add a statement with subject, predicate, and object nodes or literals to the statement set.
     * Literals can be of any valid Java classes.
     *
     * @param statementSet Set of statements to append the newly generated statement.
     * @param subject      The subject node with a valid URI.
     * @param predicate    The predicate node with a valid URI.
     * @param object       The object node with a valid URI or any literal type.
     */
    public static void addStatement(LinkedHashSet<Statement> statementSet, String subject, String predicate, Object object) {
        // If the object is a string class, add it as a String literal
        if (object.getClass().equals(String.class)) {
            addStatement(statementSet, subject, predicate, (String) object, true);
        } else {
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
    }

    /**
     * Add a statement with the given subject, predicate, and object (literal or node) to the statement set.
     * Note that this statement will only be added if the object node is not null.
     *
     * @param statementSet Set of statements to append the newly generated statement.
     * @param subject      The subject node with a valid URI.
     * @param predicate    The predicate node with a valid URI.
     * @param object       The object node with a valid URI or a string literal.
     * @param isObjInst    A boolean indicating if the object is a node or a string.
     */
    public static void addOptionalStatement(LinkedHashSet<Statement> statementSet, String subject, String predicate, String object, boolean isObjInst) {
        if (object != null) {
            addStatement(statementSet, subject, predicate, object, isObjInst);
        }
    }

    /**
     * An overloaded method to adding an optional statement consisting of all nodes to the set.
     * Note that this statement will only be added if the object node is not null.
     *
     * @param statementSet Set of statements to append the newly generated statement.
     * @param subject      The subject node with a valid URI.
     * @param predicate    The predicate node with a valid URI.
     * @param object       The object node with a valid URI or a string literal.
     */
    public static void addOptionalStatement(LinkedHashSet<Statement> statementSet, String subject, String predicate, String object) {
        if (object != null) {
            addStatement(statementSet, subject, predicate, object, true);
        }
    }

    /**
     * An overloaded method for adding statements with numerical literals to the set.
     * Note that this statement will only be added if the object node is not null.
     *
     * @param statementSet Set of statements to append the newly generated statement.
     * @param subject      The subject node with a valid URI.
     * @param predicate    The predicate node with a valid URI.
     * @param object       The object node with a valid URI or a string literal.
     */
    public static void addOptionalStatement(LinkedHashSet<Statement> statementSet, String subject, String predicate, Number object) {
        if (object != null) {
            addStatement(statementSet, subject, predicate, object);
        }
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
