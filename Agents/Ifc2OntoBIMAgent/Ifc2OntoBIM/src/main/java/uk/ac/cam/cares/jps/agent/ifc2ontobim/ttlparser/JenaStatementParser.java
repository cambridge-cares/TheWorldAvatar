package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.apache.jena.graph.Node;
import org.apache.jena.rdf.model.Statement;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * Provides methods to parse the nodes in Apache Jena's Statement class into TTL triple string formats.
 *
 * @author qhouyee
 */
class JenaStatementParser {
    private static final String listNamespace = "https://w3id.org/list#";

    /**
     * An overloaded method that calls the standard method with a null for the third parameter.
     * Parses the triples into valid TTL node format as String.
     *
     * @param statementSet A set containing unique statements in sequence.
     * @param inverseNsMap The inverse mappings of URIs to their prefix.
     * @return All input statements as one string builder object.
     */
    protected static StringBuilder parseTriples(LinkedHashSet<Statement> statementSet, Map<String, String> inverseNsMap) {
        return parseTriples(statementSet, inverseNsMap, null);
    }

    /**
     * Standard method to parse triples into valid TTL node format as String. Syntax remains incomplete in this intermediate step.
     * Statement is also removed once appended to a string builder to clear memory.
     *
     * @param statementSet A set containing unique statements in sequence.
     * @param inverseNsMap The inverse mappings of URIs to their prefix.
     * @param ignoreSet    A set containing the classes that should be ignored.
     * @return All input statements as one string builder object.
     */
    protected static StringBuilder parseTriples(LinkedHashSet<Statement> statementSet, Map<String, String> inverseNsMap, Set<String> ignoreSet) {
        StringBuilder sbuilder = new StringBuilder();

        // Set up an iterator to allow removal of objects when in use
        Iterator<Statement> iter = statementSet.iterator();
        while (iter.hasNext()) {
            Statement statement = iter.next();
            Node subject = statement.asTriple().getSubject();
            Node predicate = statement.asTriple().getPredicate();
            Node object = statement.asTriple().getObject();
            if (checkIfAppendStatement(predicate, object, ignoreSet)) {
                parseNodeAsString(subject, sbuilder, inverseNsMap, true);
                sbuilder.append(StringUtils.WHITESPACE);
                parseNodeAsString(predicate, sbuilder, inverseNsMap);
                sbuilder.append(StringUtils.WHITESPACE);
                parseNodeAsString(object, sbuilder, inverseNsMap, true);
                sbuilder.append("\n");
                // Remove this statement from the Set to free memory once appended
                iter.remove();
            }
        }
        return sbuilder;
    }

    /**
     * Check if the statement should be appended depending on if their object class is designated as an Ignored class.
     *
     * @param predicate The RDF predicate node retrieved from statement.
     * @param object    The RDF object node retrieved from statement.
     * @param ignoreSet A set containing the classes that should be ignored.
     * @return true if this triple does not belong to the ignored classes, otherwise, false.
     */
    private static boolean checkIfAppendStatement(Node predicate, Node object, Set<String> ignoreSet) {
        if (ignoreSet == null || ignoreSet.isEmpty()) {
            return true;
        }
        // Ensure that the object is a URI
        if (object.isURI()) {
            // Predicate must be of rdf:type
            if (predicate.getLocalName().equals("type")) {
                return ignoreSet.stream().noneMatch(ignoreClass -> object.getLocalName().contains(ignoreClass));
            }
        }
        return true;
    }

    /**
     * An overloaded method that calls the standard method with a false boolean when no input is given.
     * This should only be used for predicate nodes, and parse them into suitable TTL string format.
     *
     * @param node         The RDF node retrieved from statement.
     * @param builder      The string builder appending the new node in the TTL format.
     * @param inverseNsMap The inverse mappings to replace URIs with their prefix.
     */
    private static void parseNodeAsString(Node node, StringBuilder builder, Map<String, String> inverseNsMap) {
        parseNodeAsString(node, builder, inverseNsMap, false);
    }

    /**
     * Standard method that parses the nodes into suitable TTL string format.
     * This method is able to handle both literals and URIs accordingly.
     *
     * @param node                  The RDF node retrieved from statement.
     * @param builder               The string builder appending the new node in the TTL format.
     * @param inverseNsMap          The inverse mappings to replace URIs with their prefix.
     * @param isSubjectOrObjectNode A boolean indicating true if it is a subject or object node,
     *                              and false if it is a predicate node.
     */
    private static void parseNodeAsString(Node node, StringBuilder builder, Map<String, String> inverseNsMap, boolean isSubjectOrObjectNode) {
        if (node.isLiteral()) {
            String literalValue = node.getLiteralLexicalForm();
            String dataTypeURI = node.getLiteralDatatypeURI();
            // All literal format begin with "value"
            builder.append('"')
                    .append(literalValue)
                    .append('"');
            // Append non-string literals in the format "value"^^xsd:datatype
            if (!dataTypeURI.contains("string")) {
                builder.append("^^xsd:"); // Assumes that the namespace is always the xsd namespace, which is true for IfcOwl literals
                String dataType = StringUtils.getStringAfterLastCharacterOccurrence(dataTypeURI, StringUtils.HASHMARK);
                builder.append(dataType);
            }
        } else if (node.isURI()) {
            // Default results from Jena Node parser
            String namespace = node.getNameSpace();
            String localName = node.getLocalName();

            // Jena namespace parser does not allow digits as the start of the local name.
            // One example is ...ontoBIM#3DModel..., which gives the namespace "...ontoBIM#3" and local name "DModel...".
            // To circumvent this limitation, the below if condition statement checks if the namespace ends in a digit.
            // If it does, it will manually retrieve the namespaces and local names with a custom solution rather than using Jena
            if (Character.isDigit(namespace.charAt(namespace.length() - 1))) {
                int hashIndex = namespace.lastIndexOf(StringUtils.HASHMARK); // The problematic URI namespaces end with #, but / may be an issue for other uses
                namespace = namespace.substring(0, hashIndex + 1);
                localName = node.toString().substring(namespace.length());
            }
            // Renames the instance to their respective ontoBIM class and namespace if available
            localName = isSubjectOrObjectNode ? renameInstance(localName) : renameListPredicate(localName, namespace);
            namespace = isSubjectOrObjectNode ? namespace :
                    (namespace.equals(listNamespace) ? "http://www.theworldavatar.com/ontology/ontobim/ontoBIM#" : namespace);

            builder.append(inverseNsMap.get(namespace)) // Append the namespace prefix, not full URI
                    .append(StringUtils.SEMICOLON)
                    .append(localName); // Append the name
        }
    }

    /**
     * Rename the nodes into ontoBIM instances.
     *
     * @param localName The local name of a node.
     * @return The required local name in the right namespace.
     */
    private static String renameInstance(String localName) {
        String ifcClass = StringUtils.getStringBeforeLastCharacterOccurrence(localName, StringUtils.UNDERSCORE);
        String replacementName = OntoBIMInstance.retrieveOntoBimName(ifcClass);

        if (replacementName != "NA") {
            String identifier = StringUtils.getStringAfterLastCharacterOccurrence(localName, StringUtils.UNDERSCORE);
            localName = replacementName + StringUtils.UNDERSCORE + identifier;
        }
        return localName;
    }

    /**
     * Rename the predicate starting with list namespace into their ontoBIM instances.
     *
     * @param localName The local name of a node.
     * @param namespace The namespace of a node.
     * @return The required local name in the right namespace.
     */
    private static String renameListPredicate(String localName, String namespace) {
        // Note that there is no need to rename hasContents as it is the same
        localName = namespace.equals(listNamespace) ?
                (localName.equals("hasNext") ? "hasNextVertex" :
                        (localName.equals("hasContents") ? "hasRefPoint" : localName))
                : localName;
        return localName;
    }
}
