package com.cmclinnovations.agent.utils;

import java.net.URI;
import java.net.URISyntaxException;
import java.text.MessageFormat;

import com.fasterxml.jackson.databind.JsonNode;

public class StringResource {
  public static final String QUERY_TEMPLATE_PREFIX = "PREFIX cmns-col: <https://www.omg.org/spec/Commons/Collections/>"
      + "PREFIX cmns-dt: <https://www.omg.org/spec/Commons/DatesAndTimes/>"
      + "PREFIX cmns-dsg: <https://www.omg.org/spec/Commons/Designators/>"
      + "PREFIX cmns-rlcmp: <https://www.omg.org/spec/Commons/RolesAndCompositions/>"
      + "PREFIX fibo-fnd-arr-id:<https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/IdentifiersAndIndices/>"
      + "PREFIX fibo-fnd-plc-adr:<https://spec.edmcouncil.org/fibo/ontology/FND/Places/Addresses/>"
      + "PREFIX fibo-fnd-plc-loc:<https://spec.edmcouncil.org/fibo/ontology/FND/Places/Locations/>"
      + "PREFIX fibo-fnd-arr-lif: <https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Lifecycles/>"
      + "PREFIX fibo-fnd-arr-rep: <https://spec.edmcouncil.org/fibo/ontology/FND/Arrangements/Reporting/>"
      + "PREFIX fibo-fnd-dt-fd: <https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/FinancialDates/>"
      + "PREFIX fibo-fnd-dt-oc: <https://spec.edmcouncil.org/fibo/ontology/FND/DatesAndTimes/Occurrences/>"
      + "PREFIX fibo-fnd-rel-rel: <https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/>"
      + "PREFIX fibo-fnd-pas-pas: <https://spec.edmcouncil.org/fibo/ontology/FND/ProductsAndServices/ProductsAndServices/>"
      + "PREFIX geo: <http://www.opengis.net/ont/geosparql#>"
      + "PREFIX ontobim: <https://www.theworldavatar.com/kg/ontobim/>"
      + "PREFIX ontoservice: <https://www.theworldavatar.com/kg/ontoservice/>"
      + "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>";

  // Private constructor to prevent instantiation
  private StringResource() {
    throw new UnsupportedOperationException("This class cannot be instantiated!");
  }

  /**
   * Get a string from the specified field name of the input field.
   * 
   * @param field     object containing the string.
   * @param fieldName target field key.
   */
  public static String getNodeString(JsonNode field, String fieldName) {
    return optNodeString(field, fieldName, null);
  }

  /**
   * Get an optional string from the input field.
   * 
   * @param field         object containing the string.
   * @param fieldName     target field key.
   * @param defaultOption default value if there is no such field.
   */
  public static String optNodeString(JsonNode field, String fieldName, String defaultOption) {
    JsonNode fieldNode = field.path(fieldName);
    if (fieldNode.isMissingNode()) {
      if (defaultOption == null) {
        return "";
      }
      return defaultOption;
    }
    return fieldNode.asText();
  }

  /**
   * Appends the triples as a query line in the builder.
   * 
   * @param queryBuilder A query builder for any clause.
   * @param subject      Subject node value for triple.
   * @param predicate    Predicate node value for triple.
   * @param object       Object node value for triple.
   */
  public static void appendTriple(StringBuilder queryBuilder, String subject, String predicate, String object) {
    queryBuilder.append(subject)
        .append(ShaclResource.WHITE_SPACE).append(predicate).append(ShaclResource.WHITE_SPACE)
        .append(object)
        .append(ShaclResource.FULL_STOP);
  }

  /**
   * Wraps the content into an OPTIONAL clause
   * 
   * @param content Content of the optional clause
   */
  public static String genOptionalClause(String content) {
    return "OPTIONAL{" + content + "}";
  }

  /**
   * Get local name of the IRI for namespaces containing # or /.
   * 
   * @param iri Input.
   */
  public static String getLocalName(String iri) {
    if (isValidIRI(iri)) {
      int index = iri.indexOf("#");
      if (index != -1) {
        return iri.substring(index + 1);
      }
      String[] parts = iri.split("/");
      return parts[parts.length - 1];
    }
    return iri;
  }

  /**
   * Parses a SPARQL query variable to ensure that any spaces are replaced.
   * 
   * @param variable Target variable input.
   */
  public static String parseQueryVariable(String variable) {
    return variable.replaceAll("\\s+", "_");
  }

  /**
   * Parses the string literal for SPARQL queries ie enclosing it with "".
   * 
   * @param literal Target literal input.
   */
  public static String parseLiteral(String literal) {
    return "\"" + literal + "\"";
  }

  /**
   * Parses the IRI for SPARQL queries ie enclosing it with <>.
   * 
   * @param iri Target iri input.
   */
  public static String parseIriForQuery(String iri) {
    if (isValidIRI(iri)) {
      return "<" + iri + ">";
    }
    throw new IllegalArgumentException(MessageFormat.format("Invalid IRI for: {0}", iri));
  }

  /**
   * Validates if the input is an IRI or not.
   * 
   * @param iri Input.
   */
  public static boolean isValidIRI(String iri) {
    try {
      URI uri = new URI(iri);
      // Check if the URI has valid scheme, path, etc
      return uri.getScheme() != null && uri.getHost() != null;
    } catch (URISyntaxException e) {
      // If a URISyntaxException is thrown, the string is not a valid IRI
      return false;
    }
  }

  /**
   * Retrieve the prefix of the input IRI.
   * 
   * @param iri Input.
   */
  public static String getPrefix(String iri) {
    if (isValidIRI(iri)) {
      int lastSlashIndex = iri.lastIndexOf("/");
      return iri.substring(0, lastSlashIndex);
    }
    throw new IllegalArgumentException("Invalid IRI! Does not conform to RFC2396 specifications.");
  }
}