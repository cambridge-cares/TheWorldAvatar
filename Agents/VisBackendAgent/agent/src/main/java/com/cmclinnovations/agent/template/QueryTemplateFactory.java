package com.cmclinnovations.agent.template;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.List;
import java.util.Queue;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.utils.StringResource;

public class QueryTemplateFactory implements ShaclTemplateFactory {
  private String parentField;
  private final Queue<String> optionalLines;

  private static final String CLAZZ_VAR = "clazz";
  private static final String NAME_VAR = "name";
  private static final String IS_OPTIONAL_VAR = "isoptional";
  private static final String IS_PARENT_VAR = "isparent";
  private static final String SUBJECT_VAR = "subject";
  private static final String PATH_PREFIX = "_proppath";
  private static final String MULTIPATH_FIRST_VAR = "multipath1";
  private static final String MULTIPATH_SEC_VAR = "multipath2";
  private static final String MULTIPATH_THIRD_VAR = "multipath3";
  private static final String MULTISUBPATH_FIRST_VAR = "multisubpath1";
  private static final String MULTISUBPATH_SEC_VAR = "multisubpath2";
  private static final String MULTISUBPATH_THIRD_VAR = "multisubpath3";
  private static final String MULTISUBPATH_FORTH_VAR = "multisubpath4";
  private static final List<String> propertyPathParts = Arrays.asList(
      MULTIPATH_FIRST_VAR,
      MULTIPATH_SEC_VAR,
      MULTIPATH_THIRD_VAR,
      MULTISUBPATH_FIRST_VAR,
      MULTISUBPATH_SEC_VAR,
      MULTISUBPATH_THIRD_VAR,
      MULTISUBPATH_FORTH_VAR);

  private static final Logger LOGGER = LogManager.getLogger(QueryTemplateFactory.class);

  /**
   * Constructs a new query template factory.
   * 
   */
  public QueryTemplateFactory() {
    this.optionalLines = new ArrayDeque<>();
  }

  /**
   * Generate a SPARQL query template to get the required data. It will typically
   * be in the following format:
   * 
   * SELECT * WHERE {
   * ?iri a <clazz_iri>;
   * <prop_path>/<sub_path> ?property1;
   * <prop_path>/<prop_path2> ?property2.
   * }
   * 
   * @param bindings  The bindings queried from SHACL restrictions that should
   *                  be in the template.
   * @param filterId  An optional field to target the query at a specific
   *                  instance.
   * @param hasParent Indicates if the query needs to filter out parent entities.
   */
  public String genGetTemplate(List<SparqlBinding> bindings, String filterId, boolean hasParent) {
    // Validate inputs
    if (hasParent && filterId == null) {
      LOGGER.error("Detected a parent without a valid filter ID!");
      throw new IllegalArgumentException("Detected a parent without a valid filter ID!");
    }

    LOGGER.info("Generating a query template for getting data...");
    String whereQueryClause = this.parseBindingsToWhereClause(bindings, hasParent);
    StringBuilder query = new StringBuilder();
    query.append("SELECT * WHERE {")
        .append(whereQueryClause)
        .append(".");
    appendOptionalFilters(query, filterId, hasParent);
    // Parse the optional lines
    while (!this.optionalLines.isEmpty()) {
      query.append(this.optionalLines.poll());
    }
    return query.append("}").toString();
  }

  /**
   * Parses the bindings into the WHERE clause of the SPARQL query. It aims to
   * create the following format:
   * 
   * ?iri a <clazz_iri>;
   * <prop_path>/<sub_path> ?property1;
   * <prop_path>/<prop_path2> ?property2.
   * 
   * @param bindings  The bindings queried from SHACL restrictions that should
   *                  be queried in template.
   * @param hasParent Indicates if the query needs to filter out parent entities.
   */
  private String parseBindingsToWhereClause(List<SparqlBinding> bindings, boolean hasParent) {
    // Set up the first line with an IRI variable, along with the target class
    StringBuilder query = new StringBuilder("?iri a ");
    // Retrieve the target class from the first binding
    String targetClass = bindings.get(0).getFieldValue(CLAZZ_VAR);
    query.append(StringResource.parseIriForQuery(targetClass));

    // Iterate through each binding and create a new line for each binding to reach
    // the field via SPARQL ie path1/part2/subpath1/subpath2 fieldName
    bindings.forEach(binding -> genPredSubjectLine(query, binding, hasParent));
    return query.toString();
  }

  /**
   * A helper function to generate the predicate subject parts ie
   * <prop_path>/<sub_path> ?property1
   * 
   * @param query     The query template builder
   * @param binding   An individual binding queried from SHACL restrictions that
   *                  should be queried in template.
   * @param hasParent Indicates if the query needs to filter out parent entities.
   */
  private void genPredSubjectLine(StringBuilder query, SparqlBinding binding, boolean hasParent) {
    // Parse predicate path
    StringBuilder predSubjectLineBuilder = new StringBuilder();
    propertyPathParts.forEach(propertyPathPart -> {
      if (binding.containsField(propertyPathPart)) {
        String predPath = binding.getFieldValue(propertyPathPart);
        // Do not process any paths without the http protocol as it is likely to be a
        // blank node
        if (predPath.startsWith("http")) {
          // If this is not the first part, there should be a / to denote a sequence path
          if (predSubjectLineBuilder.length() > 0) {
            predSubjectLineBuilder.append("/");
          }
          this.appendPropertyPathExpressionIfAvailable(predSubjectLineBuilder, propertyPathPart, binding);
          predSubjectLineBuilder.append(StringResource.parseIriForQuery(predPath));
        }
      }
    });
    // Parse the variable name
    String propertyName = binding.getFieldValue(NAME_VAR).replaceAll("\\s+", "_");
    predSubjectLineBuilder.append(" ?").append(propertyName);

    // If the field is a parent field, and the template requires a parent, store the
    // parent field
    if (Boolean.parseBoolean(binding.getFieldValue(IS_PARENT_VAR)) && hasParent) {
      this.parentField = propertyName;
    }

    // Verify if these bindings are optional or not to determine what to execute
    boolean isOptional = Boolean.parseBoolean(binding.getFieldValue(IS_OPTIONAL_VAR));
    // If it is optional, store the optional lines into a separate queue so that
    // they can be generated separately at the end of the WHERE clause
    if (isOptional) {
      // FORMAT: OPTIONAL{?iri <proppath> ?var}
      String optionalLine = "OPTIONAL{?iri " + predSubjectLineBuilder + ".";
      // If the value must conform to a specific subject variable, filter it
      if (binding.containsField(SUBJECT_VAR)) {
        // FORMAT: FILTER{?var =<subject>}
        optionalLine += "FILTER(?" + propertyName + "=<" + binding.getFieldValue(SUBJECT_VAR) + ">)";
      }
      this.optionalLines.offer(optionalLine + "}");
    } else {
      // If it is non-optional, simply append it to the WHERE clause
      // Append an ; as per sparql convention to break up the property from the
      // previous line
      query.append(";");
      // Construct a new line with a new variable for the SPARQL query
      query.append(predSubjectLineBuilder);
    }
  }

  /**
   * Appends an additional property path expression only if it is available
   * 
   * @param builder          Builder for the query template.
   * @param propertyPathPart The current property path part variable name.
   * @param binding          An individual binding queried from SHACL restrictions
   */
  private void appendPropertyPathExpressionIfAvailable(StringBuilder builder, String propertyPathPart,
      SparqlBinding binding) {
    if (binding.containsField(propertyPathPart + PATH_PREFIX)) {
      // Separate the clauses as there may be other path prefixes in future
      // For inverse paths, simply append a ^ before
      if (binding.getFieldValue(propertyPathPart + PATH_PREFIX)
          .equals(SHACL_PREFIX + "inversePath")) {
        builder.append("^");
      }
    }
  }

  /**
   * Appends optional filters to the query if required.
   * 
   * @param query     Builder for the query template.
   * @param filterId  An optional field to target the query at a specific
   *                  instance.
   * @param hasParent Indicates if the query needs to filter out parent entities.
   */
  private void appendOptionalFilters(StringBuilder query, String filterId, boolean hasParent) {
    if (hasParent) {
      if (this.parentField == null) {
        LOGGER.error("Detected a parent but no valid parent fields are available!");
        throw new IllegalArgumentException("Detected a parent but no valid parent fields are available!");
      }
      query.append("FILTER STRENDS(STR(?")
          .append(this.parentField)
          .append("), \"")
          .append(filterId)
          .append("\")");
    } else if (filterId != null) {
      // Add filter clause if there is a valid filter ID
      query.append("FILTER STRENDS(STR(?id), \"")
          .append(filterId)
          .append("\")");
    }
  }
}
