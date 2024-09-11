package com.cmclinnovations.agent.template;

import java.util.Arrays;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.cmclinnovations.agent.model.SparqlBinding;
import com.cmclinnovations.agent.utils.StringResource;

public class QueryTemplateFactory implements ShaclTemplateFactory {
  private static final Logger LOGGER = LogManager.getLogger(QueryTemplateFactory.class);
  private static final String CLAZZ_VAR = "clazz";
  private static final String NAME_VAR = "name";
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

  /**
   * Constructs a new query template factory.
   * 
   */
  public QueryTemplateFactory() {
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
   * @param bindings The bindings queried from SHACL restrictions that should
   *                 be in the template.
   */
  public String genGetTemplate(List<SparqlBinding> bindings) {
    LOGGER.info("Generating a query template for getting data...");
    String whereQueryClause = this.parseBindingsToWhereClause(bindings);
    return "SELECT * WHERE {" + whereQueryClause + ".}";
  }

  /**
   * Parses the bindings into the WHERE clause of the SPARQL query. It aims to
   * create the following format:
   * 
   * ?iri a <clazz_iri>;
   * <prop_path>/<sub_path> ?property1;
   * <prop_path>/<prop_path2> ?property2.
   * 
   * @param bindings The bindings queried from SHACL restrictions that should
   *                 be queried in template.
   */
  private String parseBindingsToWhereClause(List<SparqlBinding> bindings) {
    // Set up the first line with an IRI variable, along with the target class
    StringBuilder query = new StringBuilder("?iri a ");
    // Retrieve the target class from the first binding
    String targetClass = bindings.get(0).getFieldValue(CLAZZ_VAR);
    query.append(StringResource.parseIriForQuery(targetClass));

    // Iterate through each binding and create a new line for each binding to reach
    // the field via SPARQL ie path1/part2/subpath1/subpath2 fieldName
    bindings.forEach(binding -> genPredSubjectLine(query, binding));
    return query.toString();
  }

  /**
   * A helper function to generate the predicate subject parts ie
   * <prop_path>/<sub_path> ?property1
   * 
   * @param query   The query template builder
   * @param binding An individual binding queried from SHACL restrictions that
   *                should be queried in template.
   */
  private void genPredSubjectLine(StringBuilder query, SparqlBinding binding) {
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
    // Append an ; as per sparql convention to break up the property from the
    // previous line
    query.append(";")
        .append(predSubjectLineBuilder); // Construct a new line with a new variable for the SPARQL query
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
}
