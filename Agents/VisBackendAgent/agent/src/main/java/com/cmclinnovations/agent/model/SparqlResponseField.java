package com.cmclinnovations.agent.model;

/**
 * A record of an individual field in the SPARQL query response.
 * 
 * @param type     Field value type - URI or literal
 * @param value    Field value
 * @param dataType Indicates the data type of the field
 * @param lang     Optional parameter to denote the language of the value
 *                 returned
 */
public record SparqlResponseField(String type, String value, String dataType, String lang) {
}