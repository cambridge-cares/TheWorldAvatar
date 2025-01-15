package com.cmclinnovations.agent.model;

/**
 * A record of a SPARQL query line with its specific property.
 * 
 * @param property       Name of the property variable, intended to be the
 *                       object of the query line
 * @param predicate      Predicate of triple for main property
 * @param subPredicate   Predicate of triple for sub property
 * @param labelPredicate Predicate of triple to reach the label of the property
 * @param subjectFilter  For the restricted possible value of a subject, if
 *                       available; Else, default to an empty string
 * @param isOptional     Indicates if this query line is optional
 */
public record SparqlQueryLine(String property, String predicate, String subPredicate, String labelPredicate,
        String subjectFilter, boolean isOptional) {
}