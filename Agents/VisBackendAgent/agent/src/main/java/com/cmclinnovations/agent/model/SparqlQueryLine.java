package com.cmclinnovations.agent.model;

/**
 * A record of a SPARQL query line with its specific property.
 * 
 * @param property       Name of the property variable, intended to be the
 *                       object of the query line
 * @param instanceClass  Class concept if the property is an instance
 * @param predicate      Predicate of triple for main property
 * @param labelPredicate Predicate of triple to reach the label of the property
 * @param subjectFilter  For the restricted possible value of a subject, if
 *                       available; Else, default to an empty string
 * @param isOptional     Indicates if this query line is optional
 * @param isClazz        Indicates if this query line requires a rdfs:label predicate
 */
public record SparqlQueryLine(String property, String instanceClass, String predicate, String labelPredicate, String subjectFilter,
    boolean isOptional, boolean isClazz) {
}