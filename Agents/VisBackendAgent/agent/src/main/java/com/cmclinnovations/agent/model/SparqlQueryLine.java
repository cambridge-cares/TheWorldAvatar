package com.cmclinnovations.agent.model;

/**
 * A record of a SPARQL query line with its specific property.
 * 
 * @param property Name of the property variable
 * @param contents Query line contents
 */
public record SparqlQueryLine(String property, String contents) {
}