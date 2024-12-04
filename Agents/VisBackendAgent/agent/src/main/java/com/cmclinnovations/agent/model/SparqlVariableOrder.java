package com.cmclinnovations.agent.model;

/**
 * A record of a SPARQL variable and its order.
 * 
 * @param property Name of the property variable
 * @param order    The primary order for the property
 * @param subOrder The secondary order for the property
 */
public record SparqlVariableOrder(String property, int order, int subOrder) {
}