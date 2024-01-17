package com.cmclinnovations.aermod.sparqlbuilder;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Operand;
import org.eclipse.rdf4j.sparqlbuilder.core.QueryElement;

public final class GeoSPARQL {
    public static Operand sfWithin(QueryElement a, QueryElement b) {
        return () -> "geof:sfWithin(" + a.getQueryString() + "," + b.getQueryString() + ")";
    }
}