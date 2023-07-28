package com.cmclinnovations.dispersion.sparqlbuilder;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Operand;
import org.eclipse.rdf4j.sparqlbuilder.core.QueryElement;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;

public final class GeoSPARQL {
	public static Operand sfIntersects(Variable a, Variable b) {
		return () -> "geof:sfIntersects(" + a.getQueryString() + "," + b.getQueryString() + ")";
	}

	public static Operand sfWithin(QueryElement a, QueryElement b) {
		return () -> "geof:sfWithin(" + a.getQueryString() + "," + b.getQueryString() + ")";
	}
}