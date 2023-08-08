package com.cmclinnovations.aermod.sparqlbuilder;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Operand;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;

public final class GeoSPARQL {
	public static Operand sfIntersects(Variable a, Variable b) {
		return () -> "geof:sfIntersects(" + a.getQueryString() + "," + b.getQueryString() + ")";
	}
}