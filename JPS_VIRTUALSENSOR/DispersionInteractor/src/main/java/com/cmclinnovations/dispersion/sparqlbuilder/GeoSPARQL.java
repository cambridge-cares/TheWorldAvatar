package com.cmclinnovations.dispersion.sparqlbuilder;

import org.eclipse.rdf4j.sparqlbuilder.constraint.Operand;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;

public final class GeoSPARQL {
	public static Operand sfIntersects(Variable a, Variable b) {
		return () -> "geof:sfIntersects(" + a.getQueryString() + "," + b.getQueryString() + ")";
	}

	public static Operand sfWithin(String locationString, Variable b) {
		return () -> "geof:sfWithin(" + "\"<http://www.opengis.net/def/crs/EPSG/0/4326> " +
				locationString + "\"^^<http://www.opengis.net/ont/geosparql#wktLiteral>" + "," + b.getQueryString()
				+ ")";
	}
}