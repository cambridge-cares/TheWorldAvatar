package com.cmclinnovations.ship;
import java.util.List;

import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

public class ValuesPattern implements GraphPattern {
	List<?> values;
	Variable variable;
	
    public ValuesPattern(Variable variable, List<?> values) {
    	this.values = values;
    	this.variable = variable;
    }
	
	@Override
	public String getQueryString() {
		String queryString = "VALUES " + variable.getQueryString() + " {";
		for (Object value : values) {
			try {
				queryString += ((Iri) value).getQueryString() + " ";
			} catch (ClassCastException e) { // for literals
                if (value instanceof Number) {
                    queryString += String.valueOf(value) + " ";
                } else {
                    queryString += "\"" + value.toString() + "\" ";
                }
			}
		}
		queryString += "}";
		return queryString;
	}
}
