package com.cmclinnovations.ship;
import java.util.List;

import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

public class ValuesPattern<T> implements GraphPattern {
	List<T> values;
	Variable variable;
	Class<T> valuesClass;
	
    public ValuesPattern(Variable variable, List<T> values, Class<T> valuesClass) {
    	this.values = values;
    	this.variable = variable;
		this.valuesClass = valuesClass;
    }
	
	@Override
	public String getQueryString() {
		StringBuilder bld = new StringBuilder();
		bld.append(String.format("VALUES %s {", variable.getQueryString()));
		for (T value : values) {
			if (valuesClass == Iri.class) {
				bld.append(((Iri) value).getQueryString() + " ");
			} else if (Number.class.isAssignableFrom(valuesClass)) { 
				bld.append(value + " ");
			} else if (valuesClass == String.class) {
				bld.append("\"" + value.toString() + "\" ");
			}
		}
		bld.append("}");
		return bld.toString();
	}
}
