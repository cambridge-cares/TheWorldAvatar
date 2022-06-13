package uk.ac.cam.cares.jps.agent.flood.sparqlbuilder;

import java.util.List;

import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

public class ValuesPattern implements GraphPattern {
	List<?> values;
	Variable var;
	
    public ValuesPattern(Variable var, List<?> values) {
    	this.values = values;
    	this.var = var;
    }
	
	@Override
	public String getQueryString() {
		String queryString = "VALUES " + var.getQueryString() + " {";
		for (Object value : values) {
			if (value.getClass() == Iri.class) {
				queryString += ((Iri) value).getQueryString() + " ";
			} else {
				queryString += "\"" + value.toString() + "\" ";
			}
		}
		queryString += "}";
		return queryString;
	}
}

