package uk.ac.cam.cares.jps.agent.flood.sparqlbuilder;

import java.util.List;

import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

public class ValuesPattern implements GraphPattern {
	List<Iri> values;
	Variable var;
	
    public ValuesPattern(Variable var, List<Iri> values) {
    	this.values = values;
    	this.var = var;
    }
	
	@Override
	public String getQueryString() {
		String queryString = "VALUES " + var.getQueryString() + " {";
		for (Iri value : values) {
			queryString += value.getQueryString() + " ";
		}
		queryString += "}";
		return queryString;
	}
}

