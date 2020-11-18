package com.cmclinnovations.ontokin.model.sparql.endpoint;

import java.io.IOException;
import java.util.Map;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import com.cmclinnovations.ontokin.model.exception.OntoException;

/**
 * This interface contains methods that generate SPARQL query response.
 * 
 * @author msff2
 *
 */
public interface IMechanism {
	public Map<String, String> queryIriPlusName() throws OntoException, OWLOntologyCreationException, IOException;
}
