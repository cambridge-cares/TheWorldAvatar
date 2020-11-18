package com.cmclinnovations.ontochem.model.converter.json;

import java.io.IOException;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.slf4j.Logger;

import com.cmclinnovations.ontochem.model.owl.OwlConstructWriter;

public class CompChemOwlWriter extends CompChem {
	private Logger logger = org.slf4j.LoggerFactory.getLogger(CompChemOwlWriter.class);
	
	public OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
	public OWLOntology ontology;
	public OWLDataFactory dataFactory = OWLManager.getOWLDataFactory();
	public String basePathABox;
	
	public static void main(String[] args) throws IOException {
		CompChemOwlWriter ccw = new CompChemOwlWriter();
	}
	
	public void createMechanism(String instance, String dataPropertyValue){
		// Creates an instance of a mechanism
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		// Creates the value of the data property being created
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyValue);
		// Reads the data property
//		OWLDataProperty dataProperty = dataFactory.getOWLDataProperty(dataPropertyIRI);
//		manager.applyChange(new AddAxiom(ontology,
//				dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, individual, literal)));
	}

	public void createProvenance(){
		
	}
	
	public void createPhase(){
		
	}
	
	public void createSpecies(){
		
	}
	
	/**
	 * Creates an OWL class using OWLDataFactory.
	 * </br>
	 * To enable the creation of the class, its name and URL forming
	 * path should be provided.
	 * 
	 * @param ontoFactory an instance of OWLDataFactory.
	 * @param owlFilePath the path for forming the URL. 
	 * @param className the name of the class.
	 * @return an OWL class.
	 * @see OWLDataFactory
	 */
	private OWLClass createOWLClass(OWLDataFactory ontoFactory, String owlFilePath, String className){
		return ontoFactory.getOWLClass(owlFilePath.concat("#").concat(className));
	}
	
	private OWLIndividual createOWLIndividual(OWLDataFactory ontoFactory, String owlFilePath, String individualName){
		return ontoFactory.getOWLNamedIndividual(owlFilePath.concat("#").concat(individualName));
	}
	
	private OWLDataProperty createOWLDataProperty(OWLDataFactory dataFactory, String iri, String propertyName, String separator){
		return dataFactory.getOWLDataProperty(iri.concat(separator).concat(propertyName));
	}
	
	private OWLLiteral createOWLLiteral(OWLDataFactory ontoFactory, String literal){
		return ontoFactory.getOWLLiteral(literal);
	}

	public void init(){
		
	}
}
