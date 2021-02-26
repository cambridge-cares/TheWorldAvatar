package com.cmclinnovations.prime.species.model.owl;

import static com.cmclinnovations.prime.species.model.owl.IChemicalIdentifierWriter.logger;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontology.model.exception.ABoxManagementException;
import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.Atom;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;

public class ChemicalCompositionWriter extends PrimeSpeciesConverter implements IChemicalCompositionWriter {
	public void writer(char ch[], int start, int length) throws SAXException {
		readElements(ch, start, length);
	}
	
	private void readElements(char ch[], int start, int length) throws SAXException {
		if (atomParseStatus.isAtom()) {
			String element = atom.getSymbol();
			String value = new String(ch, start, length);
			atom.setValue(value);
			
			if (element != null && !element.isEmpty()) {
				createElement();
				countNumberOfElement();
				establishLinks();
			}
			
			
			
			atomParseStatus.setAtom(false);
			atom = new Atom();
		}
	}
	
	private void createElement() {
		elementCount += 1;
		
		try {
//			iABoxManagement.createIndividual(ontoPrimeSpeciesVocabulary.getOSClassElement(), 
//					"Element"+UNDERSCORE+atom.getSymbol());
			createIndividualFromOtherOntology(ontoPrimeSpeciesKB.getOntoKinKbTBoxIri(), 
					ontoPrimeSpeciesVocabulary.getOSClassElement(), 
					"Element"+UNDERSCORE+atom.getSymbol());
		} catch (OntoPrimeSpeciesException e) {
			logger.error("Element of an individual of chemicalSpecies could not be identified.");
		}
	}
	
	private void countNumberOfElement() {
		try {
//			iABoxManagement.createIndividual(ontoPrimeSpeciesVocabulary.getOSClassElementNumber(), 
//					"ElementNumber"+UNDERSCORE+(empiricalFormulaInstanceID+elementCount));
			
//			iABoxManagement.addProperty("ElementNumber"+UNDERSCORE+(empiricalFormulaInstanceID+elementCount), 
//					ontoPrimeSpeciesVocabulary.getOSDataPropertyhasNumberOfElement(), 
//					atom.getValue(), STRING);
			
			createIndividualFromOtherOntology(ontoPrimeSpeciesKB.getOntoKinKbTBoxIri(), 
					ontoPrimeSpeciesVocabulary.getOSClassElementNumber(), 
					"ElementNumber"+UNDERSCORE+(empiricalFormulaInstanceID+elementCount));
			
			addDataPropertyFromOtherOntology(ontoPrimeSpeciesKB.getOntoKinKbTBoxIri(), 
					"ElementNumber"+UNDERSCORE+(empiricalFormulaInstanceID+elementCount), 
					ontoPrimeSpeciesVocabulary.getOSDataPropertyhasNumberOfElement(), 
					atom.getValue(), STRING);
			
		} catch (OntoPrimeSpeciesException e) {
			logger.error("NumberOfElement of an Element of an individual of chemicalSpecies could not be counted.");
		}
	}
	
	private void establishLinks() {
		try {
//			iABoxManagement.addObjectProperty(ontoPrimeSpeciesVocabulary.getOSObjPropertyhasElement(), 
//					"EmpiricalFormula"+UNDERSCORE+empiricalFormulaInstanceID, 
//					"Element"+UNDERSCORE+atom.getSymbol());
			
			addObjectPropertyFromOtherOntology(ontoPrimeSpeciesKB.getOntoKinKbTBoxIri(), 
					ontoPrimeSpeciesVocabulary.getOSObjPropertyhasElement(), 
					"EmpiricalFormula"+UNDERSCORE+empiricalFormulaInstanceID, 
					"Element"+UNDERSCORE+atom.getSymbol());
			
		} catch (OntoPrimeSpeciesException e) {
			logger.error("Element could not be linked to the EmpiricalFormula it belongs to.");
		}
		
		try {
//			iABoxManagement.addObjectProperty(ontoPrimeSpeciesVocabulary.getOSObjPropertyhasElementNumber(), 
//					"EmpiricalFormula"+UNDERSCORE+empiricalFormulaInstanceID, 
//					"ElementNumber"+UNDERSCORE+(empiricalFormulaInstanceID+elementCount));
			
			addObjectPropertyFromOtherOntology(ontoPrimeSpeciesKB.getOntoKinKbTBoxIri(), 
					ontoPrimeSpeciesVocabulary.getOSObjPropertyhasElementNumber(), 
					"EmpiricalFormula"+UNDERSCORE+empiricalFormulaInstanceID, 
					"ElementNumber"+UNDERSCORE+(empiricalFormulaInstanceID+elementCount));
			
		} catch (OntoPrimeSpeciesException e) {
			logger.error("ElementNumber could not be linked to the EmpiricalFormula it belongs to.");
		}
		
		try {
//			iABoxManagement.addObjectProperty(ontoPrimeSpeciesVocabulary.getOSObjPropertyindicatesNumberOf(), 
//					"ElementNumber"+UNDERSCORE+(empiricalFormulaInstanceID+elementCount), 
//					"Element"+UNDERSCORE+atom.getSymbol());
			
			addObjectPropertyFromOtherOntology(ontoPrimeSpeciesKB.getOntoKinKbTBoxIri(), 
					ontoPrimeSpeciesVocabulary.getOSObjPropertyindicatesNumberOf(), 
					"ElementNumber"+UNDERSCORE+(empiricalFormulaInstanceID+elementCount), 
					"Element"+UNDERSCORE+atom.getSymbol());
			
		} catch (OntoPrimeSpeciesException e) {
			logger.error("ElementNumber could not be linked to the Element it indicates.");
		}
	}
	
	public OWLIndividual createIndividualFromOtherOntology(String basePath, String clasName, String instance) throws OntoPrimeSpeciesException {
		OWLClass claz = createOWLClass(dataFactory, basePath, clasName);
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		// Adds to the ontology the instance of the class
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(claz, individual)));
		return individual;
	}
	
	public void addObjectPropertyFromOtherOntology(String basePath, String objectPropertyName, String domainInstanceName, String rangeInstanceName) throws OntoPrimeSpeciesException {
		OWLObjectProperty objectProperty = dataFactory.getOWLObjectProperty(basePath.concat(HASH).concat(objectPropertyName));
		OWLIndividual domainIndividual = createOWLIndividual(dataFactory, basePathABox, domainInstanceName);
		OWLIndividual rangeIndividual = createOWLIndividual(dataFactory, basePathABox, rangeInstanceName);
		manager.applyChange(new AddAxiom(ontology, 
				dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, domainIndividual, rangeIndividual)));
	}
	
	public void addDataPropertyFromOtherOntology(String basePath, String instance, String dataPropertyName, String dataPropertyValue, String propertyType) throws OntoPrimeSpeciesException {
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyValue, propertyType);
		OWLDataProperty dataPropertyCreated = createOWLDataProperty(dataFactory, basePath, dataPropertyName, HASH);
		manager.applyChange(new AddAxiom(ontology, 
				dataFactory.getOWLDataPropertyAssertionAxiom(dataPropertyCreated, individual, literal)));
	}
	
	private OWLClass createOWLClass(OWLDataFactory ontoFactory, String owlFilePath, String className) {
		return ontoFactory.getOWLClass(owlFilePath.concat("#").concat(className));
	}
	
	private OWLIndividual createOWLIndividual(OWLDataFactory ontoFactory, String owlFilePath, String individualName) {
		return ontoFactory.getOWLNamedIndividual(owlFilePath.concat(BACKSLASH).concat(individualName));
	}
	
	private OWLDataProperty createOWLDataProperty(OWLDataFactory dataFactory, String iri, String propertyName, String separator) {
		return dataFactory.getOWLDataProperty(iri.concat(separator).concat(propertyName));
	}
	
	private OWLLiteral createOWLLiteral(OWLDataFactory ontoFactory, String literal, String propertyType) throws OntoPrimeSpeciesException {
		if (propertyType.equalsIgnoreCase("string")) {
			return ontoFactory.getOWLLiteral(literal);
		} else if (propertyType.equalsIgnoreCase("integer")) {
			try {
				return ontoFactory.getOWLLiteral(Integer.parseInt(literal));
			} catch (NumberFormatException e) {
				throw new OntoPrimeSpeciesException("The following value is not an integer:"+literal);
			}
		} else if (propertyType.equalsIgnoreCase("float")) {
			try {
				return ontoFactory.getOWLLiteral(Float.parseFloat(literal));
			} catch (NumberFormatException e) {
				throw new OntoPrimeSpeciesException("The following value is not a float:"+literal);
			}
		}
		return ontoFactory.getOWLLiteral(literal);
	}
}
