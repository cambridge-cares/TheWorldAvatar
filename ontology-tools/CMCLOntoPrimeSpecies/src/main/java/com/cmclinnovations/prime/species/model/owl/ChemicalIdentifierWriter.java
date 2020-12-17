package com.cmclinnovations.prime.species.model.owl;

import static com.cmclinnovations.prime.species.model.owl.IPreferredKeyWriter.logger;

import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontology.model.exception.ABoxManagementException;
import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_identifier.Name;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;

public class ChemicalIdentifierWriter extends PrimeSpeciesConverter implements IChemicalIdentifierWriter {
	public void writer(char ch[], int start, int length) throws SAXException {
		readNames(ch, start, length);
	}
	
	private void readNames(char ch[], int start, int length) throws SAXException {
		if (nameParseStatus.isName()) {
			String source = name.getSource();
			String type = name.getType();
			
//			String value = new String(ch, start, length);
//			name.setValue(value);
//			
//			
//			
//			if (type != null && !type.isEmpty()) {
//				switch (type) {
//				case "CASRegistryNumber":
//					addCASRegistryNumber();
//					break;
//				case "formula":
//					createEmpiricalFormula();
//					break;
//				case "InChI":
//					addInChI();
//					break;
//				default:
//					addAltLabel();
//				}
//			} else {
//				addAltLabel();
//			}
//			
//			nameParseStatus.setName(false);
//			name = new Name();
		}
	}
	
	public void writeName() {
		if (name.getType() != null && !name.getType().isEmpty()) {
			switch (name.getType()) {
			case "CASRegistryNumber":
				addCASRegistryNumber();
				break;
			case "formula":
				createEmpiricalFormula();
				break;
			case "InChI":
				addInChI();
				break;
			default:
				addAltLabel();
			}
		} else {
			addAltLabel();
		}
		
		nameParseStatus.setName(false);
		name = new Name();
	}
	
	private void addCASRegistryNumber() {
		try {
			
			if (name.getValue() != null && !name.getValue().isEmpty()) {
				iABoxManagement.addProperty("Species"+UNDERSCORE+chemicalSpeciesInstanceID, 
						ontoPrimeSpeciesVocabulary.getOSDataPropertyhasCasRegistryID(), 
						name.getValue(), STRING);
			}
			
		} catch (ABoxManagementException e) {
			logger.error("CASRegistryNumber could not be added to an individual of chemicalSpecies.");
		}
	}
	
	private void createEmpiricalFormula() {
		try {
			
			if (name.getValue() != null && !name.getValue().isEmpty()) {
				
				if (!empiricalFormulaCreated) {
					iABoxManagement.createIndividual(ontoPrimeSpeciesVocabulary.getOSClassEmpiricalFormula(), 
							"EmpiricalFormula"+UNDERSCORE+empiricalFormulaInstanceID);
					
					empiricalFormulaCreated = true;
					
					iABoxManagement.addObjectProperty(ontoPrimeSpeciesVocabulary.getOSObjPropertyhasEmpiricalFormula(), 
							"Species"+UNDERSCORE+chemicalSpeciesInstanceID, 
							"EmpiricalFormula"+UNDERSCORE+empiricalFormulaInstanceID);
				}
				
				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
				iABoxManagement.addProperty("EmpiricalFormula"+UNDERSCORE+empiricalFormulaInstanceID, 
						dataPropertyIRI, name.getValue(), STRING);

			}
			
		} catch (ABoxManagementException e) {
			logger.error("EmpiricalFormula could not be added to an individual of chemicalSpecies.");
		}
	}
	
	private void addInChI() {
		try {
			
			if (name.getValue() != null && !name.getValue().isEmpty()) {
				iABoxManagement.addProperty("Species"+UNDERSCORE+chemicalSpeciesInstanceID, 
						ontoPrimeSpeciesVocabulary.getOSDataPropertyhasInChI(), 
						name.getValue(), STRING);
			}
			
		} catch (ABoxManagementException e) {
			logger.error("InChI could not be added to an individual of chemicalSpecies.");
		}
	}
	
	private void addAltLabel() {
		try {
			
			if (name.getValue() != null && !name.getValue().isEmpty()) {
				IRI dataPropertyIRI = IRI.create(SKOS_URL.concat(ontoPrimeSpeciesVocabulary.getOSDataPropertyhasAltLabel()));
				iABoxManagement.addProperty("Species"+UNDERSCORE+chemicalSpeciesInstanceID, 
						dataPropertyIRI, name.getValue().replace("\n", ""), STRING);
			}
			
		} catch (ABoxManagementException e) {
			logger.error("AltLabel could not be added to an individual of chemicalSpecies.");
		}
	}
}
