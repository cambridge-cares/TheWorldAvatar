package com.cmclinnovations.prime.species.model.converter.owl.query.sparql;

import java.util.ArrayList;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.prime.species.model.converter.owl.OwlConverter;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_composition.Atom;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.chemical_identifier.Name;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;

public class ChemicalSpeciesQuery extends OwlConverter implements IChemicalSpeciesQuery {
static Logger logger = LoggerFactory.getLogger(ChemicalSpeciesQuery.class);
	
	public void query() throws OntoPrimeSpeciesException {
		queryInstance(ontoPrimeSpeciesKB.getOntoSpeciesNamespace().concat(COLON),
				ontoPrimeSpeciesKB.getOntoSpeciesKbTBoxIri(), 
				ontoPrimeSpeciesVocabulary.getOSClassSpecies());
		
		ArrayList<String> chemicalSpeciesInstance = queryResult;
		queryResult = new ArrayList<>();
		if (chemicalSpeciesInstance.size()>0 && chemicalSpeciesInstance.size()<2) {
			
			setUPAttribs(chemicalSpeciesInstance.get(0));
			setUpPreferredKey(chemicalSpeciesInstance.get(0));
			setUpCASNumber(chemicalSpeciesInstance.get(0));
			setUpEmpiricalFormula(chemicalSpeciesInstance.get(0));
			setUpOtherNames(chemicalSpeciesInstance.get(0));
			setUpInChI(chemicalSpeciesInstance.get(0));
			chemicalIdentifier.setName(nameList);
			nameList = new ArrayList<Name>();
			chemicalSpecies.setChemicalIdentifier(chemicalIdentifier);
			
		} else if (chemicalSpeciesInstance.size() > 1) {
			logger.error("There should not be more than one instance of chemicalSpecies in an OWL file.");
		} else {
			logger.error("No instance of chemicalSpecies found in an OWL file");
		}
	}
	
	private void setUPAttribs(String instance) {
		String xmlns = "http://purl.org/NET/prime/";
		String xmlnsXsi = "http://www.w3.org/2001/XMLSchema-instance";
		String xsiSchemaLocation = "http://purl.org/NET/prime/ http://warehouse.primekinetics.org/schema/species.xsd";
		chemicalSpecies.setXmlns(xmlns);
		chemicalSpecies.setXmlnsXsi(xmlnsXsi);
		chemicalSpecies.setXsiSchemaLocation(xsiSchemaLocation);
	}
	
	private void setUpPreferredKey(String instance) {
		ArrayList<String> values = readRDFSLabel(instance);	
		if (values != null && !values.isEmpty()) {
			String value = values.get(0);
			if (value != null && !value.isEmpty()) {
				preferredKey.setValue(value);
				preferredKey.setGroup("prime");
				preferredKey.setType("formula");
			}
			chemicalSpecies.setPreferredKey(preferredKey);
		}
	}
	
	private void setUpCASNumber(String instance) {
		
		ArrayList<String> cass = readDataPropertyCasRegistryID(instance);
		if (cass != null && !cass.isEmpty()) {
			for (String cas : cass) {
				if (cas != null && !cas.isEmpty()) {
					name.setValue(cas);
					name.setSource("@NIST");
					name.setType("CASRegistryNumber");
					nameList.add(name);
					name = new Name();
				}
			}
		}
	}
	
	private void setUpEmpiricalFormula(String instance) {
		ArrayList<String> empiricalFormulaInstance = readObjProperty(ontoPrimeSpeciesKB.getOntoSpeciesNamespace(), 
				ontoPrimeSpeciesKB.getOntoSpeciesKbTBoxIri(), 
				instance, ontoPrimeSpeciesVocabulary.getOSObjPropertyhasEmpiricalFormula());
		
		if (empiricalFormulaInstance != null && !empiricalFormulaInstance.isEmpty()) {
			if (empiricalFormulaInstance.size()>0 && empiricalFormulaInstance.size()<2) {
				ArrayList<String> empiFormulas = readRDFSLabel(empiricalFormulaInstance.get(0));
				
				for (String empiFormula : empiFormulas) {
					if (empiFormula != null && !empiFormula.isEmpty()) {
						name.setValue(empiFormula);
						name.setSource("@NIST");
						name.setType("formula");
						nameList.add(name);
						name = new Name();
					}
				}
				
				setUpChemicalComposition(empiricalFormulaInstance.get(0));
			} else if (empiricalFormulaInstance.size()>1) {
				logger.error("There should not be more than one instance of empiricalFormula for a species.");
			}
		}
	}
	
	private void setUpOtherNames(String instance) {
		ArrayList<String> otherNames = readDataPropertyAltLabel(instance);
		if (otherNames != null && !otherNames.isEmpty()) {
			for (String otherName : otherNames) {
				if (otherName != null && !otherName.isEmpty()) {
					name.setValue(otherName);
					name.setSource("@NIST");
					nameList.add(name);
					name = new Name();
				}
			}
		}
	}
	
	private void setUpInChI(String instance) {
		String inChI = readDataProperty(ontoPrimeSpeciesKB.getOntoSpeciesNamespace(), 
				ontoPrimeSpeciesKB.getOntoSpeciesKbTBoxIri(), 
				instance, ontoPrimeSpeciesVocabulary.getOSDataPropertyhasInChI());
		if (inChI != null && !inChI.isEmpty()) {
			name.setValue(inChI);
			name.setType("InChI");
			nameList.add(name);
			name = new Name();
		}
	}
	
	private void setUpChemicalComposition(String instance) {
		ArrayList<String> elementNumbers = readObjProperty(ontoPrimeSpeciesKB.getOntoKinNamespace(), 
				ontoPrimeSpeciesKB.getOntoKinKbTBoxIri(), 
				instance, ontoPrimeSpeciesVocabulary.getOSObjPropertyhasElementNumber());
		
		ArrayList<String> elements = readObjProperty(ontoPrimeSpeciesKB.getOntoKinNamespace(), 
				ontoPrimeSpeciesKB.getOntoKinKbTBoxIri(), 
				instance, ontoPrimeSpeciesVocabulary.getOSObjPropertyhasElement());

		if (elementNumbers != null && !elementNumbers.isEmpty()) {
			if (elementNumbers.size() == elements.size()) {
				for (String elementNumber : elementNumbers) {
				
					String indicatedElement = readObjProperty(ontoPrimeSpeciesKB.getOntoKinNamespace(), 
							ontoPrimeSpeciesKB.getOntoKinKbTBoxIri(), 
							elementNumber, 
							ontoPrimeSpeciesVocabulary.getOSObjPropertyindicatesNumberOf()).get(0);
					
					if (indicatedElement != null && !indicatedElement.isEmpty()) {
						String symbol = indicatedElement.substring(8, indicatedElement.length());
						String value = readDataProperty(ontoPrimeSpeciesKB.getOntoKinNamespace(), ontoPrimeSpeciesKB.getOntoKinKbTBoxIri(), 
								elementNumber, ontoPrimeSpeciesVocabulary.getOSDataPropertyhasNumberOfElement());
						
						if (symbol != null && !symbol.isEmpty()
							&& value != null && !value.isEmpty()) {
						atom.setSymbol(symbol);
						atom.setValue(value);
						atomList.add(atom);
						atom = new Atom();
						} else {
							logger.error("Symbol and value of atom imcomplete.");
						}
					} else {
						logger.error("ElementNumber is not linked to Element.");
					}
					
				}
				chemicalComposition.setAtom(atomList);
				chemicalSpecies.setChemicalComposition(chemicalComposition);
			} else {
				logger.error("The Element doesn't match ElementNumber.");
			}
		} else {
			logger.error("Element doesn't exist for EmpiricalFormula");
		}
	}
}
