package com.cmclinnovations.prime.species.model.owl;

import org.semanticweb.owlapi.model.IRI;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontology.model.exception.ABoxManagementException;
import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.preferred_key.PreferredKey;

public class PreferredKeyWriter extends PrimeSpeciesConverter implements IPreferredKeyWriter {
	public void writer(char ch[], int start, int length) throws SAXException {
		readPreferredKey(ch, start, length);
	}
	
	private void readPreferredKey(char ch[], int start, int length) throws SAXException {
		if (preferredKeyParseStatus.isPreferredKey()) {
			String group = preferredKey.getGroup();
			String type = preferredKey.getType();
			
			String value = new String(ch, start, length);
			preferredKey.setValue(value);
			
			if (preferredKey.getGroup() != null && !preferredKey.getGroup().isEmpty()
					&& preferredKey.getType() != null && !preferredKey.getType().isEmpty()) {
				
				if (group.equals("prime") && type.equals("formula")) {
					nameSpeciesLabel();
				}
				
			}
			
			preferredKeyParseStatus.setPreferredKey(false);
			preferredKey = new PreferredKey();
		}
	}
	
	private void nameSpeciesLabel() {
		try {
			
			if (preferredKey.getValue() != null && !preferredKey.getValue().isEmpty()) {
				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
				iABoxManagement.addProperty("Species"+UNDERSCORE+chemicalSpeciesInstanceID, 
						dataPropertyIRI, preferredKey.getValue(), STRING);
			}
			
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of chemicalSpecies could not be named.");
		}
	}
}
