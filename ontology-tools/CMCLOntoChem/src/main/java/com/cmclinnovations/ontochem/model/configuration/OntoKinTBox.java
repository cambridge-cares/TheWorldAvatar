package com.cmclinnovations.ontochem.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all TBoxes provided in the tbox.management.properties file.
 * 
 * This will empower users to use OntoKin, if some
 * of its IRI or anything change at a later stage,
 * without changing the source code of OntoKin.</br>
 * 
 * @author msff2
 *
 */
@Configuration
@PropertySource("classpath:kb.management.properties")
public class OntoKinTBox {
	@Value("${ontokin.kb.tbox.iri}")
	private String ontoKinKbTBoxIri;

	public String getOntoKinKbTBoxIri() {
		return ontoKinKbTBoxIri;
	}

	public void setOntoKinKbTBoxIri(String ontoKinKbTBoxIri) {
		this.ontoKinKbTBoxIri = ontoKinKbTBoxIri;
	}
}