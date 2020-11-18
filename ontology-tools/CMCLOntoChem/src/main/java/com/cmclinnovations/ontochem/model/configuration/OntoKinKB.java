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
public class OntoKinKB {
	@Value("${ontokin.kb.tbox.iri}")
	private String ontoKinKbTBoxIri;

	@Value("${ontokin.kb.abox.iri}")
	private String ontoKinKbABoxIri;

	@Value("${ontokin.kb.root.directory}")
	private String ontoKinKbRootDirectory;

	@Value("${ontokin.kb.url}")
	private String ontoKinKbURL;

	@Value("${ontokin.kb.abox.file.name}")
	private String ontoKinKbAboxFileName;

	public String getOntoKinKbTBoxIri() {
		return ontoKinKbTBoxIri;
	}
	
	public void setOntoKinKbTBoxIri(String ontoKinKbTBoxIri) {
		this.ontoKinKbTBoxIri = ontoKinKbTBoxIri;
	}
	
	public String getOntoKinKbABoxIri() {
		return ontoKinKbABoxIri;
	}

	public void setOntoKinKbABoxIri(String ontoKinKbABoxIri) {
		this.ontoKinKbABoxIri = ontoKinKbABoxIri;
	}

	public String getOntoKinKbRootDirectory() {
		return ontoKinKbRootDirectory;
	}

	public void setOntoKinKbRootDirectory(String ontoKinKbRootDirectory) {
		this.ontoKinKbRootDirectory = ontoKinKbRootDirectory;
	}

	public String getOntoKinKbURL() {
		return ontoKinKbURL;
	}

	public void setOntoKinKbURL(String ontoKinKbURL) {
		this.ontoKinKbURL = ontoKinKbURL;
	}

	public String getOntoKinKbAboxFileName() {
		return ontoKinKbAboxFileName;
	}

	public void setOntoKinKbAboxFileName(String ontoKinKbAboxFileName) {
		this.ontoKinKbAboxFileName = ontoKinKbAboxFileName;
	}
}
