package com.cmclinnovations.ontochem.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all TBoxes provided in the tbox.management.properties file.
 * 
 * This will empower users to use OntoChem, if some
 * of its IRI or anything change at a later stage,
 * without changing the source code of OntoChem.</br>
 * 
 * @author msff2
 *
 */
@Configuration
@PropertySource("classpath:kb.ontochem.management.properties")
public class OntoChemKB {
	@Value("${ontochem.kb.tbox.iri}")
	private String ontoKinKbTBoxIri;

	@Value("${ontochem.kb.abox.iri}")
	private String ontoKinKbABoxIri;

	@Value("${ontochem.kb.root.directory}")
	private String ontoKinKbRootDirectory;

	@Value("${ontochem.kb.url}")
	private String ontoKinKbURL;

	@Value("${ontochem.kb.abox.file.name}")
	private String ontoKinKbAboxFileName;

	@Value("${ontochem.ontology.file.path}")
	private String ontoChemOntolgyFilePath;
	
	@Value("${ontochem.ontology.file.name}")
	private String ontoChemOntolgyFileName;

	@Value("${ontochem.kb.file.path}")
	private String ontoChemKBFilePath;

	@Value("${ontochem.kb.file.name}")
	private String ontoChemKBFileName;
	
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

	public String getOntoChemOntolgyFilePath() {
		return ontoChemOntolgyFilePath;
	}

	public void setOntoChemOntolgyFilePath(String ontoChemOntolgyFilePath) {
		this.ontoChemOntolgyFilePath = ontoChemOntolgyFilePath;
	}

	public String getOntoChemOntolgyFileName() {
		return ontoChemOntolgyFileName;
	}

	public void setOntoChemOntolgyFileName(String ontoChemOntolgyFileName) {
		this.ontoChemOntolgyFileName = ontoChemOntolgyFileName;
	}

	public String getOntoChemKBFilePath() {
		return ontoChemKBFilePath;
	}

	public void setOntoChemKBFilePath(String ontoChemKBFilePath) {
		this.ontoChemKBFilePath = ontoChemKBFilePath;
	}

	public String getOntoChemKBFileName() {
		return ontoChemKBFileName;
	}

	public void setOntoChemKBFileName(String ontoChemKBFileName) {
		this.ontoChemKBFileName = ontoChemKBFileName;
	}
}
