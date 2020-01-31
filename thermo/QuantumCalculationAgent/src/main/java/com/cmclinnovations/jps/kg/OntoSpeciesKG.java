package com.cmclinnovations.jps.kg;

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
@PropertySource("classpath:jps-project.properties")
public class OntoSpeciesKG {
	@Value("${ontochem.kb.tbox.iri}")
	private String ontoChemKbTBoxIri;

	@Value("${ontochem.kb.root.directory}")
	private String ontoChemKbRootDirectory;

	@Value("${ontochem.kb.url}")
	private String ontoChemKbURL;

	@Value("${ontochem.ontology.file.path}")
	private String ontoChemOntolgyFilePath;
	
	@Value("${ontochem.ontology.file.name}")
	private String ontoChemOntolgyFileName;

	@Value("${ontochem.kb.file.path}")
	private String ontoChemKBFilePath;
	
	@Value("${ontochem.kb.rdf4j.server.url}")
	private String ontoChemKBRDF4JServerUrl;
	
	@Value("${ontochem.kb.rdf4j.repository.id}")
	private String ontoChemKBRDF4JRepositoryId;

	@Value("${ontochem.kb.tbox.prefix}")
	private String ontoChemKBTBoxPrefix;

	public String getOntoChemKbTBoxIri() {
		return ontoChemKbTBoxIri;
	}

	public void setOntoChemKbTBoxIri(String ontoChemKbTBoxIri) {
		this.ontoChemKbTBoxIri = ontoChemKbTBoxIri;
	}

	public String getOntoChemKbRootDirectory() {
		return ontoChemKbRootDirectory;
	}

	public void setOntoChemKbRootDirectory(String ontoChemKbRootDirectory) {
		this.ontoChemKbRootDirectory = ontoChemKbRootDirectory;
	}

	public String getOntoChemKbURL() {
		return ontoChemKbURL;
	}

	public void setOntoChemKbURL(String ontoChemKbURL) {
		this.ontoChemKbURL = ontoChemKbURL;
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

	public String getOntoChemKBRDF4JServerUrl() {
		return ontoChemKBRDF4JServerUrl;
	}

	public void setOntoChemKBRDF4JServerUrl(String ontoChemKBRDF4JServerUrl) {
		this.ontoChemKBRDF4JServerUrl = ontoChemKBRDF4JServerUrl;
	}

	public String getOntoChemKBRDF4JRepositoryId() {
		return ontoChemKBRDF4JRepositoryId;
	}

	public void setOntoChemKBRDF4JRepositoryId(String ontoChemKBRDF4JRepositoryId) {
		this.ontoChemKBRDF4JRepositoryId = ontoChemKBRDF4JRepositoryId;
	}

	public String getOntoChemKBTBoxPrefix() {
		return ontoChemKBTBoxPrefix;
	}

	public void setOntoChemKBTBoxPrefix(String ontoChemKBTBoxPrefix) {
		this.ontoChemKBTBoxPrefix = ontoChemKBTBoxPrefix;
	}
}
