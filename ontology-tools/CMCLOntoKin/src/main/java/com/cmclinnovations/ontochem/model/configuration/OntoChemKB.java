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
@PropertySource("classpath:kb.management.properties")
public class OntoChemKB {
	@Value("${kb.tbox.iri}")
	private String ontoKinKbTBoxIri;

	@Value("${kb.root.directory}")
	private String ontoKinKbRootDirectory;

	@Value("${kb.url}")
	private String ontoKinKbURL;

	@Value("${ontology.file.path}")
	private String ontoChemOntolgyFilePath;
	
	@Value("${ontology.file.name}")
	private String ontoChemOntolgyFileName;

	@Value("${kb.file.path}")
	private String ontoChemKBFilePath;
	
	@Value("${kb.rdf4j.server.url}")
	private String ontoChemKBRDF4JServerUrl;
	
	@Value("${kb.rdf4j.repository.id}")
	private String ontoChemKBRDF4JRepositoryId;

	@Value("${kb.tbox.prefix}")
	private String ontoChemKBTBoxPrefix;
	
	@Value("${kb.abox.file.path}")
	private String ontoChemKBABoxFilePath;
	
	@Value("${single.species.mechanism.name.preamble}")
	private String ontoChemKBSingleSpeciesMechPreamble;
	
	@Value("${single.species.mechanism.head.comment}")
	private String ontoChemKBSingleSpeciesMechHeadComment;
	
	public String getOntoKinKbTBoxIri() {
		return ontoKinKbTBoxIri;
	}
	
	public void setOntoKinKbTBoxIri(String ontoKinKbTBoxIri) {
		this.ontoKinKbTBoxIri = ontoKinKbTBoxIri;
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

	public String getOntoChemKBABoxFilePath() {
		return ontoChemKBABoxFilePath;
	}

	public void setOntoChemKBABoxFilePath(String ontoChemKBABoxFilePath) {
		this.ontoChemKBABoxFilePath = ontoChemKBABoxFilePath;
	}

	public String getOntoChemKBSingleSpeciesMechPreamble() {
		return ontoChemKBSingleSpeciesMechPreamble;
	}

	public void setOntoChemKBSingleSpeciesMechPreamble(String ontoChemKBSingleSpeciesMechPreamble) {
		this.ontoChemKBSingleSpeciesMechPreamble = ontoChemKBSingleSpeciesMechPreamble;
	}

	public String getOntoChemKBSingleSpeciesMechHeadComment() {
		return ontoChemKBSingleSpeciesMechHeadComment;
	}

	public void setOntoChemKBSingleSpeciesMechHeadComment(String ontoChemKBSingleSpeciesMechHeadComment) {
		this.ontoChemKBSingleSpeciesMechHeadComment = ontoChemKBSingleSpeciesMechHeadComment;
	}
}
