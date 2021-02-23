package com.cmclinnovations.prime.species.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:kb.ontoprimespecies.management.properties")
public class OntoPrimeSpeciesKBConfig {
	@Value("${ontospecies.kb.tbox.iri}")
	private String ontoSpeciesKbTBoxIri;

	@Value("${ontospecies.kb.abox.iri}")
	private String ontoSpeciesKbABoxIri;

	@Value("${ontospecies.kb.root.directory}")
	private String ontoSpeciesKbRootDirectory;

	@Value("${ontospecies.kb.url}")
	private String ontoSpeciesKbURL;

	@Value("${ontospecies.kb.abox.file.name}")
	private String ontoSpeciesKbAboxFileName;

	@Value("${ontospecies.ontology.file.path}")
	private String ontoSpeciesOntolgyFilePath;
	
	@Value("${ontospecies.ontology.file.name}")
	private String ontoSpeciesOntolgyFileName;

	@Value("${ontospecies.kb.file.path}")
	private String ontoSpeciesKBFilePath;

	@Value("${ontospecies.kb.file.name}")
	private String ontoSpeciesKBFileName;
	
	@Value("${ontospecies.namespace}")
	private String ontoSpeciesNamespace;
	
	@Value("${ontokin.kb.tbox.iri}")
	private String ontoKinKbTBoxIri;
	
	@Value("${ontokin.namespace}")
	private String ontoKinNamespece;
	
	
	public String getOntoSpeciesNamespace() {
		return ontoSpeciesNamespace;
	}

	public void setOntoSpeciesNamespace(String ontoSpeciesNamespace) {
		this.ontoSpeciesNamespace = ontoSpeciesNamespace;
	}

	public String getOntoSpeciesKbTBoxIri() {
		return ontoSpeciesKbTBoxIri;
	}

	public void setOntoSpeciesKbTBoxIri(String ontoSpeciesKbTBoxIri) {
		this.ontoSpeciesKbTBoxIri = ontoSpeciesKbTBoxIri;
	}

	public String getOntoSpeciesKbABoxIri() {
		return ontoSpeciesKbABoxIri;
	}

	public void setOntoSpeciesKbABoxIri(String ontoSpeciesKbABoxIri) {
		this.ontoSpeciesKbABoxIri = ontoSpeciesKbABoxIri;
	}

	public String getOntoSpeciesKbRootDirectory() {
		return ontoSpeciesKbRootDirectory;
	}

	public void setOntoSpeciesKbRootDirectory(String ontoSpeciesKbRootDirectory) {
		this.ontoSpeciesKbRootDirectory = ontoSpeciesKbRootDirectory;
	}

	public String getOntoSpeciesKbURL() {
		return ontoSpeciesKbURL;
	}

	public void setOntoSpeciesKbURL(String ontoSpeciesKbURL) {
		this.ontoSpeciesKbURL = ontoSpeciesKbURL;
	}

	public String getOntoSpeciesKbAboxFileName() {
		return ontoSpeciesKbAboxFileName;
	}

	public void setOntoSpeciesKbAboxFileName(String ontoSpeciesKbAboxFileName) {
		this.ontoSpeciesKbAboxFileName = ontoSpeciesKbAboxFileName;
	}

	public String getOntoSpeciesOntolgyFilePath() {
		return ontoSpeciesOntolgyFilePath;
	}

	public void setOntoSpeciesOntolgyFilePath(String ontoSpeciesOntolgyFilePath) {
		this.ontoSpeciesOntolgyFilePath = ontoSpeciesOntolgyFilePath;
	}

	public String getOntoSpeciesOntolgyFileName() {
		return ontoSpeciesOntolgyFileName;
	}

	public void setOntoSpeciesOntolgyFileName(String ontoSpeciesOntolgyFileName) {
		this.ontoSpeciesOntolgyFileName = ontoSpeciesOntolgyFileName;
	}

	public String getOntoSpeciesKBFilePath() {
		return ontoSpeciesKBFilePath;
	}

	public void setOntoSpeciesKBFilePath(String ontoSpeciesKBFilePath) {
		this.ontoSpeciesKBFilePath = ontoSpeciesKBFilePath;
	}

	public String getOntoSpeciesKBFileName() {
		return ontoSpeciesKBFileName;
	}

	public void setOntoSpeciesKBFileName(String ontoSpeciesKBFileName) {
		this.ontoSpeciesKBFileName = ontoSpeciesKBFileName;
	}
	
	public String getOntoKinNamespace() {
		return ontoKinNamespece;
	}

	public void setOntoKinNamespace(String ontoKinNamespece) {
		this.ontoKinNamespece = ontoKinNamespece;
	}

	public String getOntoKinKbTBoxIri() {
		return ontoKinKbTBoxIri;
	}

	public void setOntoKinKbTBoxIri(String ontoKinKbTBoxIri) {
		this.ontoKinKbTBoxIri = ontoKinKbTBoxIri;
	}
	
	
	
	
	
	@Value("${ontoprimespecies.kb.tbox.iri}")
	private String ontoPrimeSpeciesKbTBoxIri;

	@Value("${ontoprimespecies.kb.abox.iri}")
	private String ontoPrimeSpeciesKbABoxIri;

	@Value("${ontoprimespecies.kb.root.directory}")
	private String ontoPrimeSpeciesKbRootDirectory;

	@Value("${ontoprimespecies.kb.url}")
	private String ontoPrimeSpeciesKbURL;

	@Value("${ontoprimespecies.kb.abox.file.name}")
	private String ontoPrimeSpeciesKbAboxFileName;

	@Value("${ontoprimespecies.ontology.file.path}")
	private String ontoPrimeSpeciesOntolgyFilePath;
	
	@Value("${ontoprimespecies.ontology.file.name}")
	private String ontoPrimeSpeciesOntolgyFileName;

	@Value("${ontoprimespecies.kb.file.path}")
	private String ontoPrimeSpeciesKBFilePath;

	@Value("${ontoprimespecies.kb.file.name}")
	private String ontoPrimeSpeciesKBFileName;
	
	@Value("${ontoprimespecies.namespace}")
	private String ontoPrimeSpeciesNamespace;
	
	@Value("${ontospecies.kb.file.head.comment}")
	private String ontoSpeciesHeadComment;
	
	public String getOntoPrimeSpeciesNamespace() {
		return ontoPrimeSpeciesNamespace;
	}

	public void setOntoPrimeSpeciesNamespace(String ontoPrimeSpeciesNamespace) {
		this.ontoPrimeSpeciesNamespace = ontoPrimeSpeciesNamespace;
	}

	public String getOntoPrimeSpeciesKbTBoxIri() {
		return ontoPrimeSpeciesKbTBoxIri;
	}

	public void setOntoPrimeSpeciesKbTBoxIri(String ontoPrimeSpeciesKbTBoxIri) {
		this.ontoPrimeSpeciesKbTBoxIri = ontoPrimeSpeciesKbTBoxIri;
	}

	public String getOntoPrimeSpeciesKbABoxIri() {
		return ontoPrimeSpeciesKbABoxIri;
	}

	public void setOntoPrimeSpeciesKbABoxIri(String ontoPrimeSpeciesKbABoxIri) {
		this.ontoPrimeSpeciesKbABoxIri = ontoPrimeSpeciesKbABoxIri;
	}

	public String getOntoPrimeSpeciesKbRootDirectory() {
		return ontoPrimeSpeciesKbRootDirectory;
	}

	public void setOntoPrimeSpeciesKbRootDirectory(String ontoPrimeSpeciesKbRootDirectory) {
		this.ontoPrimeSpeciesKbRootDirectory = ontoPrimeSpeciesKbRootDirectory;
	}

	public String getOntoPrimeSpeciesKbURL() {
		return ontoPrimeSpeciesKbURL;
	}

	public void setOntoPrimeSpeciesKbURL(String ontoPrimeSpeciesKbURL) {
		this.ontoPrimeSpeciesKbURL = ontoPrimeSpeciesKbURL;
	}

	public String getOntoPrimeSpeciesKbAboxFileName() {
		return ontoPrimeSpeciesKbAboxFileName;
	}

	public void setOntoPrimeSpeciesKbAboxFileName(String ontoPrimeSpeciesKbAboxFileName) {
		this.ontoPrimeSpeciesKbAboxFileName = ontoPrimeSpeciesKbAboxFileName;
	}

	public String getOntoPrimeSpeciesOntolgyFilePath() {
		return ontoPrimeSpeciesOntolgyFilePath;
	}

	public void setOntoPrimeSpeciesOntolgyFilePath(String ontoPrimeSpeciesOntolgyFilePath) {
		this.ontoPrimeSpeciesOntolgyFilePath = ontoPrimeSpeciesOntolgyFilePath;
	}

	public String getOntoPrimeSpeciesOntolgyFileName() {
		return ontoPrimeSpeciesOntolgyFileName;
	}

	public void setOntoPrimeSpeciesOntolgyFileName(String ontoPrimeSpeciesOntolgyFileName) {
		this.ontoPrimeSpeciesOntolgyFileName = ontoPrimeSpeciesOntolgyFileName;
	}

	public String getOntoPrimeSpeciesKBFilePath() {
		return ontoPrimeSpeciesKBFilePath;
	}

	public void setOntoPrimeSpeciesKBFilePath(String ontoPrimeSpeciesKBFilePath) {
		this.ontoPrimeSpeciesKBFilePath = ontoPrimeSpeciesKBFilePath;
	}

	public String getOntoPrimeSpeciesKBFileName() {
		return ontoPrimeSpeciesKBFileName;
	}

	public void setOntoPrimeSpeciesKBFileName(String ontoPrimeSpeciesKBFileName) {
		this.ontoPrimeSpeciesKBFileName = ontoPrimeSpeciesKBFileName;
	}

	public String getOntoSpeciesHeadComment() {
		return ontoSpeciesHeadComment;
	}

	public void setOntoSpeciesHeadComment(String ontoSpeciesHeadComment) {
		this.ontoSpeciesHeadComment = ontoSpeciesHeadComment;
	}
}
