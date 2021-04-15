package com.cmclinnovations.ontochemexp.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all TBox elements provided in 
 * the kb.ontochemexp.management.properties file.
 * 
 * This will empower users to use OntoChemExp, if some
 * of its IRI or anything change at a later stage,
 * without changing its source code.</br>
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
@Configuration
@PropertySource("classpath:kb.ontochemexp.management.properties")
public class OntoChemExpKBConfig {
	@Value("${ontochemexp.kb.tbox.iri}")
	private String ontoChemExpKbTBoxIri;

	@Value("${ontochemexp.kb.abox.iri}")
	private String ontoChemExpKbABoxIri;

	@Value("${ontochemexp.kb.root.directory}")
	private String ontoChemExpKbRootDirectory;

	@Value("${ontochemexp.kb.url}")
	private String ontoChemExpKbURL;

	@Value("${ontochemexp.kb.abox.file.name}")
	private String ontoChemExpKbAboxFileName;

	@Value("${ontochemexp.ontology.file.path}")
	private String ontoChemExpOntolgyFilePath;
	
	@Value("${ontochemexp.ontology.file.name}")
	private String ontoChemExpOntolgyFileName;

	@Value("${ontochemexp.kb.file.path}")
	private String ontoChemExpKBFilePath;

	@Value("${ontochemexp.kb.file.name}")
	private String ontoChemExpKBFileName;
	
	@Value("${ontochemexp.namespace}")
	private String ontoChemNamespace;	
	
	@Value("${ontochemexp.kb.file.head.comment}")
	private String ontoChemExpHeadComment;
	
	@Value("${ontospecies.uniquespeciesiri.kb.server.url}")
	private String ontoSpeciesUniqueSpeciesIRIKBServerURL;
	
	@Value("${ontospecies.uniquespeciesiri.kb.repository.id}")
	private String ontoSpeciesUniqueSpeciesIRIKBRepositoryID;
	
	@Value("${ontospecies.uniquespeciesiri.kb.abox.iri}")
	private String ontoSpeciesUniqueSpeciesIRIKBAboxIRI;
	
	@Value("${files.generation}")
	private String filesGeneration;
	
	@Value("${upload.triple.store.server.url}")
	private String uploadTripleStoreServerURL;
	
	@Value("${upload.triple.store.repository.ontochemexp}")
	private String uploadTripleStoreRepositoryOntoChemExp;
	
	public String getOntoChemNamespace() {
		return ontoChemNamespace;
	}

	public void setOntoChemNamespace(String ontoChemNamespace) {
		this.ontoChemNamespace = ontoChemNamespace;
	}

	public String getOntoExpKbTBoxIri() {
		return ontoChemExpKbTBoxIri;
	}

	public String getOntoChemExpKbTBoxIri() {
		return ontoChemExpKbTBoxIri;
	}

	public void setOntoChemExpKbTBoxIri(String ontoChemExpKbTBoxIri) {
		this.ontoChemExpKbTBoxIri = ontoChemExpKbTBoxIri;
	}

	public String getOntoChemExpKbABoxIri() {
		return ontoChemExpKbABoxIri;
	}

	public void setOntoChemExpKbABoxIri(String ontoChemExpKbABoxIri) {
		this.ontoChemExpKbABoxIri = ontoChemExpKbABoxIri;
	}

	public String getOntoChemExpKbRootDirectory() {
		return ontoChemExpKbRootDirectory;
	}

	public void setOntoChemExpKbRootDirectory(String ontoChemExpKbRootDirectory) {
		this.ontoChemExpKbRootDirectory = ontoChemExpKbRootDirectory;
	}

	public String getOntoChemExpKbURL() {
		return ontoChemExpKbURL;
	}

	public void setOntoChemExpKbURL(String ontoChemExpKbURL) {
		this.ontoChemExpKbURL = ontoChemExpKbURL;
	}

	public String getOntoChemExpKbAboxFileName() {
		return ontoChemExpKbAboxFileName;
	}

	public void setOntoChemExpKbAboxFileName(String ontoChemExpKbAboxFileName) {
		this.ontoChemExpKbAboxFileName = ontoChemExpKbAboxFileName;
	}

	public String getOntoChemExpOntolgyFilePath() {
		return ontoChemExpOntolgyFilePath;
	}

	public void setOntoChemExpOntolgyFilePath(String ontoChemExpOntolgyFilePath) {
		this.ontoChemExpOntolgyFilePath = ontoChemExpOntolgyFilePath;
	}

	public String getOntoChemExpOntolgyFileName() {
		return ontoChemExpOntolgyFileName;
	}

	public void setOntoChemExpOntolgyFileName(String ontoChemExpOntolgyFileName) {
		this.ontoChemExpOntolgyFileName = ontoChemExpOntolgyFileName;
	}

	public String getOntoChemExpKBFilePath() {
		return ontoChemExpKBFilePath;
	}

	public void setOntoChemExpKBFilePath(String ontoChemExpKBFilePath) {
		this.ontoChemExpKBFilePath = ontoChemExpKBFilePath;
	}

	public String getOntoChemExpKBFileName() {
		return ontoChemExpKBFileName;
	}

	public void setOntoChemExpKBFileName(String ontoChemExpKBFileName) {
		this.ontoChemExpKBFileName = ontoChemExpKBFileName;
	}
	
	public String getOntoChemExpHeadComment() {
		return ontoChemExpHeadComment;
	}

	public void setOntoChemExpHeadComment(String ontoChemExpHeadComment) {
		this.ontoChemExpHeadComment = ontoChemExpHeadComment;
	}

	public String getOntoSpeciesUniqueSpeciesIRIKBServerURL() {
		return ontoSpeciesUniqueSpeciesIRIKBServerURL;
	}

	public void setOntoSpeciesUniqueSpeciesIRIKBServerURL(String ontoSpeciesUniqueSpeciesIRIKBServerURL) {
		this.ontoSpeciesUniqueSpeciesIRIKBServerURL = ontoSpeciesUniqueSpeciesIRIKBServerURL;
	}

	public String getOntoSpeciesUniqueSpeciesIRIKBRepositoryID() {
		return ontoSpeciesUniqueSpeciesIRIKBRepositoryID;
	}

	public void setOntoSpeciesUniqueSpeciesIRIKBRepositoryID(String ontoSpeciesUniqueSpeciesIRIKBRepositoryID) {
		this.ontoSpeciesUniqueSpeciesIRIKBRepositoryID = ontoSpeciesUniqueSpeciesIRIKBRepositoryID;
	}

	public String getOntoSpeciesUniqueSpeciesIRIKBAboxIRI() {
		return ontoSpeciesUniqueSpeciesIRIKBAboxIRI;
	}

	public void setOntoSpeciesUniqueSpeciesIRIKBAboxIRI(String ontoSpeciesUniqueSpeciesIRIKBAboxIRI) {
		this.ontoSpeciesUniqueSpeciesIRIKBAboxIRI = ontoSpeciesUniqueSpeciesIRIKBAboxIRI;
	}

	public String getFilesGeneration() {
		return filesGeneration;
	}

	public void setFilesGeneration(String filesGeneration) {
		this.filesGeneration = filesGeneration;
	}

	public String getUploadTripleStoreServerURL() {
		return uploadTripleStoreServerURL;
	}

	public void setUploadTripleStoreServerURL(String uploadTripleStoreServerURL) {
		this.uploadTripleStoreServerURL = uploadTripleStoreServerURL;
	}

	public String getUploadTripleStoreRepositoryOntoChemExp() {
		return uploadTripleStoreRepositoryOntoChemExp;
	}

	public void setUploadTripleStoreRepositoryOntoChemExp(String uploadTripleStoreRepositoryOntoChemExp) {
		this.uploadTripleStoreRepositoryOntoChemExp = uploadTripleStoreRepositoryOntoChemExp;
	}
}
