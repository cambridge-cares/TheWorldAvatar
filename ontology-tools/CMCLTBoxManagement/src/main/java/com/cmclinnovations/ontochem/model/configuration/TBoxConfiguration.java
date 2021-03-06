package com.cmclinnovations.ontochem.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all TBoxes provided in the tbox.management.properties file.
 * 
 * This will empower users to use the TBoxManagement tool, if some
 * of its IRI or anything change at a later stage,
 * without changing the source code of TBoxManagement.</br>
 * 
 * @author msff2
 *
 */
@Configuration
@PropertySource("classpath:tbox.management.properties")
public class TBoxConfiguration {
	@Value("${tbox.iri}")
	private String tBoxIri;
	
	@Value("${tbox.version}")
	private String tBoxVersion;	
	
	@Value("${tbox.comment}")
	private String tBoxComment;
	
	@Value("${tbox.git.commit.hash.value}")
	private String gitCommitHashValue;
	
	@Value("${tbox.date.of.publication}")
	private String tBoxCreationDate;
	
	@Value("${tbox.import}")
	private String tBoxImport;
	
	public String gettBoxIri() {
		return tBoxIri;
	}

	public void settBoxIri(String tBoxIri) {
		this.tBoxIri = tBoxIri;
	}

	public String gettBoxVersion() {
		return tBoxVersion;
	}

	public void settBoxVersion(String tBoxVersion) {
		this.tBoxVersion = tBoxVersion;
	}

	public String gettBoxComment() {
		return tBoxComment;
	}

	public void settBoxComment(String tBoxComment) {
		this.tBoxComment = tBoxComment;
	}

	public String getGitCommitHashValue() {
		return gitCommitHashValue;
	}

	public void setGitCommitHashValue(String gitCommitHashValue) {
		this.gitCommitHashValue = gitCommitHashValue;
	}

	public String gettBoxCreationDate() {
		return tBoxCreationDate;
	}

	public void settBoxCreationDate(String tBoxCreationDate) {
		this.tBoxCreationDate = tBoxCreationDate;
	}

	public String gettBoxImport() {
		return tBoxImport;
	}

	public void settBoxImport(String tBoxImport) {
		this.tBoxImport = tBoxImport;
	}
}
