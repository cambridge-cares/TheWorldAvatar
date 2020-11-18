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

	@Value("${kb.root.directory}")
	private String kbRootDirectory;

	@Value("${kb.url}")
	private String kbURL;

	@Value("${ontology.file.path}")
	private String ontolgyFilePath;
	
	@Value("${ontology.file.name}")
	private String ontolgyFileName;

	@Value("${kb.file.path}")
	private String kbFilePath;

	@Value("${kb.file.name}")
	private String kbFileName;

	public String gettBoxIri() {
		return tBoxIri;
	}

	public void settBoxIri(String tBoxIri) {
		this.tBoxIri = tBoxIri;
	}

	public String getKbRootDirectory() {
		return kbRootDirectory;
	}

	public void setKbRootDirectory(String kbRootDirectory) {
		this.kbRootDirectory = kbRootDirectory;
	}

	public String getKbURL() {
		return kbURL;
	}

	public void setKbURL(String kbURL) {
		this.kbURL = kbURL;
	}

	public String getOntolgyFilePath() {
		return ontolgyFilePath;
	}

	public void setOntolgyFilePath(String ontolgyFilePath) {
		this.ontolgyFilePath = ontolgyFilePath;
	}

	public String getOntolgyFileName() {
		return ontolgyFileName;
	}

	public void setOntolgyFileName(String ontolgyFileName) {
		this.ontolgyFileName = ontolgyFileName;
	}

	public String getKbFilePath() {
		return kbFilePath;
	}

	public void setKbFilePath(String kbFilePath) {
		this.kbFilePath = kbFilePath;
	}

	public String getKbFileName() {
		return kbFileName;
	}

	public void setKbFileName(String kbFileName) {
		this.kbFileName = kbFileName;
	}
}
