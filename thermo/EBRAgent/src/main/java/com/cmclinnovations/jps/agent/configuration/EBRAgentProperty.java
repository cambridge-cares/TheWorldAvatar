package com.cmclinnovations.jps.agent.configuration;

import java.util.List;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

import com.cmclinnovations.jps.agent.ebr.Utils;

/**
 * This class reads all inputs from the ebr-agent.properties file, that is<br>
 * to say it reads:
 * - multiple comma separated endpoints for ontospecies knowledge graph
 * - multiple comma separated endpoints for ontocompchem knowledge graph
 * 
 * @author msff2
 *
 */
@Configuration
@PropertySource("classpath:ebr-agent.properties")
public class EBRAgentProperty {

	@Value("${ebr.agent.ontospecies.kb.endpoints}")
	private String ontoSpeciesKBEndPoints;
	
	@Value("${ebr.agent.ontocompchem.kb.endpoints}")
	private String ontoCompChemKBEndPoints;
	
	@Value("${ebr.agent.ontospecies.kb.single.endpoint}")
	private String ontoSpeciesKBSingleEndPoint;
	
	@Value("${ebr.agent.ontocompchem.kb.single.endpoint}")
	private String ontoCompChemKBSingleEndPoint;
	
	/**
	 * Both the OntoSpecies and OntoCompChem endpoints can be listed here.<br>
	 * This will be used in performing federated queries which involve the<br>
	 * use of both knowledge bases.  
	 * 
	 */
	@Value("${ebr.agent.endpoints}")
	private String ebrAgentEndPoints;
	
	@Value("${ebr.agent.ontospecies.repsitory.id}")
	private String ontoSpeciesRepositoryID;
	
	@Value("${ebr.agent.ontocompchem.repsitory.id}")
	private String ontoCompChemRepositoryID;

	@Value("${ebr.agent.cross.validation.process}")
	private String ebrAgentCrossValidation;

	@Value("${ebr.agent.eof.calculation}")
	private String ebrAgentCalculation;

	@Value("${ebr.agent.invalid.species.folder}")
	private String ebrAgentInvalidSpeciesFolder;

	@Value("${ebr.agent.invalid.species.file}")
	private String ebrAgentInvalidSpeciesFile;

	@Value("${ebr.agent.ontospecies.abox.base.url}")
	private String ebrAgentOntoSpeciesABoxBaseUrl;

	@Value("${ebr.agent.ontospecies.abox.file.extension}")
	private String ebrAgentOntoSpeciesABoxFileExtension;
	
	public List<String> getOntoSpeciesKBEndPoints() {
		return Utils.convertStringToList(ontoSpeciesKBEndPoints, ",");
	}

	public List<String> getOntoCompChemKBEndPoints() {
		return Utils.convertStringToList(ontoCompChemKBEndPoints, ",");
	}
	
	public String getOntoSpeciesKBSingleEndPoint() {
		return ontoSpeciesKBSingleEndPoint;
	}

	public String getOntoCompChemKBSingleEndPoint() {
		return ontoCompChemKBSingleEndPoint;
	}

	public List<String> getEbrAgentEndPoints() {
		return Utils.convertStringToList(ebrAgentEndPoints, ",");
	}

	public String getOntoSpeciesRepositoryID() {
		return ontoSpeciesRepositoryID;
	}

	public String getOntoCompChemRepositoryID() {
		return ontoCompChemRepositoryID;
	}

	public String getEbrAgentCrossValidation() {
		return ebrAgentCrossValidation;
	}

	public String getEbrAgentCalculation() {
		return ebrAgentCalculation;
	}

	public String getEbrAgentInvalidSpeciesFolder() {
		return ebrAgentInvalidSpeciesFolder;
	}

	public String getEbrAgentInvalidSpeciesFile() {
		return ebrAgentInvalidSpeciesFile;
	}

	public String getEbrAgentOntoSpeciesABoxBaseUrl() {
		return ebrAgentOntoSpeciesABoxBaseUrl;
	}

	public String getEbrAgentOntoSpeciesABoxFileExtension() {
		return ebrAgentOntoSpeciesABoxFileExtension;
	}
}