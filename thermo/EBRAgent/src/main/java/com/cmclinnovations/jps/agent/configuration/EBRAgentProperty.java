package com.cmclinnovations.jps.agent.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

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

	public String getOntoSpeciesKBEndPoints() {
		return ontoSpeciesKBEndPoints;
	}

	public String getOntoCompChemKBEndPoints() {
		return ontoCompChemKBEndPoints;
	}
}