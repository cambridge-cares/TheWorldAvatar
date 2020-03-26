package com.cmclinnovations.jps.agent.caller.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:dft-agent-caller.properties")
public class DFTAgentCallerProperty {
	@Value("${sparql.endpoint.ontospecies}")
	private String endpointOntoSpecies;

	@Value("${sparql.endpoint.ontocompchem}")
	private String endpointOntoCompChem;

	public String getEndpointOntoSpecies() {
		return endpointOntoSpecies;
	}

	public String getEndpointOntoCompChem() {
		return endpointOntoCompChem;
	}
}
