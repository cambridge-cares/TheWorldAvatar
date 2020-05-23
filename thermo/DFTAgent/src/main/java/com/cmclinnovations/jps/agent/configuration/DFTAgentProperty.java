package com.cmclinnovations.jps.agent.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:dft-agent.properties")
public class DFTAgentProperty {
	@Value("${thermo.agent.http.request.first.part}")
	private String thermoAgentHttpRequestFirstPart;

	@Value("${dft.agent.rdf4j.ontokin.repository.iri}")
	private String ontoKinRepositoryIRI;
	
	public String getThermoAgentHttpRequestFirstPart() {
		return thermoAgentHttpRequestFirstPart;
	}

	public String getOntoKinRepositoryIRI() {
		return ontoKinRepositoryIRI;
	}
}

