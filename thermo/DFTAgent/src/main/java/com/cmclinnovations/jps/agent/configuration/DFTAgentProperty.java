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
	
	@Value("${dft.agent.rdf4j.server.url}")
	private String rdf4jServerURL;

	
	@Value("${dft.agent.rdf4j.ontokin.repository.id}")
	private String ontoKinRepositoryID;

	public String getThermoAgentHttpRequestFirstPart() {
		return thermoAgentHttpRequestFirstPart;
	}

	public String getOntoKinRepositoryIRI() {
		return ontoKinRepositoryIRI;
	}

	public String getRdf4jServerURL() {
		return rdf4jServerURL;
	}

	public String getOntoKinRepositoryID() {
		return ontoKinRepositoryID;
	}
}

