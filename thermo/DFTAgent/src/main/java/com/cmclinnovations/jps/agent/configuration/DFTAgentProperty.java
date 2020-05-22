package com.cmclinnovations.jps.agent.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

@Configuration
@PropertySource("classpath:dft-agent.properties")
public class DFTAgentProperty {
	@Value("${thermo.agent.http.request.first.part}")
	private String thermoAgentHttpRequestFirstPart;

	public String getThermoAgentHttpRequestFirstPart() {
		return thermoAgentHttpRequestFirstPart;
	}
}

