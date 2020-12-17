package com.cmclinnovations.ontochem.model.configuration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * Reads values of all KB IRIs provided in the jps.kb.iris.properties file.
 * 
 * This will empower users to use OntoKin, if some
 * of its IRI or anything change at a later stage,
 * without changing the source code of OntoKin.</br>
 * 
 * @author msff2
 *
 */
@Configuration
@PropertySource("classpath:jps.kb.iris.properties")
public class JpsKnowledgeBase {
	@Value("${jps.kb.iris}")
	private String jpsKbIris;

	public String getJpsKbIris() {
		return jpsKbIris;
	}

	public void setJpsKbIris(String jpsKbIris) {
		this.jpsKbIris = jpsKbIris;
	}
}