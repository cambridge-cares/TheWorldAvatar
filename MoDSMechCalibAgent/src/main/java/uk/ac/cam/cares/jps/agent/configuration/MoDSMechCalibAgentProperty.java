package uk.ac.cam.cares.jps.agent.configuration;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;

/**
 * This class reads all inputs from the mods-agent.properties file, that is<br>
 * to say it reads:
 * - multiple comma separated endpoints for ontochemexp knowledge graph
 * - multiple comma separated endpoints for ontokin knowledge graph
 * 
 * @author jb2197
 *
 */
@Configuration
@PropertySource("classpath:mods-agent.properties")
public class MoDSMechCalibAgentProperty {
	
}
