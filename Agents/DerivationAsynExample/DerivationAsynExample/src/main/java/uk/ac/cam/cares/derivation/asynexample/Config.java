package uk.ac.cam.cares.derivation.asynexample;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import java.util.Properties;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.core.io.ClassPathResource;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class Config {
	public static Properties props = null;
	
	public static String agentIriDifference;
	public static String agentIriMaxValue;
	public static String agentIriMinValue;
	public static String agentIriRNG;
	
	public static String agentHttpUrlDifference;
	public static String agentHttpUrlMaxValue;
	public static String agentHttpUrlMinValue;
	public static String agentHttpUrlRNG;
		
	public static long initDelayAgentDifference;
	public static long initDelayAgentMaxValue;
	public static long initDelayAgentMinValue;
	public static long initDelayAgentRNG;
	
	public static long periodAgentDifference;
	public static long periodAgentMaxValue;
	public static long periodAgentMinValue;
	public static long periodAgentRNG;
	
	public static String derivationInstanceBaseURL;
	
	public static String sparqlEndpoint;
	public static String kgUser;
	public static String kgPassword;
	
	private static final Logger LOGGER = LogManager.getLogger(Config.class);
	
	public static void initProperties() {
		if (props == null) {
			try {
				String agentProps = Paths.get("agents.properties").toString();
				InputStream inputStream = new ClassPathResource(agentProps).getInputStream();
				
				Config.props = new Properties();
				Config.props.load(inputStream);
				
				Config.agentIriDifference = Config.props.getProperty("agent.iri.difference");
				Config.agentIriMaxValue = Config.props.getProperty("agent.iri.maxvalue");
				Config.agentIriMinValue= Config.props.getProperty("agent.iri.minvalue");
				Config.agentIriRNG = Config.props.getProperty("agent.iri.rng");
				
				Config.agentHttpUrlDifference = Config.props.getProperty("deploy.host.base.url") + DifferenceAgent.API_PATTERN;
				Config.agentHttpUrlMaxValue = Config.props.getProperty("deploy.host.base.url") + MaxValueAgent.API_PATTERN;
				Config.agentHttpUrlMinValue= Config.props.getProperty("deploy.host.base.url") + MinValueAgent.API_PATTERN;
				Config.agentHttpUrlRNG = Config.props.getProperty("deploy.host.base.url") + RNGAgent.API_PATTERN;
				
				Config.initDelayAgentDifference = Long.parseLong(Config.props.getProperty("initial.delay.agent.difference"));
				Config.initDelayAgentMaxValue = Long.parseLong(Config.props.getProperty("initial.delay.agent.maxvalue"));
				Config.initDelayAgentMinValue = Long.parseLong(Config.props.getProperty("initial.delay.agent.minvalue"));
				Config.initDelayAgentRNG = Long.parseLong(Config.props.getProperty("initial.delay.agent.rng"));
				
				Config.periodAgentDifference = Long.parseLong(Config.props.getProperty("period.agent.difference"));
				Config.periodAgentMaxValue = Long.parseLong(Config.props.getProperty("period.agent.maxvalue"));
				Config.periodAgentMinValue = Long.parseLong(Config.props.getProperty("period.agent.minvalue"));
				Config.periodAgentRNG = Long.parseLong(Config.props.getProperty("period.agent.rng"));
				
				Config.derivationInstanceBaseURL = Config.props.getProperty("derivation.instance.base.url");
				
				Config.sparqlEndpoint = Config.props.getProperty("kg.url");
				Config.kgUser = Config.props.getProperty("kg.user");
				Config.kgPassword = Config.props.getProperty("kg.password");
			} catch (IOException e) {
				LOGGER.error(e.getMessage());
				throw new JPSRuntimeException(e);
			}
		}
	}
}
