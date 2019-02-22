package uk.ac.cam.cares.jps.base.config;

public interface JPSConstants {

	// TODO-AE SC 20190218 rename the constant value and use prefix jps to make them unique, merge this interface with others (e.g. from config)
	static String SCENARIO_URL = "scenariourl";
	static String SCENARIO_RESOURCE = "scenarioresource";
	static String SCENARIO_AGENT = "scenarioagent";
	static String SCENARIO_AGENT_OPERATION = "scenarioagentoperation";
	
	static String QUERY_SPARQL_QUERY = "sparqlquery";
	static String QUERY_SPARQL_UPDATE = "sparqlupdate";
	static String QUERY_FILE = "file";
	
	
	
	/**
	 * If true then all read files are copied into the scenario bucket.
	 * If false then copy-on-write, i.e. files are only copied into the scenario bucket if they are changed.
	 */
	// TODO-AE SC make this configurable
	static boolean COPY_ON_READ = true;
}
