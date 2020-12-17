package uk.ac.cam.cares.jps.base.annotate;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService.RDFStoreType;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

public class MetaDataAnnotator implements Prefixes {
	
	private static MetaDataAnnotator instance = null;
	private SparqlOverHttpService sparqlService = null;
	private String datasetUrl = null;
	
	static synchronized MetaDataAnnotator getInstance() {
		if (instance == null) {
			instance = new MetaDataAnnotator();
		}
		return instance;
	}
	
	private MetaDataAnnotator() {
		//String datasetUrl = "http://localhost:8080/rdfdataset/jpsmetadata";
		//String datasetUrl = KeyValueManager.get(IKeys.URL_RDF_METADATA);
		String oldDatasetUrl = "http://localhost:8080/rdf4j-server/repositories/jpsmetadata";
		sparqlService = new SparqlOverHttpService(RDFStoreType.RDF4J, oldDatasetUrl);
		datasetUrl = KeyValueManager.get(IKeys.DATASET_META_URL);
	}
	
	public static SparqlOverHttpService getSparqlService() {
		//return getInstance().sparqlService;
		return null;
	}
	
	public static String getMetadataSetUrl() {
		return getInstance().datasetUrl;
	}
	
	public static void update(String sparql) {
		//MetaDataAnnotator.getInstance().sparqlService.executePost(sparql);
		KnowledgeBaseClient.update(getMetadataSetUrl(), null, sparql);
	}
	
	public static void annotateWithTimeAndAgent(String iriTarget, String time, String iriCreatingAgent) {
		String sparql = getSparqlInsertFull(iriTarget, null, time, iriCreatingAgent, false, time, null, null, null, null);
		update(sparql);	
	}
	
	public static void annotate(String iriTarget, MediaType mediaType, String iriCreatingAgent, boolean addJPSContext, List<String> topics) {
		String sparql = getSparqlInsert(iriTarget, null, null, iriCreatingAgent, addJPSContext, topics, null, null);
		update(sparql);	
	}
	
	public static void annotate(String iriTarget, MediaType mediaType, String iriCreatingAgent, boolean addJPSContext, List<String> topics, String creationTime) {
		String sparql = getSparqlInsert(iriTarget, null, creationTime, iriCreatingAgent, addJPSContext, topics, null, null);
		update(sparql);
	}

	public static String getSparqlInsert(String iriTarget, MediaType mediaType, String creationTime, String iriCreatingAgent, 
			boolean addJPSContext, List<String> topics, List<String> prefixes, List<String> triples) {

		long millis = System.currentTimeMillis();
		if (creationTime == null) {
			creationTime = getTimeInXsdTimeStampFormat(millis);
		}
		
		String simulationTime = creationTime;
		String iriScenario = JPSContext.getScenarioUrl();
		if (addJPSContext) {
			// if the JPS context specifies a simulation time, overtake it
			String contextTime = JPSContext.getSimulationTime();
			if ((contextTime != null) && !contextTime.isEmpty()) {
				simulationTime = contextTime;
			}
			
		}
		
		return getSparqlInsertFull(iriTarget, mediaType, creationTime, iriCreatingAgent, addJPSContext, simulationTime, iriScenario, topics, prefixes, triples);
	}
	
	/**
	 * @param prefixes
	 * @param iriTarget the resource that should be annotated - required
	 * @param mediaType
	 * @param creationTime the real time at which the target resource was created - required
	 * @param iriCreatingAgent
	 * @param simulationTime the time for which the simulation run (possibly in the past or future) - required
	 * @param iriScenario
	 * @param topics
	 * @param triples
	 * @return
	 */
	public static String getSparqlInsertFull(String iriTarget, MediaType mediaType, String creationTime, String iriCreatingAgent, 
			boolean addJPSContext, String simulationTime, String iriScenario, List<String> topics, List<String> prefixes, List<String> triples) {
		
		JPSBaseLogger.info(getInstance(), "annotating target = " + iriTarget + " with media type = " + mediaType 
				+ ", creation time = " + creationTime + ", creatingAgent = " + iriCreatingAgent + ", simulation time = " + simulationTime + ", scenario = " + iriScenario );
		
		StringBuffer sparql = new StringBuffer();
		sparql.append(PrefixToUrlMap.getPrefixForSPARQL(DCTERMS));
		//sparql.append(PrefixToUrlMap.getPrefixForSPARQL(FOAF));
		//sparql.append(PrefixToUrlMap.getPrefixForSPARQL(TIME));
		sparql.append(PrefixToUrlMap.getPrefixForSPARQL(XSD));
		//sparql.append(PrefixToUrlMap.getPrefixForSPARQL(JPSAGEN));

		if (prefixes != null) {
			for (String current : prefixes) {
				sparql.append(current);
				if (!current.endsWith("\n")) {
					sparql.append(" \r\n");
				}
			}
		}
		if (iriScenario == null) {
			sparql.append("INSERT DATA { \r\n");
		} else {
			sparql.append("INSERT DATA { GRAPH <" + iriScenario + "> { \r\n");
		}
		
		if (mediaType != null) {
			sparql.append("<" + iriTarget + "> dcterms:format \"" + mediaType.type + "\" . \r\n");
		}
		sparql.append("<" + iriTarget + "> dcterms:created \"" + creationTime + "\"^^xsd:dateTime . \r\n");
		if (iriCreatingAgent != null) {
			sparql.append("<" + iriCreatingAgent + "> a dcterms:Agent . \r\n");			
			//sparql.append("<" + iriCreatingAgent + "> a JPSAGEN:Service . \r\n");
			sparql.append("<" + iriCreatingAgent + "> a <http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Service> . \r\n");
			sparql.append("<" + iriTarget + "> dcterms:creator <" + iriCreatingAgent + "> . \r\n");
		}
		//TODO-AE SC URGENT 20190918 discuss with Kevin what have to be changed for the ADMS soft sensor
		//sparql.append("<?time> a time:Instant . \r\n");
		//sparql.append("<?time> time:inXSDDateTime \"" + simulationTime + "\"^^xsd:dateTime . \r\n");
		//sparql.append("<" + iriTarget + "> time:hasTime <?time> . \r\n");
		sparql.append("<" + iriTarget + "> dcterms:date \"" + simulationTime + "\"^^xsd:dateTime . \r\n");
		// TODO-AE SC URGENT 20190916 Scenario Ontology and Namespace doesn't exist. We lend terms from other ontologies here
		if (iriScenario != null) {
			//sparql.append("<" + iriScenario + "> a JPSAGEN:Scenario . \r\n");
			sparql.append("<" + iriTarget + "> dcterms:isPartOf <" + iriScenario + "> . \r\n");
		}
		List<String> optionalTriples = new ArrayList<String>();
		if (topics != null) {
			for (String current : topics) {
				//optionalTriples.add("<" + iriTarget + "> foaf:topic " + current + " .");
				optionalTriples.add("<" + iriTarget + "> dcterms:subject <" + current + "> .");
			}
		}
		if (triples != null) {
			optionalTriples.addAll(triples);
			
		}
		for (String current : optionalTriples) {
			sparql.append(current + " \r\n");
		}
		
		if (iriScenario == null) {
			sparql.append("} \r\n");	
		} else {
			sparql.append("} } \r\n");	
		}
			
		
		//return replaceByUUID(sparql.toString(), "?time");
		return sparql.toString();
	}
	
	public static String getTimeInXsdTimeStampFormat(long millis) { 
		Date date = new Date(millis);
	    SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS"); 
		format.setTimeZone(TimeZone.getTimeZone("GMT+0:00"));
	    String timestamp = format.format(date);
	    return timestamp.replace(" ", "T");
	} 
	
	public static long getMillisFromXsdTimeStampFormat(String timestamp) { 
		timestamp = timestamp.replace("T", " ");	
	    SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS"); 
		format.setTimeZone(TimeZone.getTimeZone("GMT+0:00"));
		Date date;
		try {
			date = format.parse(timestamp);
		} catch (ParseException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
		return date.getTime();
	} 
}