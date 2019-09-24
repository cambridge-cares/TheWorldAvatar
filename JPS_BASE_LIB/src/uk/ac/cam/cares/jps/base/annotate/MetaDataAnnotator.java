package uk.ac.cam.cares.jps.base.annotate;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;
import java.util.UUID;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.log.JPSBaseLogger;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService.RDFStoreType;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;

public class MetaDataAnnotator implements Prefixes {
	
	private static MetaDataAnnotator instance = null;
	private SparqlOverHttpService sparqlService = null;
	
	static synchronized MetaDataAnnotator getInstance() {
		if (instance == null) {
			instance = new MetaDataAnnotator();
		}
		return instance;
	}
	
	private MetaDataAnnotator() {
		// String datasetUrl = "http://localhost:8081/rdfdataset/jpsmetadata";
		String datasetUrl = KeyValueManager.get(IKeys.URL_RDF_METADATA);
		sparqlService = new SparqlOverHttpService(RDFStoreType.FUSEKI, datasetUrl);
	}
	
	public static SparqlOverHttpService getSparqlService() {
		return getInstance().sparqlService;
	}

	public static void annotateWithCurrentTime(String iri) {
		long millis = System.currentTimeMillis();
		String time = getTimeInXsdTimeStampFormat(millis);
		MetaDataAnnotator.getInstance().annotateWithTimeAndAgentInternally(iri, time, null);
	}
	
	public static void annotateWithTime(String iri, String time) {
		MetaDataAnnotator.getInstance().annotateWithTimeAndAgentInternally(iri, time, null);
	}
	
	public static void annotateWithTimeAndAgent(String iriTarget, String time, String iriGeneratingAgent) {
		MetaDataAnnotator.getInstance().annotateWithTimeAndAgentInternally(iriTarget, time, iriGeneratingAgent);
	}
	
	private void annotateWithTimeAndAgentInternally(String iri, String time, String iriGeneratingAgent) {
		JPSBaseLogger.info(this, "annotating target = " + iri + " with time = " + time + ", generatingAgent = " + iriGeneratingAgent);
			
		String sparql = getSparqlInsertAnnotateWithTimeAndAgent(iri, time, iriGeneratingAgent);
		sparqlService.executePost(sparql);		
	}
	
	public static String getSparqlInsertAnnotateWithTimeAndAgent(String iri, String time, String iriGeneratingAgent) {
		StringBuffer sparql = new StringBuffer("PREFIX dcterms:<" + PrefixToUrlMap.getPrefixUrl(DCTERMS) + "> \r\n");
		sparql.append("PREFIX time:<" + PrefixToUrlMap.getPrefixUrl(TIME) + "> \r\n");
		sparql.append("PREFIX xsd:<" + PrefixToUrlMap.getPrefixUrl(XSD) + "> \r\n");
		sparql.append("INSERT DATA { \r\n");
		if (time != null) {	
			sparql.append("<?time> a time:Instant . \r\n");
			sparql.append("<?time> time:inXSDDateTime \"" + time + "\"^^xsd:dateTime . \r\n");
			sparql.append("<" + iri + "> time:hasTime <?time> . \r\n");
		}
		if (iriGeneratingAgent != null) {
			sparql.append("<" + iriGeneratingAgent + "> a dcterms:Agent . \r\n");
			sparql.append("<" + iri + "> dcterms:creator <" + iriGeneratingAgent + "> . \r\n");
		}
		
		sparql.append("} \r\n");		
		
		return replaceByUUID(sparql.toString(), "?time");
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
	
	private static String replaceByUUID(String s, String replace) {
		String uuid = UUID.randomUUID().toString();
		String iri = "http://www.theworldavatar.com/kb/metadata/" + uuid;
		return s.replace(replace, iri);
	}
}