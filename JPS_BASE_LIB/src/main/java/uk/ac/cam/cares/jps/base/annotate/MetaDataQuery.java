package uk.ac.cam.cares.jps.base.annotate;

import java.util.List;

import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.ResultSet;

import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;

public class MetaDataQuery implements Prefixes {

	public static String query(String sparql, String metadataSetUrl) {
		if (metadataSetUrl.isEmpty()) {
			metadataSetUrl = MetaDataAnnotator.getMetadataSetUrl();
			return AccessAgentCaller.query(MetaDataAnnotator.getMetadataSetUrl(), null, sparql);
		}
		//return KnowledgeBaseClient.query(metadataSetUrl, null, sparql);
		String datasetUrl = KeyValueManager.get(IKeys.URL_RDF_METADATA);
		QueryExecution q = QueryExecutionFactory.sparqlService(datasetUrl,sparql);
		ResultSet rs_metadata = q.execSelect();			
		return JenaResultSetFormatter.convertToJSONW3CStandard(rs_metadata);
	}

	public static String query(String sparql) {
		//SparqlOverHttpService sparqlService =  MetaDataAnnotator.getSparqlService();
		//return sparqlService.executeGet(sparql);
		return AccessAgentCaller.query(MetaDataAnnotator.getMetadataSetUrl(), null, sparql);
	}
	
	public static String getSparqlQueryResources(MediaType mediaType, String fromCreationTime, String toCreationTime, 
			String iriCreatingAgent, String fromSimulationTime, String toSimulationTime, String iriScenario, List<String> topics) {
		
		StringBuffer sparql = new StringBuffer();
		sparql.append(PrefixToUrlMap.getPrefixForSPARQL(DCTERMS));
		sparql.append(PrefixToUrlMap.getPrefixForSPARQL(XSD));
		
		sparql.append("SELECT ?resource ?mediatype ?creationTime ?agent ?simulationTime ?scenario \r\n");
		sparql.append("WHERE { \r\n");
		
		sparql.append("OPTIONAL {?resource dcterms:format ?mediatype .}. \r\n");
		if (mediaType != null) {
			sparql.append("?resource dcterms:format \"" + mediaType.type + "\" . \r\n");
		}
		
		sparql.append("OPTIONAL {?resource dcterms:created ?creationTime .}. \r\n");
		if (fromCreationTime != null) {
			sparql.append("FILTER ( ?creationTime >= \"" + fromCreationTime + "\"^^xsd:dateTime ) \r\n");
		}
		if (toCreationTime != null) {
			sparql.append("FILTER ( ?creationTime <= \"" + toCreationTime + "\"^^xsd:dateTime ) \r\n");
		}
		
		sparql.append("OPTIONAL {?resource dcterms:creator ?agent .}. \r\n");
		if (iriCreatingAgent != null) {
			sparql.append("?resource dcterms:creator <" + iriCreatingAgent + "> . \r\n");
		}
		
		sparql.append("OPTIONAL {?resource dcterms:date ?simulationTime .}. \r\n");
		if (fromSimulationTime != null) {
			sparql.append("FILTER ( ?simulationTime >= \"" + fromSimulationTime + "\"^^xsd:dateTime ) \r\n");
		}
		if (toSimulationTime != null) {
			sparql.append("FILTER ( ?simulationTime <= \"" + toSimulationTime + "\"^^xsd:dateTime ) \r\n");
		}
		
		sparql.append("OPTIONAL {?resource dcterms:isPartOf ?scenario .}. \r\n");
		if (iriScenario != null) {
			sparql.append("?resource dcterms:isPartOf <" + iriScenario + "> . \r\n");
		}
		
		if (topics != null) {
			for (String current : topics) {
				sparql.append("?resource dcterms:subject <" + current + "> .");
			}
		}
		
		sparql.append("} ORDER BY DESC(?creationTime) \r\n");
		sparql.append("LIMIT 1000");	
		
		return sparql.toString();
	}
	
	// TODO-AE SC URGENT 20190918 discuss with Kevin and maybe delete the next two methods
	public static String queryResources(String iriCreatingAgent, String fromSimulationTime, String toSimulationTime) {
		String sparql = getSparqlQueryResources(null, null, null, iriCreatingAgent, fromSimulationTime, toSimulationTime, null, null);
		//System.out.println(sparql);
		return query(sparql);
	}
	
	public static String queryResources(MediaType mediaType, String fromCreationTime, String toCreationTime, 
			String iriCreatingAgent, String fromSimulationTime, String toSimulationTime, String iriScenario, List<String> topics) {
		
		String sparql = getSparqlQueryResources(mediaType, fromCreationTime, toCreationTime, iriCreatingAgent, fromSimulationTime, toSimulationTime, iriScenario, topics);
		//System.out.println(sparql);
		return query(sparql);
	}

	public static String queryOldResources(String iriCreatingAgent, String fromSimulationTime, String toSimulationTime,List<String> topics) {
		//String sparql = getSparqlMetaDataResources(fromSimulationTime, toSimulationTime, iriCreatingAgent);
		String sparql = getSparqlQueryResourcesOldRepository(null, null, null, iriCreatingAgent, fromSimulationTime, toSimulationTime, null, topics);
		return query(sparql, KeyValueManager.get(IKeys.URL_RDF_METADATA));
	}
	
	public static String getSparqlQueryResourcesOldRepository(MediaType mediaType, String fromCreationTime, String toCreationTime, 
			String iriCreatingAgent, String fromSimulationTime, String toSimulationTime, String iriScenario, List<String> topics) {
		
		StringBuffer sparql = new StringBuffer();
		sparql.append(PrefixToUrlMap.getPrefixForSPARQL(DCTERMS));
		sparql.append(PrefixToUrlMap.getPrefixForSPARQL(XSD));
		sparql.append("PREFIX j1:<https://www.w3.org/2006/time#> \r\n");
		
		sparql.append("SELECT ?resource ?mediatype ?creationTime ?agent ?simulationTime ?scenario \r\n");
		sparql.append("WHERE { \r\n");
		
		sparql.append("OPTIONAL {?resource dcterms:format ?mediatype .}. \r\n");
		if (mediaType != null) {
			sparql.append("?resource dcterms:format \"" + mediaType.type + "\" . \r\n");
		}
		
		sparql.append("OPTIONAL {?resource dcterms:created ?creationTime .}. \r\n");
		if (fromCreationTime != null) {
			sparql.append("FILTER ( ?creationTime >= \"" + fromCreationTime + "\"^^xsd:dateTime ) \r\n");
		}
		if (toCreationTime != null) {
			sparql.append("FILTER ( ?creationTime <= \"" + toCreationTime + "\"^^xsd:dateTime ) \r\n");
		}
		
		sparql.append("OPTIONAL {?resource dcterms:creator ?agent .}. \r\n");
		if (iriCreatingAgent != null) {
			sparql.append("?resource dcterms:creator <" + iriCreatingAgent + "> . \r\n");
		}
		
		sparql.append("OPTIONAL {?resource j1:hasTime ?inst .}. \r\n");
		sparql.append("OPTIONAL {?inst j1:inXSDDateTime ?simulationTime .}. \r\n");
		if (fromSimulationTime != null) {
			sparql.append("FILTER ( ?simulationTime >= \"" + fromSimulationTime + "\"^^xsd:dateTime ) \r\n");
		}
		if (toSimulationTime != null) {
			sparql.append("FILTER ( ?simulationTime <= \"" + toSimulationTime + "\"^^xsd:dateTime ) \r\n");
		}
		sparql.append("FILTER ( regex(str(?resource), \"C://JPS_DATA/workingdir/JPS_SCENARIO\") ) \r\n");
		sparql.append("OPTIONAL {?resource dcterms:isPartOf ?scenario .}. \r\n");
		if (iriScenario != null) {
			sparql.append("?resource dcterms:isPartOf <" + iriScenario + "> . \r\n");
		}
		
		if (topics != null) {
			for (String current : topics) {
				sparql.append("?resource dcterms:subject <" + current + "> . \r\n");
			}
		}
		
		sparql.append("} ORDER BY DESC(?simulationTime) \r\n");
		sparql.append("LIMIT 1000");	
		
		return sparql.toString();
	}
	
	// TODO-AE SC URGENT 20190919 discuss with Kevin, delete is not required any more
	private static String getSparqlMetaDataResources(String timefrom, String timeto, String agentiri) {
			
		String query=null;
		String filterline="filter(?time >= \""+timefrom+"\"^^xsd:dateTime && ?time <= \""+timeto+"\"^^xsd:dateTime ).";
		//String agentiri = result.optString("agent","none");
		String optionalline="";
		String limitline="";
		if (agentiri != null){
			optionalline="?directory dcterms:creator <" + agentiri + "> . \r\n";
		}
		if(timefrom.contains("none")&&timeto.contains("none")) {
			
			query= "PREFIX j1:<https://www.w3.org/2006/time#>"
							+"PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>"
							+"PREFIX dcterms:<http://purl.org/dc/terms/>"
					 
						
					+ "SELECT ?directory ?time ?agent "
					+ "WHERE {?directory j1:hasTime ?inst ." 
					+ "?inst j1:inXSDDateTime ?time."
					+ "OPTIONAL {?directory dcterms:creator ?agent .}.\r\n"
					+optionalline
					+ "}"
					+"ORDER BY DESC(?time) "
					+"LIMIT 1";
		} else {
			if(timefrom.contains("none")){
				filterline="filter(?time <= \""+timeto+"\"^^xsd:dateTime ).";	
				limitline="LIMIT 1";
			}
			else if(timeto.contains("none")){
				filterline="filter(?time >= \""+timefrom+"\"^^xsd:dateTime ).";	
			}
		
			query=	"PREFIX j1:<https://www.w3.org/2006/time#> "
					+"PREFIX xsd:<http://www.w3.org/2001/XMLSchema#> "
					+"PREFIX dcterms:<http://purl.org/dc/terms/> "
				
			+ "SELECT ?directory ?time ?agent "
			+ "WHERE {?directory j1:hasTime ?inst ." 
			+ "?inst j1:inXSDDateTime ?time."
			+  filterline
			+ "OPTIONAL {?directory dcterms:creator ?agent .}.\r\n"
			+optionalline
			+ "}"
			+"ORDER BY DESC(?time) "
			+limitline;

		}	
		
		return query;
	}
}
