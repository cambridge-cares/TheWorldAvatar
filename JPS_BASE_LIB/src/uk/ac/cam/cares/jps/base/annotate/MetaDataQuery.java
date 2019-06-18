package uk.ac.cam.cares.jps.base.annotate;

import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService;
import uk.ac.cam.cares.jps.base.query.sparql.PrefixToUrlMap;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;

public class MetaDataQuery implements Prefixes {

	public static String queryResources(String fromTime, String toTime, String iriAgent) {
		String sparql = getSparqlQueryResources(fromTime, toTime, iriAgent);
		System.out.println(sparql);
		SparqlOverHttpService sparqlService =  MetaDataAnnotator.getSparqlService();
		return sparqlService.executeGet(sparql);
	}
	
	public static String getSparqlQueryResources(String fromTime, String toTime, String iriAgent) {
		
		StringBuffer sparql = new StringBuffer("PREFIX dcterms:<" + PrefixToUrlMap.getPrefixUrl(DCTERMS) + "> \r\n");
		sparql.append("PREFIX time:<" + PrefixToUrlMap.getPrefixUrl(TIME) + "> \r\n");
		sparql.append("PREFIX xsd:<" + PrefixToUrlMap.getPrefixUrl(XSD) + "> \r\n");
		sparql.append("SELECT ?resource ?time ?agent\r\n");
		sparql.append("WHERE { \r\n");
		sparql.append("?resource time:hasTime/time:inXSDDateTime ?time . \r\n");
		if (fromTime != null) {
			sparql.append("FILTER ( ?time >= \"" + fromTime + "\"^^xsd:dateTime ) \r\n");
		}
		if (toTime != null) {
			sparql.append("FILTER ( ?time <= \"" + toTime + "\"^^xsd:dateTime ) \r\n");
		}
		sparql.append("OPTIONAL {?resource dcterms:creator ?agent .}. \r\n");
		if (iriAgent != null) {
			sparql.append("?resource dcterms:creator <" + iriAgent + "> . \r\n");
		} 
			
		sparql.append("} \r\n");
		sparql.append("LIMIT 1000");	
		
		return sparql.toString();
	}
}
