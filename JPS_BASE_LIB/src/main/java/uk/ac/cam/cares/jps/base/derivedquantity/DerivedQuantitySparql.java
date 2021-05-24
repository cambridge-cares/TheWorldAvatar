package uk.ac.cam.cares.jps.base.derivedquantity;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

import uk.ac.cam.cares.jps.base.interfaces.KnowledgeBaseClientInterface;

/**
 * SPARQL queries/updates for instances related to derived quantities
 * @author Kok Foong Lee
 *
 */
public class DerivedQuantitySparql{
	// prefix/namespace
	private static Prefix p_agent = SparqlBuilder.prefix("agent",iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#"));
	private static Prefix p_derived = SparqlBuilder.prefix("derived",iri("http://www.theworldavatar.com/ontology/ontoderived/ontoderived.owl#"));
	private static Prefix p_time = SparqlBuilder.prefix("time", iri("http://www.w3.org/2006/time#"));
	
	// types
	private static Iri Service = p_agent.iri("Service");
    private static Iri TimePosition = p_time.iri("TimePosition");
	
	// relations
	private static Iri hasHttpUrl = p_agent.iri("hasHttpUrl");
	private static Iri isDerivedFrom = p_derived.iri("isDerivedFrom");
	private static Iri isDerivedUsing = p_derived.iri("isDerivedUsing");
	private static Iri hasTime = p_time.iri("hasTime");
	private static Iri numericPosition = p_time.iri("numericPosition");
	
	
	public static void initAgent(KnowledgeBaseClientInterface kbClient, String agentIRI, String url, String namedGraph) {
		ModifyQuery modify = Queries.MODIFY();
		
		TriplePattern agent_tp = iri(agentIRI).isA(Service).andHas(hasHttpUrl,url);
		
		modify.prefix(p_agent).insert(agent_tp).where();
		
		// optional named graph
		if (namedGraph != null) {
			modify.with(iri(namedGraph));
		}
		
		kbClient.setQuery(modify.getQueryString());
		kbClient.executeUpdate();
	}
	
	public static void initParameters(KnowledgeBaseClientInterface kbClient, String derivedQuantity, String[] inputs, String agentIRI, String namedGraph) {
		ModifyQuery modify = Queries.MODIFY();
		
		Iri derivedQuantity_iri = iri(derivedQuantity);
		
		// link to agent
		modify.insert(derivedQuantity_iri.has(isDerivedUsing,iri(agentIRI)));
		
		// add time stamp instance for derived quantity
		String derivedQuantityTime = derivedQuantity+"Time";
		if (!checkTimeExists(kbClient, derivedQuantityTime)) {
			Iri derivedQuantityTime_iri = iri(derivedQuantityTime);
		    modify.insert(derivedQuantity_iri.has(hasTime,derivedQuantityTime_iri));
		    modify.insert(derivedQuantityTime_iri.isA(TimePosition).andHas(numericPosition,0));
		}
		
		// link derived quantity to each input, and also add the time stamp instance
	    for (int i = 0; i < inputs.length; i++) {
	    	Iri input_iri = iri(inputs[i]);
			modify.insert(derivedQuantity_iri.has(isDerivedFrom, input_iri));
			
			String inputTime = inputs[i] + "Time";
			if (!checkTimeExists(kbClient, inputTime)) {
				Iri inputTime_iri = iri(inputTime);
				modify.insert(input_iri.has(hasTime, inputTime_iri));
				modify.insert(inputTime_iri.isA(TimePosition).andHas(numericPosition,0));
			}
		}
	    
	    // optional named graph if given
	    if (namedGraph != null) {
	    	modify.with(iri(namedGraph));
	    }
	    
	    kbClient.setQuery(modify.prefix(p_time,p_derived).getQueryString());
	    kbClient.executeUpdate();
	}
	
	/**
	 * ensure there are no time duplicates, maybe the time instance was already initialise before,, e.g. an input is also a derived quantity
	 * @param kbClient
	 * @param time_iri_string
	 * @return
	 */
	private static boolean checkTimeExists(KnowledgeBaseClientInterface kbClient, String time_iri) {
		// ask query is not supported by SparqlBuilder, hence hardcode
		String query = String.format("ask {<%s> a <http://www.w3.org/2006/time#TimePosition>}",time_iri);
		kbClient.setQuery(query);
		boolean timeExist = kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
		return timeExist;
	}
	
	public static String getAgentUrl(KnowledgeBaseClientInterface kbClient, String agentIRI, String namedGraph) {
		SelectQuery query = Queries.SELECT();
		
		String queryKey = "url";
		Variable url = SparqlBuilder.var(queryKey);
		
		GraphPattern queryPattern = iri(agentIRI).has(hasHttpUrl,url);
		
		query.select(url).where(queryPattern).prefix(p_agent);
		
		if (namedGraph != null) {
			query.from(SparqlBuilder.from(iri(namedGraph)));
		}
		
		kbClient.setQuery(query.getQueryString());
		
		String queryResult = kbClient.executeQuery().getJSONObject(0).getString(queryKey);
		
		return queryResult;
	}
}
