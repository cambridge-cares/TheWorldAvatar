package uk.ac.cam.cares.ebr;


import org.eclipse.rdf4j.federated.FedXFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;

import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;




/**
 * 
 * @author NK510
 * all methods below are borrowed from https://rdf4j.org/documentation/programming/federation/#core-features
 *
 */
public class FederatedQuery {
	
	public static void runFederatedSPARQLOnDbpediaWikipedia() throws Exception {
		
      Repository repository = FedXFactory.newFederation()
		.withSparqlEndpoint("http://dbpedia.org/sparql")
//		.withSparqlEndpoint("https://query.wikidata.org/sparql")
        .withSparqlEndpoint("http://theworldavatar.com/rdf4j-server/repositories/ontokin")        
		.create();
			
	try (RepositoryConnection conn = repository.getConnection()) {

//		String query = 
//			"PREFIX wd: <http://www.wikidata.org/entity/> "
//			+ "PREFIX wdt: <http://www.wikidata.org/prop/direct/> "
//			+ "SELECT * WHERE { "
//			+ " ?country a <http://dbpedia.org/class/yago/WikicatMemberStatesOfTheEuropeanUnion> ."
//			+ " ?country <http://www.w3.org/2002/07/owl#sameAs> ?countrySameAs . "
//			+ " ?countrySameAs wdt:P2131 ?gdp ."
//			+ "}";

		String query = 
				"SELECT ?s ?o WHERE { "
				+ "?s <http://www.theworldavatar.com/kb/ontokin/ontokin.owl#hasElementNumber>  ?o  . "
				+ "} LIMIT 10";
				
		
		TupleQuery tq = conn.prepareTupleQuery(query);
		try (TupleQueryResult tqRes = tq.evaluate()) {


			int count = 0;
			while (tqRes.hasNext()) {
				BindingSet b = tqRes.next();
				System.out.println(b);
				count++;
			}

			System.out.println("Results: " + count);
		}
	}
			
	repository.shutDown();
		
		
	}
}
