package com.cmclinnovations.jps.kg;

import java.io.IOException;
import java.net.URLEncoder;

import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.Model;
import org.eclipse.rdf4j.model.Statement;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.query.QueryResults;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryResult;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

import com.cmclinnovations.jps.agent.configuration.DFTAgentProperty;
import com.cmclinnovations.jps.agent.http.Request;

/**
 * This class queries an OntoKin knowledge graph and updates it with new thermodata.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class OntoKinKG {

	/**
	 * Reads the ThermoModels and CoeffValues of a species. 
	 * 
	 * @return
	 * @throws IOException
	 */
	public static String getThermoModelsOfSpecies(String speciesIRI, DFTAgentProperty dftAgentProperty) throws IOException{
		String query = formThermoModelQuery(speciesIRI);
		System.out.println("Query:"+query);
		String httpURL = dftAgentProperty.getOntoKinRepositoryIRI().concat(URLEncoder.encode(query, "UTF-8").concat("&Accept=application/sparql-results%2Bjson"));
		for(int i=0;i<5;i++){
		try{
			return Request.get(httpURL);
		}catch(IOException e){
			System.out.println("Attemp ["+i+"] to retrieve a thermodel of the species.");
		}
		}
		return null;
	}
	
	/**
	 * Deletes a thermo model from the species. 
	 * 
	 * @return
	 * @throws IOException
	 */
	public static void deleteThermoModelsOfSpecies(String triple, DFTAgentProperty dftAgentProperty) throws IOException{
		String tokens[] = triple.split(" ");
		if(tokens.length>=2){
			removeThermoModel(dftAgentProperty.getRdf4jServerURL(), dftAgentProperty.getOntoKinRepositoryID(), tokens[0], tokens[1], null);
		}
	}

	/**
	 * Deletes an object as part of the update operation.
	 * 
	 * @param rdf4jServerURL
	 * @param rdf4jRepositoryID
	 * @param subject
	 * @param predicate
	 * @param object
	 */
	private static void removeThermoModel(String rdf4jServerURL, String rdf4jRepositoryID, String subject, String predicate, String object){
		Repository repo = new HTTPRepository(rdf4jServerURL, rdf4jRepositoryID);
		repo.initialize();
		RepositoryConnection con = repo.getConnection();
		ValueFactory f = repo.getValueFactory();
		IRI thermoModelLowTemp = f.createIRI(subject);
		IRI coeffValue = f.createIRI(predicate);
		RepositoryResult<Statement> statements = con.getStatements(thermoModelLowTemp, coeffValue, null);
		Model aboutThermoModelLowTemp = QueryResults.asModel(statements);
		con.remove(aboutThermoModelLowTemp);
	}
	
	/**
	 * Inserts a thermo model into the species.
	 * 
	 * @param triple
	 * @param dftAgentProperty
	 * @throws IOException
	 */
	public static void insertThermoModelsOfSpecies(String triple, DFTAgentProperty dftAgentProperty) throws IOException{
		String tokens[] = triple.split(" ");
		if(tokens.length>=2){
			triple = triple.replaceFirst(tokens[0].concat(" "), "");
			triple = triple.replaceFirst(tokens[1].concat(" "), "");
			addThermoModel(dftAgentProperty.getRdf4jServerURL(), dftAgentProperty.getOntoKinRepositoryID(), tokens[0], tokens[1], triple.replaceAll(tokens[0], triple));
		}
	}
	
	/**
	 * Inserts a thermo model as part of the update operation.
	 * 
	 * @param rdf4jServerURL
	 * @param rdf4jRepositoryID
	 * @param subject
	 * @param predicate
	 * @param object
	 */
	private static void addThermoModel(String rdf4jServerURL, String rdf4jRepositoryID, String subject, String predicate, String object){
		Repository repo = new HTTPRepository(rdf4jServerURL, rdf4jRepositoryID);
		repo.initialize();
		RepositoryConnection con = repo.getConnection();
		ValueFactory f = repo.getValueFactory();
		IRI thermoModelLowTemp = f.createIRI(subject);
		IRI coeffValue = f.createIRI(predicate);
		Literal coeffValuesLiteral = f.createLiteral(object);
		con.add(thermoModelLowTemp, coeffValue, coeffValuesLiteral);
	}
	
	/**
	 * Builds a query to insert a thermo model into the species.
	 * 
	 * @param triple
	 * @return
	 */
	public static String formThermoModelInsertionQuery(String triple){
		String queryString = "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>\n";
		queryString = queryString.concat("INSERT DATA\n");
		queryString = queryString.concat("{"+triple+"}");
		return queryString;
	}
	
	/**
	 * Builds a query to delete a thermo model of the species.
	 * 
	 * @param triple
	 * @return
	 */
	public static String formThermoModelDeletionQuery(String triple){
		String queryString = "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>\n";
		queryString = queryString.concat("DELETE DATA\n");
		queryString = queryString.concat("{"+triple+"}");
		return queryString;
	}
	
	/**
	 * Builds a query to retrieve the number of cores needed to run a quantum job.</br>
	 * 
	 * @param speciesIRI
	 * @return
	 */
	public static String formThermoModelQuery(String speciesIRI){
		String queryString = "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>\n";
		queryString = queryString.concat("SELECT ?thermoModel ?coeffValues\n");
		queryString = queryString.concat("WHERE {\n");
		queryString = queryString.concat("?speciesIRI ontokin:hasUniqueSpecies <"+speciesIRI+"> .\n");
		queryString = queryString.concat("?speciesIRI ontokin:hasThermoModel ?thermoModel .\n");
		queryString = queryString.concat("?thermoModel ontokin:hasCoefficientValues ?coeffValues .\n");
		queryString = queryString.concat("      }");		
		return queryString;
	}
}