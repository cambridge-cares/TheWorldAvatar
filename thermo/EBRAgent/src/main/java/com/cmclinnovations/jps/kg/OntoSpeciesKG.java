package com.cmclinnovations.jps.kg;

import java.util.LinkedList;
import java.util.List;
import org.apache.log4j.Logger;
import org.eclipse.rdf4j.IsolationLevels;
import org.eclipse.rdf4j.federated.FedXFactory;

import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.query.TupleQueryResultHandlerException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import com.cmclinnovations.jps.agent.ebr.EBRAgent;
import com.cmclinnovations.jps.model.species.SpeciesBean;

/**
 * This class manages the download of species from the OntoSpecies repository</br>
 * available on any RDF4J triple store.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class OntoSpeciesKG extends EBRAgent{
	Logger logger = Logger.getLogger(OntoSpeciesKG.class);	
	public static final String RDF = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n";
	public static final String RDFS = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n";
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @author msff2 (msff2@cam.ac.uk)
	 * 
	 * @param localHostSparqlEndPoint the localhost sparql endpoint.
	 * @param claudiusServerSparqlEndPoint the remote repository sparql.
	 * @param query the query string.
	 * @return the set of sparql results.
	 * @throws Exception 
	 */
	
	public LinkedList<SpeciesBean> querySpecies(List<String> endpoints, String query, boolean isTargetSpecies) throws Exception {
		LinkedList<SpeciesBean> nistSpeciesIdList = new LinkedList<SpeciesBean>();
		Repository repository = FedXFactory.createSparqlFederation(endpoints);
		repository.init();		
		String eof="";
		try {
			RepositoryConnection conn = repository.getConnection();
			TupleQuery tq = conn.prepareTupleQuery(QueryLanguage.SPARQL, query);
			try {
				TupleQueryResult tqRes = tq.evaluate();
				while (tqRes.hasNext()) {
					BindingSet bSet = tqRes.next();
					if(isTargetSpecies){
						eof="?";
					}else{
						eof = bSet.getValue("enthalpyOfFormationValue").stringValue();
					}
					String ontoSpeciesIRI = bSet.getValue("ontoSpeciesIRI").stringValue();
					if(ontoSpeciesIRI!=null && ontoSpeciesIRI.contains("#")){
						ontoSpeciesIRI = ontoSpeciesIRI.substring(ontoSpeciesIRI.lastIndexOf("#")+1);
					}
					SpeciesBean nistSpeciesId = new SpeciesBean(ontoSpeciesIRI,
							bSet.getValue("atomicBond").stringValue(), bSet.getValue("geometry").stringValue(),
							eof,
							bSet.getValue("scfEnergyValue").stringValue(),
							bSet.getValue("zeroEnergyValue").stringValue());
					System.out.println("crsid:"+bSet.getValue("crid"));
					nistSpeciesIdList.add(nistSpeciesId);
				}
			} catch (TupleQueryResultHandlerException e) {
				e.printStackTrace();
			}
			conn.close();
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
		repository.shutDown();
		return nistSpeciesIdList;
	}	
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param ontoSpeciesIRI onto species iri given is jos input file
	 * @param ontoCompChemIRI ontocompchem iri given in json input file
	 * @return sparql query string.
	 * 
	 */
	public String getSpeciesQueryFromJsonInput(String ontoSpeciesIRI, String ontoCompChemIRI){
		
			String query ="PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> "
					+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> "
					+ "PREFIX gc: <http://purl.org/gc/> "
					+ "SELECT DISTINCT ?crid ?atomicBond ?geometry ?enthalpyOfFormationValue  ?scfEnergyValue ?zeroEnergyValue ?ontoSpeciesIRI "
					+ "WHERE { "
					+ "<"+ontoSpeciesIRI+"> OntoSpecies:casRegistryID ?crid . "
					+ "?ontoSpeciesIRI OntoSpecies:casRegistryID ?crid . "
					+ "<"+ontoSpeciesIRI+"> OntoSpecies:hasAtomicBond ?atomicBond . "
					+ "<"+ontoSpeciesIRI+"> OntoSpecies:hasGeometry ?geometry . "
					+ "<"+ontoSpeciesIRI+"> OntoSpecies:hasStandardEnthalpyOfFormation ?enthalpy . "
					+ "?enthalpy OntoSpecies:value ?enthalpyOfFormationValue ."
					+ "<"+ontoCompChemIRI+"> ontocompchem:hasUniqueSpecies <"+ontoSpeciesIRI+"> . "
					+ "<"+ontoCompChemIRI+"> gc:isCalculationOn ?scfEnergy . "
					+ "?scfEnergy a ontocompchem:ScfEnergy . "
					+ "?scfEnergy gc:hasElectronicEnergy ?scfElectronicEnergy . "
					+ "?scfElectronicEnergy gc:hasValue ?scfEnergyValue . "
					+ "<"+ontoCompChemIRI+"> gc:isCalculationOn ?zeroEnergy . "
					+ "?zeroEnergy a ontocompchem:ZeroPointEnergy . "
					+ "?zeroEnergy gc:hasElectronicEnergy ?zeroElectronicEnergy . "
					+ "?zeroElectronicEnergy gc:hasValue ?zeroEnergyValue . "
					+ "}";
			
			return query;
	}
	
	
	public static String getGaussianFileIRI(String ontoCompChemIRI) {
		
		
		String query="PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> "
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> "
				+ "PREFIX gc: <http://purl.org/gc/> "
				+ "SELECT DISTINCT ?outputFile "
				+ "WHERE { "
				+ "<"+ontoCompChemIRI+"> ontocompchem:hasEnvironment ?environment . "
				+ "?environment ontocompchem:hasProgram ?program . "
				+ "?environment gc:hasOutputFile ?outputFile . "
				+ "FILTER(REGEX(str(?outputFile),'.g09') || REGEX(str(?outputFile),'.log') || REGEX(str(?outputFile),'.g16')) ."
				+ "FILTER(str(?program)='Gaussian') ."
				+ "}";
		
		return query;
	}
	
	/**
	 * 
	 * @param serverUrl the server url
	 * @param ontoCompChemIRI the ontocompchem species IRIs
	 * @return the list of 
	 */
	public LinkedList<String> queryOntoCompChemSpeciesRepository(String serverUrl, String ontoCompChemIRI) {

		LinkedList<String> outputGaussianFileIRIList = new LinkedList<String>();
		
		String queryString = getGaussianFileIRI(ontoCompChemIRI);
		
		System.out.println("queryString: " + queryString);
		
		Repository repository = new HTTPRepository(serverUrl);

		repository.init();

		RepositoryConnection connection = repository.getConnection();

		System.out.println("connection.isActive(): " + connection.isActive());
		
		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				System.out.println("result: " + result);
				
				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					String gaussianFile = bindingSet.getValue("outputFile").stringValue();
					
					System.out.println("gaussian File: " + gaussianFile);
					
					outputGaussianFileIRIList.add(gaussianFile);

				}

				connection.commit();

			} catch (Exception e) {

				System.out.println(e.getMessage());

			} finally {

				result.close();
			}

		} catch (RepositoryException e) {

			System.out.println(e.getMessage());

			connection.rollback();

		} finally {

			connection.close();

			repository.shutDown();
		}
		
		return outputGaussianFileIRIList;
	}
}
