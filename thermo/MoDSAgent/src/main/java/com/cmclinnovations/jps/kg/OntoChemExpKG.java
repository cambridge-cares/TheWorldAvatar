package com.cmclinnovations.jps.kg;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;

import org.apache.log4j.Logger;
import org.eclipse.rdf4j.RDF4JException;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

import com.cmclinnovations.jps.agent.mechanism.calibration.MoDSAgentException;
import com.cmclinnovations.jps.agent.mechanism.calibration.Property;

public class OntoChemExpKG extends RepositoryManager {
	Logger logger = Logger.getLogger(OntoChemExpKG.class);
	public static final String RDF = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n";
	public static final String RDFS = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n";
	
	
	public static void main(String[] args) throws ServletException, MoDSAgentException {
		OntoChemExpKG ontoChemExpKG = new OntoChemExpKG();
		
//		System.out.println("test case");
//		System.out.println(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName());
//		System.out.println(Property.RDF4J_ONTOCHEMEXP_REPOSITORY_ID.getPropertyName());
		
//		ontoChemExpKG.queryConcentration("https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_684814261441600");
//		ontoChemExpKG.queryEquivalenceRatio("https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_684814261441600");
//		ontoChemExpKG.queryExperimentData("https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_684814261441600");
		ontoChemExpKG.formatExperimentDataTable("https://como.ceb.cam.ac.uk/kb/ontochemexp/x00001700.owl#Experiment_684814261441600");
	}
	
	public final class DataTable {
	    private final List<String> header;
	    private final List<List<String>> data;

	    public DataTable(List<String> header, List<List<String>> data) {
	        this.header = header;
	        this.data = data;
	    }

	    public List<String> getTableHeader() {
	        return header;
	    }

	    public List<List<String>> getTableData() {
	        return data;
	    }
	}
	
	public DataTable formatExperimentDataTable(String experimentIRI) throws MoDSAgentException {
		List<String> columnTitles = new ArrayList<String>();
		List<List<String>> experimentData = new ArrayList<List<String>>();
		
		List<List<String>> initialConc = queryConcentration(experimentIRI);
		List<List<String>> phi = queryEquivalenceRatio(experimentIRI);
		List<List<String>> expCond = queryExperimentData(experimentIRI);
		
		if (initialConc.get(0).get(0).equalsIgnoreCase("molecule")) {
			for (int i = 1; i < initialConc.size(); i++) {
				columnTitles.add(initialConc.get(i).get(0));
			}
		} else {
			logger.error("The query results should be molecule concentrations.");
		}
		columnTitles.add(phi.get(0).get(0));
		columnTitles.addAll(expCond.get(0));
		
		for (int i = 1; i < expCond.size(); i++) {
			List<String> dataLine = new ArrayList<>();
			// add molecular concentrations to dataLine
			dataLine.add(initialConc.get(1).get(1));
			dataLine.add(initialConc.get(2).get(1));
			dataLine.add(initialConc.get(3).get(1));
			// add equivalence ratio to dataLine
			dataLine.add(phi.get(1).get(0));
			// add temperature, pressure, and ignition delay times to dataLine
			dataLine.addAll(expCond.get(i));
			experimentData.add(dataLine);
		}
		
//		System.out.println(experimentData);
////		
//		System.out.println(experimentData.size());
//		for(int i = 0; i < n; i++) {
//		    out[i] = in.get(i);
//		}
//		
//		for (List<String> line : initialConc.subList(1, toIndex)) {
//			while (line.get(0) != "molecule") {
//				System.out.println(line.get(0));
//				columnTitles.add(line.get(1));
//			}
//		}
//		
//		System.out.println(columnTitles);
		
		
		return new DataTable(columnTitles, experimentData);
	}
	
	public List<List<String>> queryConcentration(String experimentIRI) throws MoDSAgentException {
//		String molecule = null;
		
		if(!experimentIRI.trim().startsWith("<") && !experimentIRI.trim().endsWith(">")){
			experimentIRI = "<".concat(experimentIRI).concat(">");
		}
		String queryString = formConcentrationQuery(Property.PREFIX_BINDING_ONTOCHEMEXP.getPropertyName(), experimentIRI);
		System.out.println(queryString);
		List<List<String>> testResults = queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), 
				Property.RDF4J_ONTOCHEMEXP_REPOSITORY_ID.getPropertyName(), queryString);
		System.out.println(testResults);
		return testResults;
	}
	
	public List<List<String>> queryEquivalenceRatio(String experimentIRI) throws MoDSAgentException {
		if(!experimentIRI.trim().startsWith("<") && !experimentIRI.trim().endsWith(">")){
			experimentIRI = "<".concat(experimentIRI).concat(">");
		}
		String queryString = formEquivalenceRatioQuery(Property.PREFIX_BINDING_ONTOCHEMEXP.getPropertyName(), experimentIRI);
		System.out.println(queryString);
		List<List<String>> testResults = queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), 
				Property.RDF4J_ONTOCHEMEXP_REPOSITORY_ID.getPropertyName(), queryString);
		System.out.println(testResults);
		return testResults;
	}
	
	public List<List<String>> queryExperimentData(String experimentIRI) throws MoDSAgentException {
		if(!experimentIRI.trim().startsWith("<") && !experimentIRI.trim().endsWith(">")){
			experimentIRI = "<".concat(experimentIRI).concat(">");
		}
		String queryString = formExperimentDataQuery(Property.PREFIX_BINDING_ONTOCHEMEXP.getPropertyName(), experimentIRI);
		System.out.println(queryString);
		List<List<String>> testResults = queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), 
				Property.RDF4J_ONTOCHEMEXP_REPOSITORY_ID.getPropertyName(), queryString);
		System.out.println(testResults);
		return testResults;
	}
	
	private String formConcentrationQuery(String prefixBindingOntoChemExp, String experimentIRI) {
		String queryString = prefixBindingOntoChemExp;
		queryString = queryString.concat("SELECT ?molecule ?composition \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(experimentIRI).concat(" OntoChemExp:hasCommonProperties ?commonProperties . \n");
		queryString = queryString.concat("    ?commonProperties OntoChemExp:hasProperty ?property . \n");
		queryString = queryString.concat("    ?property OntoChemExp:hasComponent ?component . \n");
		queryString = queryString.concat("    ?component OntoChemExp:hasSpeciesLink ?speciesLink . \n");
		queryString = queryString.concat("    ?speciesLink OntoChemExp:hasDatPreferredKey ?molecule . \n");
		queryString = queryString.concat("    ?component OntoChemExp:hasAmount ?amount . \n");
		queryString = queryString.concat("    ?amount OntoChemExp:hasVal ?composition \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	private String formEquivalenceRatioQuery(String prefixBindingOntoChemExp, String experimentIRI) {
		String queryString = prefixBindingOntoChemExp;
		queryString = queryString.concat("SELECT ?Phi \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(experimentIRI).concat(" OntoChemExp:hasCommonProperties ?commonProperties . \n");
		queryString = queryString.concat("    ?commonProperties OntoChemExp:hasProperty ?property . \n");
		queryString = queryString.concat("    ?property OntoChemExp:hasName ?name . \n");
		queryString = queryString.concat("    ?property OntoChemExp:hasValue ?value . \n");
		queryString = queryString.concat("    ?value OntoChemExp:hasVal ?Phi  \n");
		queryString = queryString.concat("    FILTER regex(str(?name), \"equivalence ratio\", \"i\") \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
	private String formExperimentDataQuery(String prefixBindingOntoChemExp, String experimentIRI) {
		String queryString = prefixBindingOntoChemExp;
		queryString = queryString.concat(RDF);
		queryString = queryString.concat("SELECT ?Temperature ?UnitTemp ?Pressure ?UnitPres ?IgnitionDelay ?UnitIgni \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(experimentIRI).concat(" OntoChemExp:hasDataGroup ?dataGroup . \n");
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasDataPoint ?dataPoint . \n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasID ?dataPointID . \n");
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasProperty ?propertyTemp . \n");
		queryString = queryString.concat("    ?propertyTemp OntoChemExp:hasName ?nameTemp . \n");
		queryString = queryString.concat("    ?propertyTemp OntoChemExp:hasID ?idTemp . \n");
		queryString = queryString.concat("    ?propertyTemp OntoChemExp:hasUnits ?UnitTemp . \n");
		queryString = queryString.concat("    FILTER regex(str(?nameTemp), \"temperature\", \"i\") \n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasDataPointX ?dataPointTemp . \n");
		queryString = queryString.concat("    ?dataPointTemp rdf:type ?typeTemp . \n");
		queryString = queryString.concat("    FILTER regex(str(?typeTemp), str(?idTemp), \"i\") \n");
		queryString = queryString.concat("    ?dataPointTemp OntoChemExp:hasVal ?Temperature . \n");
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasProperty ?propertyPres . \n");
		queryString = queryString.concat("    ?propertyPres OntoChemExp:hasName ?namePres . \n");
		queryString = queryString.concat("    ?propertyPres OntoChemExp:hasID ?idPres . \n");
		queryString = queryString.concat("    ?propertyPres OntoChemExp:hasUnits ?UnitPres . \n");
		queryString = queryString.concat("    FILTER regex(str(?namePres), \"pressure\", \"i\") \n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasDataPointX ?dataPointPres . \n");
		queryString = queryString.concat("    ?dataPointPres rdf:type ?typePres . \n");
		queryString = queryString.concat("    FILTER regex(str(?typePres), str(?idPres), \"i\") \n");
		queryString = queryString.concat("    ?dataPointPres OntoChemExp:hasVal ?Pressure . \n");
		queryString = queryString.concat("    ?dataGroup OntoChemExp:hasProperty ?propertyIgni . \n");
		queryString = queryString.concat("    ?propertyIgni OntoChemExp:hasName ?nameIgni . \n");
		queryString = queryString.concat("    ?propertyIgni OntoChemExp:hasID ?idIgni . \n");
		queryString = queryString.concat("    ?propertyIgni OntoChemExp:hasUnits ?UnitIgni . \n");
		queryString = queryString.concat("    FILTER regex(str(?nameIgni), \"ignition delay\", \"i\") \n");
		queryString = queryString.concat("    ?dataPoint OntoChemExp:hasDataPointX ?dataPointIgni . \n");
		queryString = queryString.concat("    ?dataPointIgni rdf:type ?typeIgni . \n");
		queryString = queryString.concat("    FILTER regex(str(?typeIgni), str(?idIgni), \"i\") \n");
		queryString = queryString.concat("    ?dataPointIgni OntoChemExp:hasVal ?IgnitionDelay . \n");
		queryString = queryString.concat("}");
		return queryString;
	}
	
//	public List<List<String>> queryRepository(String serverURL, String repositoryID, String queryString) throws MoDSAgentException {
//		List<List<String>> processedResultList = new ArrayList<List<String>>();
//		
//		try {
//			Repository repo = new HTTPRepository(serverURL, repositoryID);
//			repo.initialize();
//			RepositoryConnection con = repo.getConnection();
//			
//			try {
//				System.out.println("QueryString:\n"+queryString);
//				TupleQuery queryResult = con.prepareTupleQuery(queryString);
////				System.out.println(queryResult);
////				System.out.println("test again");
//				try (TupleQueryResult result = queryResult.evaluate()) {
//				processResult(result, processedResultList);
//				} finally {
//					repo.shutDown();
//				}
//			} catch (Exception e) {
//				logger.error("Exception occurred.");
//				e.printStackTrace();
//				throw new MoDSAgentException("Exception occurred.");
//			} finally {
//				logger.info("Executed the command to close the connection to the repository.");
//				con.close();
//			}
//		} catch (RDF4JException e) {
//			logger.error("RDF4JException occurred.");
//			e.printStackTrace();
//			throw new MoDSAgentException("RDF4JException occurred.");
//		}
//		return processedResultList;
//	}
//	
	
//	/**
//	 * Removes the OWL API dependent extra characters from the result.
//	 * 
//	 * @param result
//	 * @param processedResult
//	 */
//	private void processResult(TupleQueryResult result, List<List<String>> processedResultList) {
//		List<String> columnTitles = new ArrayList<>();
//		for (String bindingName : result.getBindingNames()) {
//			columnTitles.add(bindingName);
//		}
//		processedResultList.add(columnTitles);
//		// we just iterate over all solutions in the result...
//		while (result.hasNext()) {
//			BindingSet solution = result.next();
////			System.out.println(solution);
//			
//			List<String> processedResult = new ArrayList<>();
//			for (String bindingName : solution.getBindingNames()) {
////				System.out.println(bindingName);
////				System.out.println(solution.getValue(bindingName));
////				System.out.println(solution.getValue(bindingName).toString());
//				processedResult.add(removeDataType(solution.getValue(bindingName).toString()));
//			}
//			processedResultList.add(processedResult);
//		}
//	}
//	
	
//	/**
//	 * Removes the following XML Schema data types from a string:</br>
//	 * 1. string</br>
//	 * 2. integer</br>
//	 * 3. float</br>
//	 * 4. double.
//	 * 
//	 * @param value
//	 * @return
//	 */
//	private String removeDataType(String value){
//		String stringType = "^^<http://www.w3.org/2001/XMLSchema#string>";
//		String integerType = "^^<http://www.w3.org/2001/XMLSchema#integer>";
//		String floatType = "^^<http://www.w3.org/2001/XMLSchema#float>";
//		String doubleType = "^^<http://www.w3.org/2001/XMLSchema#double>";
//		if(value.contains(stringType)){
//			value = value.replace(stringType, "");
//		} else if(value.contains(integerType)){
//			value = value.replace(integerType, "");
//			value = replaceInvertedComma(value);
//		} else if(value.contains(floatType)){
//			value = value.replace(floatType, "");
//			value = replaceInvertedComma(value);
//		} else if(value.contains(doubleType)){
//			value = value.replace(doubleType, "");
//			value = replaceInvertedComma(value);
//		} else if(value.startsWith("\"") || value.endsWith("\"")){
//			value = value.replace("\"", "");
//		}
//		return value;
//	}
//	
//	/**
//	 * Removes inverted commas from a string.
//	 * 
//	 * @param value
//	 * @return
//	 */
//	private String replaceInvertedComma(String value){
//		if(value.contains("\"")){
//			value = value.replace("\"", "");
//		}
//		return value;
//	}
}
