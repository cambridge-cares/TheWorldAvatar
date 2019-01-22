package uk.ac.cam.cares.jps.ship;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ParameterizedSparqlString;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.ModelFactory;

import com.google.gson.Gson;

public class ShipQuery {
	
	public static ArrayList<ArrayList<String>> sparqlQueryRead(String shipNumber) {
		ArrayList<ArrayList<String>> resultList = new ArrayList<ArrayList<String>>();		
		OntModel jenaOwlModel = null;
		jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read("C:\\JPS_DATA\\workingdir\\JPS\\SHIP\\output\\Ship-" + shipNumber + ".owl");
//		jenaOwlModel.read("http://www.theworldavatar.com/kb/ships/Ship-1.owl");
//		jenaOwlModel.read("http://172.25.182.41/kb/ships/Ship-" + shipNumber + ".owl#Ship-" + shipNumber);
		
		ParameterizedSparqlString queryString = new ParameterizedSparqlString(
				"PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n" + 
				"PREFIX j4: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
				"SELECT ?coordinateX_value ?coordinateY_value\r\n" + 
				"WHERE { \r\n" + 
				"	?shipIRI j4:hasGISCoordinateSystem ?coordinateSystem .\r\n" + 
			    "	   ?coordinateSystem j4:hasProjectedCoordinate_x ?coordinateX .\r\n" + 
			    "	      ?coordinateX j1:hasValue ?coordinateX_V .\r\n" + 
			    "	         ?coordinateX_V j1:numericalValue ?coordinateX_value .\r\n" + 
			    "      ?coordinateSystem j4:hasProjectedCoordinate_y ?coordinateY .\r\n" + 
			    "         ?coordinateY j1:hasValue ?coordinateY_V .\r\n" + 
			    "			 ?coordinateY_V j1:numericalValue ?coordinateY_value .\r\n" + 
				"}"
				);
		
		queryString.setIri("shipIRI", "http://www.theworldavatar.com/kb/ships/Ship-" + shipNumber + ".owl#Ship-" + shipNumber);
		Query query = queryString.asQuery();
		QueryExecution queryExec = QueryExecutionFactory.create(query, jenaOwlModel);
		ResultSet rs = queryExec.execSelect();   
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);
		
		while (results.hasNext()) {			
			QuerySolution qs_p = results.nextSolution();
			
			String coordinateX = qs_p.getLiteral("coordinateX_value").getLexicalForm();
			String coordinateY = qs_p.getLiteral("coordinateY_value").getLexicalForm();

			System.out.println(coordinateX);
			System.out.println(coordinateY);
			
			ArrayList<String> coordinates = new ArrayList<String>(Arrays.asList(coordinateX, coordinateY));
			resultList.add(coordinates);
		}
		queryExec.close();
		return resultList;
	}
	
	public static ArrayList<ArrayList<String>> sparqlQueryReadEndpoint(String shipIRI) {
		ArrayList<ArrayList<String>> resultList = new ArrayList<ArrayList<String>>();		
		
		ParameterizedSparqlString queryString = new ParameterizedSparqlString();
		
		queryString.setCommandText(
				"PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n" + 
				"PREFIX j4: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
				"SELECT ?coordinateX_value ?coordinateY_value\r\n" + 
				"WHERE { \r\n" + 
				"	?shipIRI j4:hasGISCoordinateSystem ?coordinateSystem .\r\n" + 
			    "	   ?coordinateSystem j4:hasProjectedCoordinate_x ?coordinateX .\r\n" + 
			    "	      ?coordinateX j1:hasValue ?coordinateX_V .\r\n" + 
			    "	         ?coordinateX_V j1:numericalValue ?coordinateX_value .\r\n" + 
			    "      ?coordinateSystem j4:hasProjectedCoordinate_y ?coordinateY .\r\n" + 
			    "         ?coordinateY j1:hasValue ?coordinateY_V .\r\n" + 
			    "			 ?coordinateY_V j1:numericalValue ?coordinateY_value .\r\n" + 
				"}"
				);
		
		queryString.setIri("shipIRI", shipIRI);
//		System.out.println(queryString.toString());
		
		QueryExecution queryExec = QueryExecutionFactory.sparqlService(
				"http://172.25.182.41/damecoolquestion/ships-persistent/sparql", 
				queryString.asQuery());
		ResultSet rs = queryExec.execSelect();   
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);
		
		while (results.hasNext()) {			
			QuerySolution qs_p = results.nextSolution();
			
			String coordinateX = qs_p.getLiteral("coordinateX_value").getLexicalForm();
			String coordinateY = qs_p.getLiteral("coordinateY_value").getLexicalForm();

			System.out.println(coordinateX);
			System.out.println(coordinateY);
			
			ArrayList<String> coordinates = new ArrayList<String>(Arrays.asList(coordinateX, coordinateY));
			resultList.add(coordinates);
		}
		queryExec.close();
		return resultList;
	}
	
	public static String sparqlQueryPython() throws IOException {
		String[] arrayOfShipIRIs = { 
				"http://www.theworldavatar.com/kb/ships/Ship-1.owl#Ship-1",
			};
			
			Gson g = new Gson();
			String result = "";
			
			String[] cmd = { "python", 
					"C:/Users/WE/Desktop/JPS/JParkSimulator-git/JPS_SHIP/python/caresjpsship/ShipGeoJSONGetter.py", 
					g.toJson(g.toJson(arrayOfShipIRIs)) };
			
			Process p = Runtime.getRuntime().exec(cmd);

			BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
			String returnValue = stdInput.readLine();
			
			return returnValue;		
	}
	
	public static void sparqlQueryShipAgent() {
		ArrayList<ArrayList<String>> resultList = new ArrayList<ArrayList<String>>();		
		OntModel jenaOwlModel = null;
		jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read("http://172.25.182.41/kb/ships/Ship-1.owl#Ship-1");
		
		ParameterizedSparqlString queryString = new ParameterizedSparqlString(
				"PREFIX cp:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#> " +
				"PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n" + 
				"SELECT ?engine\r\n" + 
				"WHERE {?entity  j1:hasSubsystem ?engine ." + 
				"MINUS" + 
				"{?engine a cp:Pipe .}" + 
				"}");
		
		queryString = new ParameterizedSparqlString("PREFIX cp:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#> " + 
				"PREFIX j1:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> " + 
				"SELECT ?chimney\r\n" + 
				"WHERE " + 
				"{ ?entity  j1:hasSubsystem ?chimney ." + 
				"  ?chimney a cp:Pipe ." + 
				"}");
		
		Query query = queryString.asQuery();
		QueryExecution queryExec = QueryExecutionFactory.create(query, jenaOwlModel);
		ResultSet rs = queryExec.execSelect();   
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);
		
		while (results.hasNext()) {			
			QuerySolution qs_p = results.nextSolution();
			
			String coordinateX = qs_p.getResource("chimney").toString();

			System.out.println(coordinateX);
			
			ArrayList<String> coordinates = new ArrayList<String>(Arrays.asList(coordinateX));
			resultList.add(coordinates);
		}
		queryExec.close();
		return;
	}
	
	public static void main(String[] args) throws IOException {
		
//		System.out.println(Arrays.toString(sparqlQueryRead().toArray()));
		long startTime = System.currentTimeMillis();
		sparqlQueryReadEndpoint("http://www.theworldavatar.com/kb/ships/Ship-1.owl#Ship-1");
//		sparqlQueryRead("1");	
//		System.out.println(sparqlQueryPython());
//		sparqlQueryShipAgent();
		long endTime = System.currentTimeMillis();
		System.out.println((endTime-startTime));
		
		
		startTime = System.currentTimeMillis();
		sparqlQueryReadEndpoint("http://www.theworldavatar.com/kb/ships/Ship-2.owl#Ship-2");
//		sparqlQueryRead("2");
//		sparqlQueryPython();
		endTime = System.currentTimeMillis();
		System.out.println((endTime-startTime));

		
		startTime = System.currentTimeMillis();
		sparqlQueryReadEndpoint("http://www.theworldavatar.com/kb/ships/Ship-3.owl#Ship-3");
//		sparqlQueryRead("3");
//		sparqlQueryPython();
		endTime = System.currentTimeMillis();
		System.out.println((endTime-startTime));

		return;
	}
}
