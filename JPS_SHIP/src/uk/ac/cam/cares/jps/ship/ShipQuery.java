package uk.ac.cam.cares.jps.ship;

import java.util.ArrayList;
import java.util.Arrays;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFactory;
import org.apache.jena.query.ResultSetRewindable;
import org.apache.jena.rdf.model.ModelFactory;

public class ShipQuery {
	
	public static ArrayList<ArrayList<String>> sparqlQueryRead() {
		ArrayList<ArrayList<String>> resultList = new ArrayList<ArrayList<String>>();		
		OntModel jenaOwlModel = null;
		jenaOwlModel = ModelFactory.createOntologyModel();
		//jenaOwlModel.read("C:\\JPS_DATA\\workingdir\\JPS\\SHIP\\output\\Ship-1.owl");
		jenaOwlModel.read("http://www.theworldavatar.com/kb/ships/Ship-1.owl#Ship-1");
		
		String sparqlQuery = "PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\r\n" + 
				"        PREFIX j4: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>\r\n" + 
				"\r\n" + 
				"        SELECT ?coordinateX_value ?coordinateY_value\r\n" + 
				"        WHERE { \r\n" + 
				"          " + "<http://www.theworldavatar.com/kb/ships/Ship-1.owl#Ship-1>"  + " j4:hasGISCoordinateSystem ?coordinateSystem .\r\n" + 
				"                ?coordinateSystem j4:hasProjectedCoordinate_x ?coordinateX .\r\n" + 
				"                    ?coordinateX j1:hasValue ?coordinateX_V .\r\n" + 
				"                        ?coordinateX_V j1:numericalValue ?coordinateX_value .\r\n" + 
				"                ?coordinateSystem j4:hasProjectedCoordinate_y ?coordinateY .\r\n" + 
				"                    ?coordinateY j1:hasValue ?coordinateY_V .\r\n" + 
				"                        ?coordinateY_V j1:numericalValue ?coordinateY_value .\r\n" + 
				"        }"; 
		
		System.out.println(sparqlQuery);
		
		Query query = QueryFactory.create(sparqlQuery);
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
	
	public static void main(String[] args) {
		
		System.out.println(Arrays.toString(sparqlQueryRead().toArray()));
		return;
	}
}
