package uk.ac.cam.cares.jps.ship;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

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
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONStringer;

/**
 * Servlet implementation class ShipGeoJSON
 * for visualization part which is called by the 3DADMSMap.js
 */
@WebServlet("/ShipGeoJSON")
public class ShipGeoJSON extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    public ShipGeoJSON() {
        super();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		// -- Get String formatted in Array of Strings -- //
		request.setCharacterEncoding("UTF-8");
		String listOfIRIs = request.getParameter("listOfIRIs");
		JSONArray arrayOfIRIs = null;
		JSONStringer arrayOfShipGeoJSON = new JSONStringer();
		
		try {
			arrayOfShipGeoJSON.array();
			arrayOfIRIs = new JSONArray(listOfIRIs);
			for (int i = 0; i < arrayOfIRIs.length(); i++) {
				String shipIRI = arrayOfIRIs.getString(i);
				JSONStringer shipGeoJSON = sparqlQueryReadEndpoint(shipIRI);
				arrayOfShipGeoJSON.value(shipGeoJSON);
			}
			arrayOfShipGeoJSON.endArray();
			
		} catch (JSONException e) {
			e.printStackTrace();
		}
		
		response.setContentType("application/json");
		response.getWriter().write(arrayOfShipGeoJSON.toString());	
	}
	
	public static JSONStringer sparqlQueryReadEndpoint(String shipIRI) throws JSONException {
		
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
		
//		QueryExecution queryExec = QueryExecutionFactory.sparqlService(
//				"http://172.25.182.41/damecoolquestion/ships-persistent/sparql", 
//				queryString.asQuery());

		
		OntModel jenaOwlModel = ModelFactory.createOntologyModel();	
		jenaOwlModel.read(shipIRI);
		
		Query query = QueryFactory.create(queryString.toString());
		QueryExecution queryExec = QueryExecutionFactory.create(query, jenaOwlModel);
		
		
		ResultSet rs = queryExec.execSelect();   
		//reset the cursor, so that the ResultSet can be repeatedly used
		ResultSetRewindable results = ResultSetFactory.copyResults(rs);    
		//ResultSetFormatter.out(System.out, results, query); ?? don't know why this is needed to be commented
		

		
		
		JSONStringer shipGeoJSON = new JSONStringer();
		
		while (results.hasNext()) {			
			QuerySolution qs_p = results.nextSolution();
			
			double coordinateX = Double.parseDouble(qs_p.getLiteral("coordinateX_value").getLexicalForm());
			double coordinateY = Double.parseDouble(qs_p.getLiteral("coordinateY_value").getLexicalForm());			
			
			shipGeoJSON.object()
				.key("type").value("FeatureCollection")
				.key("features").array()
					.object()
						.key("type").value("Feature")
						.key("properties").object()
							.key("height").value(5)
							.key("minHeight").value(0)
							.key("color").value("black")
							.key("roofColor").value("black")
						.endObject()
						.key("geometry").object()
							.key("type").value("Polygon")
							.key("coordinates").array()
								.array();
			
			shipGeoJSON.array().value(coordinateX + 0.0001).value(coordinateY).endArray();
			shipGeoJSON.array().value(coordinateX).value(coordinateY - 0.0001).endArray();
			shipGeoJSON.array().value(coordinateX - 0.0001).value(coordinateY).endArray();
			shipGeoJSON.array().value(coordinateX).value(coordinateY + 0.0001).endArray();
			shipGeoJSON.array().value(coordinateX + 0.0001).value(coordinateY).endArray();
			
			shipGeoJSON
								.endArray()
							.endArray()
						.endObject()
					.endObject()
				.endArray()
			.endObject();
			
		}
		queryExec.close();
		return shipGeoJSON;
	}

}
