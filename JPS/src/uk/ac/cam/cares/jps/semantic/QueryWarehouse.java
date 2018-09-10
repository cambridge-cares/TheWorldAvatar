package uk.ac.cam.cares.jps.semantic;

import java.io.StringWriter;
import java.util.ArrayList;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;

import uk.ac.cam.cares.jps.building.CRSTransformer;
import uk.ac.cam.cares.jps.building.SimpleBuildingData;

public class QueryWarehouse {
	
	public static JSONObject getRegionCoordinates(Model model) {
		String queryString = 
				"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" + 
				"SELECT ?lowerX ?upperX ?lowerY ?upperY ?ref  WHERE \n"
				+ "{"
				+ "?region <http://test.com/Property/upperPoint> ?upperPoint . \n"
				+ "?upperPoint <http://test.com/Property/hasX> ?upperX .\n"
				+ "?upperPoint <http://test.com/Property/hasY> ?upperY .\n"
				+ "?region <http://test.com/Property/lowerPoint> ?lowerPoint .\n"
				+ "?lowerPoint <http://test.com/Property/hasX> ?lowerX .\n"
				+ "?lowerPoint <http://test.com/Property/hasY> ?lowerY .\n"
				+ "?region <http://test.com/Property/referenceSystem> ?ref .\n"
				+ "}";
		
	
		Query query = QueryFactory.create(queryString);
		QueryExecution qe = QueryExecutionFactory.create(query, model);
		ResultSet results = qe.execSelect();
		
		JSONObject output = new JSONObject();
		while(results.hasNext()) {
			 try {
				 
				QuerySolution nextSolution = results.nextSolution();
				
				System.out.println("---------------85564251--------------");
				System.out.println(nextSolution.getLiteral("upperX").getString());
				System.out.println(nextSolution.getLiteral("upperY").getString());
				System.out.println(nextSolution.getLiteral("lowerX").getString());
				System.out.println(nextSolution.getLiteral("lowerY").getString());

				output.put("xmax", nextSolution.getLiteral("upperX").getString());
				output.put("ymax", nextSolution.getLiteral("upperY").getString());
				output.put("xmin", nextSolution.getLiteral("lowerX").getString());
				output.put("ymin", nextSolution.getLiteral("lowerY").getString());			
				output.put("ref", nextSolution.getLiteral("ref").getString());				

			} catch (JSONException e) {
				e.printStackTrace();
			}
			 
		}
		

		String ref,targetCRS;
		try {
			ref = output.getString("ref");
			if (ref.equalsIgnoreCase(CRSTransformer.EPSG_4326)) {
				String sourceCRS = CRSTransformer.EPSG_4326;			 
 				double[] upperPoint = new double[] {Double.parseDouble(output.getString("xmax")),Double.parseDouble(output.getString("ymax"))};
				double[] lowerPoint = new double[] {Double.parseDouble(output.getString("xmin")),Double.parseDouble(output.getString("ymin"))};
				
  				if (Double.parseDouble(output.getString("xmax")) > 5.0) {
  					targetCRS = CRSTransformer.EPSG_28992; // Berlin
  				} else {
  					System.out.println("====================== Hague ======================");
  					targetCRS = CRSTransformer.EPSG_28992;
  				} 
 				
				double[] newUpperPoint = CRSTransformer.transform(sourceCRS, targetCRS, upperPoint);
				double[] newLowerPoint = CRSTransformer.transform(sourceCRS, targetCRS, lowerPoint);
				output.put("xmax", newUpperPoint[0]);
				output.put("ymax", newUpperPoint[1]);
				output.put("xmin", newLowerPoint[0]);
				output.put("ymin", newLowerPoint[1]);
				output.put("ref", targetCRS);
				
			}
		} catch (JSONException e) {
			e.printStackTrace();
		}

		System.out.println();
		System.out.println(output.toString());
		System.out.println("-----------------------------");				

		
		return output;
	}
	
	
	public static JSONObject getWeatherData(Model model)  {
		JSONObject weather = new JSONObject();
		String queryString = 
				"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\r\n" + 
				"PREFIX weather: <https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#>\r\n" + 
				"\r\n" + 
				"SELECT ?weatherIRI ?cloudCover  ?windDirection ?windSpeed ?precitipation ?temperature \r\n" + 
				"WHERE {\r\n" + 
				"  ?weatherIRI  rdf:type \"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState\" .\r\n" + 
				"  ?weatherIRI  weather:hasPrecipitation ?t_precitipation .\r\n" + 
				"  ?t_precitipation weather:hasIntensity ?precitipation .\r\n" + 
				"  ?weatherIRI  weather:hasWind ?wind .\r\n" + 
				"  ?wind weather:hasDirection ?windDirection .\r\n" + 
				"  ?wind weather:hasSpeed ?windSpeed .\r\n" + 
				"  ?weatherIRI  weather:hasCloudCover ?cloudCover .\r\n"
				+ "?weatherIRI  weather:hasTemperature ?temperature .\r\n" + 
				"}\r\n" + 
				"";
		 
		System.out.println(queryString);

		Query query = QueryFactory.create(queryString);
		QueryExecution qe = QueryExecutionFactory.create(query, model);
		ResultSet results = qe.execSelect();
		while(results.hasNext()) {
				QuerySolution nextSolution = results.nextSolution();
				try {
					weather.put("weatherIRI", nextSolution.get("weatherIRI").toString());
					weather.put("cloudCover", nextSolution.get("cloudCover").toString());
					weather.put("windDirection", nextSolution.get("windDirection").toString());
					weather.put("windSpeed", nextSolution.get("windSpeed").toString());
					weather.put("precitipation", nextSolution.get("precitipation").toString());
					weather.put("temperature", nextSolution.get("temperature").toString());

				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}

				
		}
		return weather;
		
		
	}
	
	
	public static String getBuildingData(Model model) {
		
		//		building.addProperty(RDF.type, "http://test.com/Ontology/Building");

 
		SimpleBuildingData result = new SimpleBuildingData();
		
		String queryString = 
				"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"+ 
				"PREFIX bld: <http://test.com/Property/>\n" + 
				"SELECT *  WHERE \n"
				+ "{"
				+ "?building rdf:type  \"http://test.com/Ontology/Building\" . \n"
				+ "?building bld:hasBuildingType    ?BuildingType . \n "
				+ "?building bld:hasBuildingX       ?hasBuildingX . \n "
				+ "?building bld:hasBuildingY       ?hasBuildingY . \n "
				+ "?building bld:hasBuildingName    ?hasBuildingName . \n "
				+ "?building bld:hasBuildingHeigth  ?hasBuildingHeigth . \n "
				+ "?building bld:hasBuildingLength  ?hasBuildingLength . \n "
				+ "?building bld:hasBuildingWidth   ?hasBuildingWidth . \n "
				+ "?building bld:hasBuildingAngel   ?hasBuildingAngel . \n "
				+ "}";
		

		Query query = QueryFactory.create(queryString);
		QueryExecution qe = QueryExecutionFactory.create(query, model);
		ResultSet results = qe.execSelect();
		while(results.hasNext()) {
			QuerySolution nextSolution = results.nextSolution();
			result.BldIRI.add(nextSolution.get("building").toString());
			result.BldName.add(nextSolution.getLiteral("hasBuildingName").getString());
			result.BldType.add(nextSolution.getLiteral("BuildingType").getInt());
			result.BldX.add(nextSolution.getLiteral("hasBuildingX").getDouble());
			result.BldY.add(nextSolution.getLiteral("hasBuildingY").getDouble());

			result.BldLength.add(nextSolution.getLiteral("hasBuildingLength").getDouble());
			result.BldWidth.add(nextSolution.getLiteral("hasBuildingWidth").getDouble());
			result.BldAngle.add(nextSolution.getLiteral("hasBuildingAngel").getDouble());
			result.BldHeight.add(nextSolution.getLiteral("hasBuildingHeigth").getDouble());
			
 
		}
		
		ObjectMapper mapper = new ObjectMapper();
		mapper.setSerializationInclusion(Include.NON_NULL);
		mapper.setSerializationInclusion(Include.NON_EMPTY);
		String MessagePartInJSON = null;
		try {
			MessagePartInJSON = mapper.writerWithDefaultPrettyPrinter().writeValueAsString(result);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		} 
	
		return MessagePartInJSON;
	}
	
	
	public static String getPlantIRI(Model model) {
		
		String queryString = 
				"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"+ 
				"SELECT ?plant  WHERE \n"
				+ "{"
				+ "?plant rdf:type  <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Plant> . \n"
				+ "}";
		
		String plantIRI ="";
		
		Query query = QueryFactory.create(queryString);
		QueryExecution qe = QueryExecutionFactory.create(query, model);
		ResultSet results = qe.execSelect();
		while(results.hasNext()) {
			QuerySolution nextSolution = results.nextSolution();
			plantIRI = nextSolution.get("plant").toString();
		}
		return plantIRI;		
	}
	
	
	public static String getCityIRI(Model model) {
		
 
		
		
		String findCityQuery = 
				"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" + 
				"SELECT ?city WHERE \n"
				+ "{"
				+ "		?city rdf:type <http://www.theworldavatar.com/OntoEIP/supporting_concepts/space_and_time_v1.owl#City> .\n"
				+ "}";
		
		String cityIRI = "";
		Query query = QueryFactory.create(findCityQuery);
		QueryExecution qe = QueryExecutionFactory.create(query, model);
		ResultSet results = qe.execSelect();
		while(results.hasNext()) {
			QuerySolution nextSolution = results.nextSolution();
			cityIRI = (nextSolution.getResource("city").getURI());
			System.out.println("---- CityIRI ----" + cityIRI);
		}
		return cityIRI;
	}
	
	
	
	
	
}
