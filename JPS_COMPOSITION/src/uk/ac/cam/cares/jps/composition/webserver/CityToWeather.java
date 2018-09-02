package uk.ac.cam.cares.jps.composition.webserver;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.vocabulary.RDF;
import org.json.JSONException;
import org.json.JSONObject;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.Lang;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.vocabulary.RDF;

 
import uk.ac.cam.cares.jps.composition.util.SendRequest;

/**
 * Servlet implementation class CoordinateToCity
 */
@WebServlet("/CityToWeather")
public class CityToWeather extends HttpServlet {
	private static final long serialVersionUID = 1L;
	public static final String queryIRILabel = "select distinct ?label where \n" + "{\n"
			+ " <%s> <http://www.w3.org/2000/01/rdf-schema#label> ?label .\n" + "FILTER (lang(?label) = 'en')\n"
			+ "}\n";
	public static final String weatherRequest = "http://api.openweathermap.org/data/2.5/weather?units=metric&q=%s&appid=329f65c3f7166977f6751cff95bfcb0a";

	public CityToWeather() {
		super();
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		String value = request.getParameter("value").replace("@", "#");

		Model model = ModelFactory.createDefaultModel();
		RDFDataMgr.read(model, new ByteArrayInputStream(value.getBytes("UTF-8")), Lang.RDFJSON);
		String cityIRI = getCityIRI(model);
		System.out.println("CityIRI : " + cityIRI);
		
		
		if (cityIRI.length() >= 1) {
			String cityName = queryCityNameLabel(cityIRI);
			try {
				System.out.println(cityName);
				if(cityName.equalsIgnoreCase("The Hague")) {
					cityName = "Den%20Haag";
				}
				System.out.println(cityName);
				response.getWriter().write(constructSemanticResponse(getWeatherFromCity(cityName), cityName.replaceAll("%20", "_"), cityIRI));
			} catch (Exception e) {
				e.printStackTrace();
			}

		} else {
			response.getWriter().write("Invalid IRI");
		}
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}

	public String queryCityNameLabel(String cityIRI) {
		ArrayList<String> parameters = new ArrayList<String>();
		parameters.add(cityIRI);
		return sendDBpediaQuery(generateQuery(queryIRILabel, parameters));
	}

	public String generateQuery(String template, ArrayList<String> parameters) {
		return String.format(template, parameters.get(0));
	}

	public String sendDBpediaQuery(String query) {
		QueryExecution exec = QueryExecutionFactory.sparqlService("http://dbpedia.org/sparql", query);
		ResultSet results = exec.execSelect();
		String label = null;
		while (results.hasNext()) {
			label = results.next().get("label").toString();
		}
		if (label == null) {
			return label;
		} else {
			if (label.contains("@")) {
				return label.split("@")[0];
			} else {
				return label;
			}
		}
	}

	public String getWeatherFromCity(String cityName) throws Exception {
		String requestURL = String.format(weatherRequest, cityName);
		System.out.println(requestURL);
		String result = SendRequest.sendGet(requestURL);
		System.out.println(result);
		return result;
	}

	public Resource selectWeatherCondition(String cityName, Model rdfModel, String weatherConditionInString) {
		Resource WeatherCondition = rdfModel.createResource(
				"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherCondition");

		Resource selected = null;
		switch (weatherConditionInString) {
		case "clear sky":
			Resource Sun = rdfModel.createResource(
					"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Sun");
			Sun.addProperty(RDF.type, WeatherCondition);
			selected = Sun;
			break;
		case "few cloud":

			Resource LightCloud = rdfModel.createResource(
					"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#LightCloud");
			LightCloud.addProperty(RDF.type, WeatherCondition);

			selected = LightCloud;
			break;

		case "scattered clouds":
			Resource PartlyCloud = rdfModel.createResource(
					"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#PartlyCloud");
			PartlyCloud.addProperty(RDF.type, WeatherCondition);
			selected = PartlyCloud;
			break;

		case "broken clouds":
			Resource Cloud = rdfModel.createResource(
					"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Cloud");
			Cloud.addProperty(RDF.type, WeatherCondition);
			selected = Cloud;
			break;

		case "shower rain":
			Resource Rain = rdfModel.createResource(
					"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Rain");
			Rain.addProperty(RDF.type, WeatherCondition);
			selected = Rain;
			break;

		case "rain":
			Resource Rain2 = rdfModel.createResource(
					"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Rain");
			Rain2.addProperty(RDF.type, WeatherCondition);
			selected = Rain2;
			selected = Rain2;
			break;

		case "thunderstorm":
			Resource Thunder = rdfModel.createResource(
					"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Thunder");
			Thunder.addProperty(RDF.type, WeatherCondition);

			selected = Thunder;
			break;

		case "snow":
			Resource Snow = rdfModel.createResource(
					"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Snow");
			Snow.addProperty(RDF.type, WeatherCondition);

			selected = Snow;
			break;
		case "mist":
			Resource Fog = rdfModel.createResource(
					"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Fog");
			Fog.addProperty(RDF.type, WeatherCondition);
			selected = Fog;
			break;

		default:
			Resource Sun2 = rdfModel.createResource(
					"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Sun");
			Sun2.addProperty(RDF.type, WeatherCondition);

			selected = Sun2;
			break;
		}

		return selected;
	}

	public String constructSemanticResponse(String weatherInString, String cityName, String cityIRI)
			throws JSONException {
		JSONObject weatherInJSON = new JSONObject(weatherInString.replace("%20", "_"));
		JSONObject main = weatherInJSON.getJSONObject("main");
		JSONObject weatherObject = weatherInJSON.getJSONArray("weather").getJSONObject(0);

		String description = weatherObject.getString("description");
		String icon = weatherObject.getString("icon");

		String humidity = main.getString("humidity");
		String temperature = main.getString("temp");
		String precipitationIntensity = "0.0";
		
		String cloudCover = weatherInJSON.getJSONObject("clouds").getString("all");
		
		JSONObject windInJSON = weatherInJSON.getJSONObject("wind");
		String wind_speed = windInJSON.getString("speed");
		String wind_direction = "";
		if (windInJSON.has("deg")) {
			wind_direction = windInJSON.getString("deg");
		} else {
			wind_direction = "";
		}
		
		if (weatherInJSON.has("rain")) {
			precipitationIntensity = weatherInJSON.getJSONObject("rain").getString("3h");
		}
		

		Model rdfModel = ModelFactory.createDefaultModel();
		Resource weather = rdfModel
				.createResource("http://www.theworldavatar.com/WeatherOf" + cityName + cityName.hashCode());
		weather.addProperty(RDF.type,
				"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#WeatherState");
		Property hasHumidity = rdfModel.createProperty(
				"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasHumidity");
		Property hasTemperature = rdfModel.createProperty(
				"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasTemperature");
		Resource wind = rdfModel
				.createResource("http://www.theworldavatar.com/WindOf" + cityName + cityName.hashCode());
		Property hasWind = rdfModel.createProperty(
				"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasWind");
		Property hasSpeed = rdfModel.createProperty(
				"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasSpeed");
		Property hasDirection = rdfModel.createProperty(
				"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasDirection");
		wind.addProperty(RDF.type,
				"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Wind");
		Property weatherIcon = rdfModel.createProperty("http://www.theworldavatar.com/weather.owl#hasIcon");
		Resource weatherConditionResource = selectWeatherCondition(cityName, rdfModel, description);
		Property hasWeatherCondition = rdfModel.createProperty(
				"https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasWeatherCondition");

		Property locationCity = rdfModel.createProperty("http://dbpedia.org/ontology/locationCity");
		Resource City = rdfModel.createResource(cityIRI);

		Resource precipitation = rdfModel.createResource("https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#Precipitation");
		Property hasPrecipitation = rdfModel.createProperty("https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasPrecipitation");
		Property hasIntensity = rdfModel.createProperty("https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasIntensity");


		Property hasCloudCover = rdfModel.createProperty("https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#hasCloudCover");
		
		
		
		
		Resource myPrecipitation = rdfModel.createResource("http://www.theworldavatar.com/PecipitationOf" + cityName + cityName.hashCode());
		myPrecipitation.addProperty(RDF.type, precipitation);
		myPrecipitation.addLiteral(hasIntensity, precipitationIntensity);		
		weather.addProperty(hasPrecipitation, myPrecipitation);
		

		wind.addProperty(hasSpeed, wind_speed);
		wind.addProperty(hasDirection, wind_direction);
		weather.addProperty(hasWind, wind);
		weather.addProperty(hasTemperature, temperature);
		weather.addProperty(hasHumidity, humidity);
		weather.addLiteral(weatherIcon, icon);
		weather.addProperty(hasWeatherCondition, weatherConditionResource);
		weather.addProperty(locationCity, City);
		weather.addLiteral(hasCloudCover, cloudCover);
		
		
		StringWriter out = new StringWriter();
		RDFDataMgr.write(out, rdfModel, RDFFormat.RDFJSON);
		return out.toString();
	}
	
	public String getCityIRI(Model model) {
		
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