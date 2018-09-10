package uk.ac.cam.cares.jps.composition.webserver;

import java.io.IOException;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFFormat;
import org.apache.jena.vocabulary.RDF;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


import uk.ac.cam.cares.jps.composition.util.SendRequest;

/**
 * Servlet implementation class CoordinateToCity
 */
@SuppressWarnings("serial")
@WebServlet("/CoordinateToCity")
public class CoordinateToCity extends HttpServlet {

	public final String requestUrl = "https://maps.googleapis.com/maps/api/geocode/json?latlng=%s,%s&key=AIzaSyBgm3-eMQauJ_dW4Cq66Hg9aP50jpp24rA";

	public CoordinateToCity() {
		super();
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		try {
			JSONObject value = new JSONObject(request.getParameter("value").replace("@", "#").replace("$", "#"));
			JSONObject coordinate = value.getJSONObject(
					"http://www.theworldavatar.com/OntoEIP/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#Coordinate");
			JSONArray lat = coordinate.getJSONArray(
					"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#space_and_time_extended:hasProjectedCoordinate_y");
			JSONArray lon = coordinate.getJSONArray(
					"http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#space_and_time_extended:hasProjectedCoordinate_x");
			JSONObject lat_obj = (JSONObject) lat.get(0);
			JSONObject lon_obj = (JSONObject) lon.get(0);

			String lat_value = lat_obj.getString("value");
			String lon_value = lon_obj.getString("value");

			String result = getCityNameFromCoordinate(lat_value, lon_value);
			String cityName = extractCityName(result);
			if (cityName != null) {
				String cityIRI = lookUpCityName(cityName);
				String resultRDF = convertIRIToRDF(cityIRI);
				response.getWriter().write(resultRDF);
			} else {
				response.getWriter().write("NOT A CITY");
			}

		} catch (JSONException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}

	public String getCityNameFromCoordinate(String lat, String lon) throws Exception {
		String url = String.format(requestUrl, lat, lon);
		return SendRequest.sendGet(url);
	}
	
	public String convertIRIToRDF(String cityIRI) {
		Model cityModel = ModelFactory.createDefaultModel();
		Resource myCity = cityModel.createResource(cityIRI);
		Resource city = cityModel.createResource("http://www.theworldavatar.com/OntoEIP/supporting_concepts/space_and_time_v1.owl#City");
		myCity.addProperty(RDF.type,city);
		
		
		StringWriter out = new StringWriter();		
		RDFDataMgr.write(out, cityModel, RDFFormat.RDFJSON);
		return out.toString();
	}

	public String extractCityName(String res) throws JSONException {
		JSONObject result = new JSONObject(res);
		JSONArray array = result.getJSONArray("results");
		if (array.length() == 0) {
			return res;
		}
		boolean foundCity = false;
		for (int k = 0; k < array.length(); k++) {
			JSONObject aResult = array.getJSONObject(k);
			JSONArray components = aResult.getJSONArray("address_components");
			for (int i = 0; i < components.length(); i++) {
				JSONObject component = components.getJSONObject(i);
				JSONArray types = component.getJSONArray("types");
				for (int j = 0; j < types.length(); j++) {
					String type = types.getString(j);
					if (type.contentEquals("locality")) {
						foundCity = true;
					}
				}
				if (foundCity) {
					return component.getString("long_name");
				}
			}
		}

		return null;
	}

	public String lookUpCityName(String cityName) throws Exception {
		cityName = cityNameFilter(cityName);
		String temp = "http://lookup.dbpedia.org/api/search/KeywordSearch?MaxHits=1&QueryClass=city&QueryString=%s";
		String url = String.format(temp, cityName.replace(" ", "_"));

		String result = SendRequest.sendGet(url);
		System.out.println("City Query :===================");
		System.out.println(result);
		if (result != null) {
			JSONObject result_JSON = new JSONObject(SendRequest.sendGet(url));
			if (result_JSON.getJSONArray("results").length() != 0) {
				JSONObject r = (JSONObject) result_JSON.getJSONArray("results").get(0);
				return r.getString("uri");
			} else {
				return "NOT IN KOWNLEDGEBASE";
			}
		} else {
			return "NOT IN KOWNLEDGEBASE";
		}

	}

	public String cityNameFilter(String cityName) throws UnsupportedEncodingException {
		if (cityName.endsWith(" Shi")) {
			cityName = cityName.replace(" Shi", "");
		}
		if (cityName.endsWith("-shi")) {
			cityName = cityName.replace("-shi", "");
		}
		System.out.println(cityName);
		return URLEncoder.encode(cityName, "utf-8");
	}

}