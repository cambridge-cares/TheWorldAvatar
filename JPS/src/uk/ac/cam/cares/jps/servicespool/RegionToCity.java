package uk.ac.cam.cares.jps.servicespool;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.building.CRSTransformer;

@WebServlet("/RegionToCity")
public class RegionToCity extends HttpServlet {

	private static final long serialVersionUID = 1L;
	public String requestUrlTemplate = "https://maps.googleapis.com/maps/api/geocode/json?latlng=%s&key=AIzaSyBgm3-eMQauJ_dW4Cq66Hg9aP50jpp24rA";
	public String requestUrl = "";

	public RegionToCity() {

	}

	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		/**
		 * This agent takes
		 */

		JSONObject input = null;
		JSONObject region = new JSONObject();
		try {
			input = new JSONObject(request.getParameter("query"));

			region.put("srsname", input.getJSONObject("region").get("srsname"));
			region.put("uppery", input.getJSONObject("region").getJSONObject("uppercorner").get("uppery"));
			region.put("upperx", input.getJSONObject("region").getJSONObject("uppercorner").get("upperx"));
			region.put("lowery", input.getJSONObject("region").getJSONObject("lowercorner").get("lowery"));
			region.put("lowerx", input.getJSONObject("region").getJSONObject("lowercorner").get("lowerx"));

		} catch (JSONException e1) {
			e1.printStackTrace();
		}
		String latlng = getCenterLatLon(region);

		try {
			String res = getCityNameFromCoordinate(latlng);
			String cityName = extractCityName(res);
			String cityIRI = lookUpCityName(cityName);
			JSONObject result = new JSONObject();
			result.put("city", cityIRI);
			response.getWriter().write(result.toString());
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}

	public String getCityNameFromCoordinate(String latlng) throws Exception {
		requestUrl = String.format(requestUrlTemplate, latlng);
		String result = sendGet(requestUrl);
		return result;
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
		String path = "https://lookup.dbpedia.org/api/search/KeywordSearch?QueryClass=place&QueryString=%s&MaxHits=1&format=json-ld";
		path = String.format(path, cityName);

		String result = sendGet(path);
		String cityIRI = "NOT IN KNOWLEDGEBASE";

		if (result != null) {
			JSONObject result_JSON = new JSONObject(result);
			JSONArray docs = result_JSON.getJSONArray("docs");
			if (docs != null & docs.length() != 0) {
				JSONObject r = (JSONObject) docs.get(0);
				JSONArray resource = r.getJSONArray("resource");
				if (result != null) {
					cityIRI = resource.getString(0);
				}
			}
		}
		
		if (cityIRI.contains("Hong_Kong")) {
			cityIRI = cityIRI.replace("British_", "");
		}
	
		return cityIRI;
	}

	public String getCenterLatLon(JSONObject region) {
		String result = "";
		try {
			double xmin = Double.parseDouble(String.valueOf(region.get("lowerx")));
			double xmax = Double.parseDouble(String.valueOf(region.get("upperx")));
			double ymin = Double.parseDouble(String.valueOf(region.get("lowery")));
			double ymax = Double.parseDouble(String.valueOf(region.get("uppery")));

			double xcenter = (xmax + xmin) / 2;
			double ycenter = (ymax + ymin) / 2;

			String ref = region.getString("srsname");
			String targetCRS = CRSTransformer.EPSG_4326;

			double[] oldPoint = new double[] { xcenter, ycenter };
			double[] newPoint = CRSTransformer.transform(ref, targetCRS, oldPoint);
			xcenter = newPoint[0];
			ycenter = newPoint[1];

			result = String.valueOf(ycenter) + "," + String.valueOf(xcenter);
			return result;
		} catch (NumberFormatException | JSONException e) {
			e.printStackTrace();
		}

		return result;
	}

	public String cityNameFilter(String cityName) throws UnsupportedEncodingException {
		if (cityName.endsWith(" Shi")) {
			cityName = cityName.replace(" Shi", "");
		}
		if (cityName.endsWith("-shi")) {
			cityName = cityName.replace("-shi", "");
		}
		System.out.println("City Selected : " + cityName);
		return URLEncoder.encode(cityName, "utf-8");
	}

	public String executeGet(URIBuilder builder) {
		try {
			URI uri = builder.build();
			HttpGet request = new HttpGet(uri);
			request.setHeader(HttpHeaders.ACCEPT, "application/json");
			HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
			if (httpResponse.getStatusLine().getStatusCode() != 200) {
				throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine());
			}
			return EntityUtils.toString(httpResponse.getEntity());
		} catch (Exception e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}

	public static String sendGet(String url) throws Exception {
		URL obj = new URL(url);

		try {
			HttpURLConnection con = (HttpURLConnection) obj.openConnection();
			con.setRequestMethod("GET");
			con.setRequestProperty("Accept", "application/json");
			BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
			String inputLine;
			StringBuffer response = new StringBuffer();
			while ((inputLine = in.readLine()) != null) {
				response.append(inputLine);
			}
			in.close();
			return response.toString();
		} catch (Exception e) {
			return null;
		}

	}
}
