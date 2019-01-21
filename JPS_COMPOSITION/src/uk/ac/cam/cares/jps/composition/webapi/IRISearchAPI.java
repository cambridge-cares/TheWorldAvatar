package uk.ac.cam.cares.jps.composition.WebAPI;

import java.io.IOException;
import java.net.URLEncoder;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.Header;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.ParseException;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;


/**
 * Author: Zhou Xiaochi 
 * Servlet implementation class IRISearchAPI
 */
@WebServlet("/IRISearchAPI")

public class IRISearchAPI extends HttpServlet {
	private static final long serialVersionUID = 1L;
	public  static String mode;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public IRISearchAPI() {
        super();
    }

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String keyword  = request.getParameter("keyword").replace(" ", "_");
		mode = request.getParameter("mode");
		try {
			String OLSResult = convertOLSResult(queryOLS(keyword));
			if (!OLSResult.contentEquals("{\"results\": []}")) {
				// Important: Since OLS is a much smaller database, we search it first and the search dbpedia only if there is nothing returned from OLS
				response.getWriter().append(OLSResult);
			}
			else {
				String DBPediaResult = convertDBPediaResult(queryDBpedia(keyword));
				response.getWriter().append(DBPediaResult);
			}
		} catch (ParseException e) {
			e.printStackTrace();
		} catch (JSONException e) {
			e.printStackTrace();
		}
	}

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}

	// This function searches the dbpedia lookup endpoint... 
	public String queryDBpedia(String keyword) throws ParseException, IOException
	{		
		String requestUrl = "http://lookup.dbpedia.org/api/search/KeywordSearch?QueryClass=&MaxHits=10&QueryString=" + keyword;
		HttpUriRequest request = new HttpGet(requestUrl);
		request.setHeader(HttpHeaders.ACCEPT, "application/json");// Set the return format to be JSON (the default is xml)
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		String responseString = EntityUtils.toString(httpResponse.getEntity());
		return responseString;
	}
	
	// This function searches the OLS lookup server which is currently hosted in NLP Cloud Server at 47.74.244.61
	@SuppressWarnings("deprecation")
	public  String queryOLS(String keyword) throws ClientProtocolException, IOException
	{
		String requestUrl = "http://47.74.244.61:8080/api/search?groupField=iri&start=0&q=" + URLEncoder.encode(createOLSSearchKeyword(keyword));
		HttpUriRequest request = new HttpGet(requestUrl);
		HttpResponse httpResponse = HttpClientBuilder.create().build().execute(request);
		String responseString = EntityUtils.toString(httpResponse.getEntity());
		return responseString;
	}
	
	public String convertOLSResult(String result) throws JSONException 
	{
		JSONObject obj = new JSONObject(result);
		JSONArray resultArray = obj.getJSONObject("response").getJSONArray("docs");
		// Basing on the mode selection, filter out all the individual/class
		if(resultArray.length() <= 0){
			return "{\"results\": []}";
		}
		
		JSONObject resultObj = new JSONObject();

		for (int i = 0; i < resultArray.length(); i++)
		{
			JSONObject currentObj = resultArray.getJSONObject(i);
			String IRI = currentObj.getString("iri");
			String label = currentObj.getString("label");
			String type  = currentObj.getString("type");
			String ontology_name = currentObj.getString("ontology_name");
			if (type.contentEquals(mode)) {
				JSONObject temp = new JSONObject();
				temp.put("name", label);
				JSONArray  innerArray = new JSONArray();
				JSONObject innerObj   = new JSONObject();
				innerObj.put("title", IRI);
				innerObj.put("description", "From domain: " + ontology_name);
				innerArray.put(innerObj);
				temp.put("results", innerArray);
				resultObj.put(label, temp);
			}
		}
		return new JSONObject().put("results",resultObj).toString(4);
	}
	
	/*
	 * @param result The returned result from the dbpedia lookup service
	 * @return JSON String that follows the format of a semantic-ui search result
	 */
	public String convertDBPediaResult(String result) throws JSONException	
	{
		JSONObject obj = new JSONObject(result);
		JSONArray  resultArray = obj.getJSONArray("results");
		JSONObject resultObj = new JSONObject();
		
		for (int i = 0; i < resultArray.length(); i++)
		{
			JSONObject temp = new JSONObject();
			JSONObject currentObj = resultArray.getJSONObject(i);
			String IRI   		= currentObj.getString("uri");
			String label 		= currentObj.getString("label");
			String description  = currentObj.getString("description");
			temp.put("name", label);
			JSONArray  innerArray = new JSONArray();
			JSONObject innerObj   = new JSONObject();
			innerObj.put("title", IRI);
			innerObj.put("description", description);
			innerArray.put(innerObj);
			temp.put("results", innerArray);
			resultObj.put(label, temp);
		}
		return new JSONObject().put("results",resultObj).toString(4);
	}
	
	public String createOLSSearchKeyword(String keyword)
	{
		return "{" + String.join(",", keyword.split("_")) + "}";
	}
}