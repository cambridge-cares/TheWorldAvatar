package uk.ac.cam.cares.jps.sparqlgui;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URI;

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
import org.json.JSONException;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

//import org.apache.http.HttpHeaders;
//import org.apache.http.HttpResponse;
//import org.apache.http.client.methods.HttpGet;
//import org.apache.http.client.utils.URIBuilder;
//import org.apache.http.impl.client.HttpClientBuilder;
//import org.apache.http.util.EntityUtils;
//import org.json.JSONException;
//
//import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Servlet implementation class SPARQLEndpointProxy
 */
@WebServlet("/SPARQLEndpointProxy")
public class SPARQLEndpointProxy extends HttpServlet {
	private static final long serialVersionUID = 1L;
       
    public SPARQLEndpointProxy() {
        super();
    }

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.setCharacterEncoding("UTF-8");
		String queryString = request.getParameter("queryString");
		String queryResult = null;
		try {
			queryResult = queryFromRDF4JServer(queryString);
		} catch (JSONException e) {
			e.printStackTrace();
		}
		
		response.getWriter().write(queryResult);
	}

	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
	
	public static String executeGet(URIBuilder builder) {
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
	
	public static String queryFromRDF4JServer(String mechanismquery) throws JSONException, UnsupportedEncodingException {

		String myHost = "www.theworldavatar.com" ;
		int myPort = 80;
		String myPath = "/sparqlendpoint/query";
				
		URIBuilder builder;
		builder = new URIBuilder().setScheme("http").setHost(myHost).setPort(myPort)
				.setPath(myPath)
				.setUserInfo("feroz", "password")
				.setParameter("query", mechanismquery)
				.setParameter("output", "json");
		
		String result = executeGet(builder);
		return result;
		
	}
	
//	public static String formRateConstantComparisonQuery(){
//		String queryString = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n";
//		queryString = queryString.concat("PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n");
//		queryString = queryString.concat("PREFIX ontochem: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#> \n");
//		queryString = queryString.concat("SELECT distinct ?mechanism1 ?mechanism2 ?reaction1 ?reaction2 ?activationEnergy1 ?activationEnergy2 ?preExpFactor1 ?preExpFactor2 \n");
//		queryString = queryString.concat("WHERE { \n");
//		queryString = queryString.concat("    ?reaction1 ontochem:hasEquation \"O + C3H4 [=] CH3 + HCCO\" . \n");
//		queryString = queryString.concat("    ?reaction1 ontochem:belongsToPhase ?phase1 . \n");
//		queryString = queryString.concat("    ?phase1 ontochem:containedIn ?mechanism1 . \n");
//		queryString = queryString.concat("    ?reaction1 ontochem:hasArrheniusCoefficient ?arrheniusRateCoefficients1 . \n");
//		queryString = queryString.concat("    ?arrheniusRateCoefficients1 ontochem:hasActivationEnergy ?activationEnergy1 . \n");
//		queryString = queryString.concat("    ?arrheniusRateCoefficients1 ontochem:hasPreExponentialFactor ?preExpFactor1 . \n");
//		queryString = queryString.concat("    ?arrheniusRateCoefficients1 ontochem:hasTemperatureExponent ?tempExponent1 . \n");
//		queryString = queryString.concat("    ?reaction2 ontochem:hasEquation \"O + C3H4 [=] CH3 + HCCO\" . \n");
//		queryString = queryString.concat("    ?reaction2 ontochem:belongsToPhase ?phase2 . \n");
//		queryString = queryString.concat("    ?phase2 ontochem:containedIn ?mechanism2 . \n");
//		queryString = queryString.concat("    ?reaction2 ontochem:hasArrheniusCoefficient ?arrheniusRateCoefficients2 . \n");
//		queryString = queryString.concat("    ?arrheniusRateCoefficients2 ontochem:hasActivationEnergy ?activationEnergy2 . \n");
//		queryString = queryString.concat("    ?arrheniusRateCoefficients2 ontochem:hasPreExponentialFactor ?preExpFactor2 . \n");
//		queryString = queryString.concat("    ?arrheniusRateCoefficients2 ontochem:hasTemperatureExponent ?tempExponent2 . \n");
//		queryString = queryString.concat("    FILTER ((?activationEnergy1 != ?activationEnergy2) && (?preExpFactor1 != ?preExpFactor2) && (?tempExponent1 != ?tempExponent2)) \n");
//		queryString = queryString.concat("	}");
//		return queryString;
//	}

}
