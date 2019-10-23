package uk.ac.cam.cares.jps.scenario;

import java.io.IOException;
import java.util.Enumeration;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.http.HttpHeaders;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;

@WebServlet(urlPatterns = {"/kb/*", "/data/*"})
public class KnowledgeBaseManagementAgent extends HttpServlet {

	private static final long serialVersionUID = -4195274773048314961L;
	private static Logger logger = LoggerFactory.getLogger(KnowledgeBaseManagementAgent.class);
	
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		String requestUrl = req.getRequestURL().toString();
		String path = req.getPathInfo();
		JSONObject input = Http.readJsonParameter(req);
		String sparql = input.optString(JPSConstants.QUERY_SPARQL_QUERY);
		String parameterUrl = input.optString(JPSConstants.SCENARIO_RESOURCE);
		
		try {
			logInputParams("GET", requestUrl, path, parameterUrl, sparql, false);
			
			String accept = getAccept(req);
			
			String datasetUrl = getDatasetUrl(requestUrl);
			KnowledgeBaseAbstract kb = getKnowledgeBase(datasetUrl);
			String resourceUrl = getResourceUrl(datasetUrl, requestUrl, parameterUrl);
			
			String result = "";	
			if (sparql.isEmpty()) {
				result = kb.get(resourceUrl, accept);
			} else {
				result = kb.query(resourceUrl, sparql);
			}
			
			Http.printToResponse(result, resp);

		} catch (RuntimeException e) {
			e.printStackTrace();
			logInputParams("GET", requestUrl, path, parameterUrl, sparql, true);
			throw e;
		}
	}
	
	protected String getAccept(HttpServletRequest req) {
		String accept = null;
		Enumeration<String> acceptList = req.getHeaders(HttpHeaders.ACCEPT);
		if (acceptList.hasMoreElements()) {
			accept = acceptList.nextElement();
		}
		logger.info("accept = " + accept);
		return accept;
	}
	
	@Override
	protected void doPut(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		String requestUrl = req.getRequestURL().toString();
		String path = req.getPathInfo();
		JSONObject input = Http.readJsonParameter(req);
		String sparql = input.optString(JPSConstants.QUERY_SPARQL_UPDATE);
		String parameterUrl = input.optString(JPSConstants.SCENARIO_RESOURCE);
		
		try {
			logInputParams("PUT", requestUrl, path, parameterUrl, sparql, false);
			
			if (!sparql.isEmpty()) {
				throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_QUERY + " is not allowed");
			}
			
			String datasetUrl = getDatasetUrl(requestUrl);
			KnowledgeBaseAbstract kb = getKnowledgeBase(datasetUrl);
			String resourceUrl = getResourceUrl(datasetUrl, requestUrl, parameterUrl);
			String body = Http.getRequestBody(req);
			
			kb.put(resourceUrl, body);

		} catch (RuntimeException e) {
			e.printStackTrace();
			logInputParams("PUT", requestUrl, path, parameterUrl, sparql, true);
			throw e;
		}
	}
	
	/**
	 * @param requestUrl is of the form http:// host:port/<jps>/data/<datasetname>/...
	 * @return
	 */
	public String getDatasetUrl(String requestUrl) {
		String jps = "/" + JPSConstants.KNOWLEDGE_BASE_JPS + "/";
		int i = requestUrl.indexOf(jps) + jps.length();
		String rest = requestUrl.substring(i);
		if (rest.startsWith("data/")) {
			i += "data/".length();
		} else if (rest.startsWith("kb/")) {
			i += "kb/".length();
		} else if (rest.startsWith("scenario/")) {
			i += "scenario/".length();
		}
		String datasetUrl = requestUrl.substring(0, i);
		rest = requestUrl.substring(i);
		i = rest.indexOf("/");
		if (i >= 0) {
			datasetUrl += rest.substring(0, i);
		} else {
			datasetUrl = requestUrl;
		}
		return datasetUrl;
	}
	
	@Override
	protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		
		String requestUrl = req.getRequestURL().toString();
		String path = req.getPathInfo();
		JSONObject input = Http.readJsonParameter(req);
		String parameterUrl = input.optString(JPSConstants.SCENARIO_RESOURCE);
		JSONObject body = new JSONObject(Http.getRequestBody(req));
		String sparql = body.optString(JPSConstants.QUERY_SPARQL_UPDATE);

		try {
			logInputParams("POST", requestUrl, path, parameterUrl, sparql, false);
	
			if (sparql.isEmpty()) {
				throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_QUERY + " is missing");
			}
			
			String datasetUrl = getDatasetUrl(requestUrl);
			KnowledgeBaseAbstract kb = getKnowledgeBase(datasetUrl);
			String resourceUrl = getResourceUrl(datasetUrl, requestUrl, parameterUrl);

			updateKnowledgeBase(kb, resourceUrl, sparql);
			
		} catch (RuntimeException e) {
			e.printStackTrace();
			logInputParams("POST", requestUrl, path, parameterUrl, sparql, true);
			throw e;
		}
	}
	
	protected void updateKnowledgeBase(KnowledgeBaseAbstract kb, String resourceUrl, String sparql) {
		kb.update(resourceUrl, sparql);
	}
	
	public String getResourceUrl(String datasetUrl, String requestUrl, String parameterUrl) {

		if ((parameterUrl == null) || parameterUrl.isEmpty()) {
			return requestUrl;
		} else if (requestUrl.equals(datasetUrl)) {
			return parameterUrl;
		}
		
		String message = "A URL was given by the query parameter " + JPSConstants.SCENARIO_RESOURCE 
				+ ". This is not allowed since the requested URL does not define a dataset URL."
				+ " parameter URL = " + parameterUrl + ", requested URL=" + requestUrl;
		throw new JPSRuntimeException(message);
	}
	
	protected KnowledgeBaseAbstract getKnowledgeBase(String datasetUrl) {
		
		String[] preexistingDatasets = new String[] {"misc", "meta", "test"};
		for (String current: preexistingDatasets) {
			if (datasetUrl.endsWith(current)) {
				return new KnowledgeBaseFileBased();
			}
		}
		
		if (datasetUrl.contains("/scenario/")) {		
			return new KnowledgeBaseFileBased();
		}
		
		throw new JPSRuntimeException("unknown datasetUrl = " + datasetUrl);
	}
	
	protected void logInputParams(String httpVerb, String requestUrl, String path, String parameterUrl, String sparql, boolean hasErrorOccured) {
		StringBuffer b = new StringBuffer(httpVerb);
		b.append(" with requestedUrl=").append(requestUrl);
		b.append(", path=").append(path);
		b.append(", parameterUrl=").append(parameterUrl);
		if (hasErrorOccured) {
			b.append(", sparql=" + sparql);
			logger.error(b.toString());
		} else {
			if ((sparql != null) && sparql.length() > 100) {
				sparql = sparql.substring(0, 100);
			}
			b.append(", sparql (short)=" + sparql);
			logger.info(b.toString());
		}
	}
}
