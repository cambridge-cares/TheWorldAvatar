package uk.ac.cam.cares.jps.scenario.kb;

import java.io.IOException;
import java.util.Enumeration;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.core.Response;

import org.apache.http.HttpHeaders;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.query.KnowledgeBaseClient;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

@WebServlet(urlPatterns = {"/kb/*", "/data/*", "/dataset/*"})
public class KnowledgeBaseAgent extends JPSAgent {

	private static final long serialVersionUID = -4195274773048314961L;
	private static Logger logger = LoggerFactory.getLogger(KnowledgeBaseAgent.class);
	/** empty Result for now, because getAccept still requires headers. 
	 * 
	 */
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		return new JSONObject();
	}
	@Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
		System.out.println("JSON PARAMS" + requestParams.toString());
		if (!validateInput(requestParams)) {
			throw new JSONException("KnowledgeBaseAgent: input Parameters not found!");
		}
		String body = MiscUtil.optNullKey(requestParams, "body");
		String requestUrl = MiscUtil.optNullKey(requestParams, "requestUrl");
		String path = MiscUtil.optNullKey(requestParams, "path");
		String contentType = MiscUtil.optNullKey(requestParams, "contentType");
		String paramResourceUrl= MiscUtil.optNullKey(requestParams,JPSConstants.SCENARIO_RESOURCE);
        String sparql = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_UPDATE);
        String method = MiscUtil.optNullKey(requestParams, "method");
		
        String paramDatasetUrl = MiscUtil.optNullKey(requestParams, JPSConstants.SCENARIO_DATASET);
		String datasetUrl = KnowledgeBaseManager.getDatasetUrl(requestUrl);
		KnowledgeBaseAbstract kb = KnowledgeBaseManager.getKnowledgeBase(datasetUrl);
		String resourceUrl = getResourceUrl(datasetUrl, requestUrl, paramResourceUrl);
		
		try {
			if (method.equals(HttpGet.METHOD_NAME)) {
            	sparql = MiscUtil.optNullKey(requestParams, JPSConstants.QUERY_SPARQL_QUERY);
                
	            String accept = getAccept(request);
				logInputParams("GET", requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparql, false);
//				
				String result = "";	
				if (sparql == null) {
					result = kb.get(resourceUrl, accept);
				} else {
					result = kb.query(resourceUrl, sparql);
				}
				return new JSONObject().put("result", result);

            }else if (method.equals(HttpPost.METHOD_NAME)) {
        		logInputParams("POST", requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparql, false);
				
				if (sparql == null) {
					throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is missing");
				}
				
				kb.update(resourceUrl, sparql);
            }else  if (method.equals(HttpPut.METHOD_NAME)) {
        		logInputParams("PUT", requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparql, false);
    			
    			if (sparql != null) {
    				throw new JPSRuntimeException("parameter " + JPSConstants.QUERY_SPARQL_UPDATE + " is not allowed");
    			}    			
    			kb.put(resourceUrl, body, contentType);
            }
			
			
		}catch (JPSRuntimeException e) {
			e.printStackTrace();
		}catch (Exception e) {
			e.printStackTrace();
		}
        return new JSONObject();
        }
	
	/**
     * Method to group pre-processing steps common to all Http request methods
     *
     * @param request  HTTP Servlet Request
     * @param response HTTP Servlet Rsponse
     * @throws IOException @see PrintWriter#getWriter
     */
    protected void doHttpJPS(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
    	setLogger();
        try {
            String responseBody = getResponseBody(request);
            JSONObject jo = new JSONObject(responseBody);
            if (jo.has("result")) {
                response.getWriter().write(jo.getString("result"));
            }else {
            	response.getWriter().write(responseBody);
            }
        } catch (BadRequestException e) {
            response.setStatus(Response.Status.BAD_REQUEST.getStatusCode());
        } catch (JPSRuntimeException e) {
        	response.setStatus(Response.Status.SERVICE_UNAVAILABLE.getStatusCode());
        }
    }
//	@Override
//	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
//		
//		String requestUrl = req.getRequestURL().toString();
//		String path = req.getPathInfo();
//		JSONObject input = Http.readJsonParameter(req);
//		String sparql = MiscUtil.optNullKey(input, JPSConstants.QUERY_SPARQL_QUERY);
//		String paramDatasetUrl = MiscUtil.optNullKey(input, JPSConstants.SCENARIO_DATASET);
//		String paramResourceUrl = MiscUtil.optNullKey(input, JPSConstants.SCENARIO_RESOURCE);
//		String contentType = req.getContentType();
//		
//		try {
//			
//			String accept = getAccept(req);
//			
//			String datasetUrl = KnowledgeBaseManager.getDatasetUrl(requestUrl);
//			KnowledgeBaseAbstract kb = KnowledgeBaseManager.getKnowledgeBase(datasetUrl);
//			String resourceUrl = getResourceUrl(datasetUrl, requestUrl, paramResourceUrl);
//			
//			String result = "";	
//			logInputParams("GET", requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparql, false);
//			
//			if (sparql == null) {
//				result = kb.get(resourceUrl, accept);
//			} else {
//				result = kb.query(resourceUrl, sparql);
//			}
//			
//			Http.printToResponse(result, resp);
//
//		} catch (RuntimeException e) {
//			e.printStackTrace();
//			logInputParams("GET", requestUrl, path, paramDatasetUrl, paramResourceUrl, contentType, sparql, true);
//			throw e;
//		}
//	}

	@Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            throw new BadRequestException();
        }
        try {
	        boolean q = InputValidator.checkURLpattern(requestParams.getString("requestUrl"));
	        String method = MiscUtil.optNullKey(requestParams, "method");
	        if (method == null) {
	        	return false;
	        }
	        return q;
        }catch (JSONException ex) {
        	ex.printStackTrace();
        	return false;
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
	
	public String getResourceUrl(String datasetUrl, String requestUrl, String parameterUrl) {

		// Example: datasetUrl = http://www.thw.com/jps/data/test
		
		if (requestUrl.equals(datasetUrl)) {
			
			if ((parameterUrl == null) || parameterUrl.isEmpty()) {
				return null;
			} else {
				// case 2: indirect query
				return KnowledgeBaseClient.cutHashFragment(parameterUrl);
			}
			
		} else {
			if ((parameterUrl == null) || parameterUrl.isEmpty()) {
				// case 3: direct query
				return requestUrl;
			}
		}
		
		String message = "A URL was given by the query parameter " + JPSConstants.SCENARIO_RESOURCE 
				+ ". This is not allowed since the requested URL does not define a dataset URL."
				+ " parameter URL = " + parameterUrl + ", requested URL=" + requestUrl;
		throw new JPSRuntimeException(message);
	}
	
	protected void logInputParams(String httpVerb, String requestUrl, String path, String datasetUrl, String resourceUrl, String contentType, String sparql, boolean hasErrorOccured) {
		StringBuffer b = new StringBuffer(httpVerb);
		b.append(" with requestedUrl=").append(requestUrl);
		b.append(", path=").append(path);
		b.append(", datasetUrl=").append(datasetUrl);
		b.append(", resourceUrl=").append(resourceUrl);
		b.append(", contentType=").append(contentType);
		if (hasErrorOccured) {
			b.append(", sparql=" + sparql);
			logger.error(b.toString());
		} else {
			if (sparql != null) {
				int i = sparql.toLowerCase().indexOf("select");
				if (i > 0) {
					sparql = sparql.substring(i);
				}
				if (sparql.length() > 150) {
					sparql = sparql.substring(0, 150);
				}
			}
			b.append(", sparql (short)=" + sparql);
			logger.info(b.toString());
		}
	}
}
