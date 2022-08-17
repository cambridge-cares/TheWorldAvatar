package uk.ac.cam.cares.jps.base.discovery;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Enumeration;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.util.EntityUtils;
import org.json.JSONException;
import org.json.JSONObject;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.util.InputValidator;

public class AgentCaller {

     /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(AgentCaller.class);
            
    private static final String JSON_PARAMETER_KEY = "query";
    private static String hostPort = null;

    private static synchronized String getHostPort() {
        if (hostPort == null) {
            hostPort = AgentLocator.getProperty("host") + ":" + AgentLocator.getProperty("port");
        }
        return hostPort;
    }

    public static String executeGet(String path) {
        URIBuilder builder = getUriBuilderForPath(path);
        try {
            HttpGet request = new HttpGet(builder.build());
            return executeGet(request);
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    public static String executeGet(String path, String... keyOrValue) {

        URIBuilder builder = getUriBuilderForPath(path);

        for (int i = 0; i < keyOrValue.length; i = i + 2) {
            String key = keyOrValue[i];
            String value = keyOrValue[i + 1];
            builder.setParameter(key, value);
        }

        try {

            HttpGet request = new HttpGet(builder.build());
            return executeGet(request);
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Method to execute HTTP POST requests
     *
     * @param path Resource name, where the request goes to
     * @param body Request payload
     * @return Response body
     */
    public static String executePost(String path, String body) {
        String response_body;
        URIBuilder builder = getUriBuilderForPath(path);
        StringEntity entity = new StringEntity(body, ContentType.APPLICATION_JSON);

        try {
            HttpClient httpClient = HttpClientBuilder.create().build();
            HttpPost request = new HttpPost(builder.build());
            request.setEntity(entity);
            HttpResponse response = httpClient.execute(request);
            if (response.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
                HttpEntity rsp_entity = response.getEntity();
                response_body = EntityUtils.toString(rsp_entity, "UTF-8");
            } else {
                throw new JPSRuntimeException(response.getStatusLine().toString());
            }
        } catch (URISyntaxException | IOException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }

        return response_body;
    }
    
    public static String executePut(String path, String body) {
    	return executePut(path, body, null);
    }
    
    public static String executePut(String path, String body, String jsonParam) {
        String response_body;
        URIBuilder builder = getUriBuilderForPath(path);
        StringEntity entity = new StringEntity(body, ContentType.APPLICATION_JSON);

        if (jsonParam != null) {
        	builder.setParameter(JSON_PARAMETER_KEY, jsonParam);
        }
        
        try {
            HttpClient httpClient = HttpClientBuilder.create().build();
            HttpPut request = new HttpPut(builder.build());
            request.setEntity(entity);
            
            HttpResponse response = httpClient.execute(request);
            if (response.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
                HttpEntity rsp_entity = response.getEntity();
                response_body = EntityUtils.toString(rsp_entity, "UTF-8");
            } else {
                throw new JPSRuntimeException(response.getStatusLine().toString());
            }
        } catch (URISyntaxException | IOException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }

        return response_body;
    }

    /**
     * General method to get bare URI builder for a given resource name.
     *
     * @param path Resource the request goes to
     * @return URIBuilder
     */
    private static URIBuilder getUriBuilderForPath(String path) {
        // TODO-AE maybe use directly class java.net.URI
        // TODO-AE refactor get hostname
        URIBuilder builder;
        try {
            builder = new URIBuilder(Http.createURI(path));
        } catch (Exception e) {
            builder = new URIBuilder().setScheme("http").setHost(getHostPort()).setPath(path);
        }
        return builder;
    }

    public static String executeGetWithURL(String url) {
        URI uri = Http.createURI(url);
        HttpGet request = new HttpGet(uri);
        return AgentCaller.executeGet(request);
    }

    public static String executeGetWithURLAndJSON(String url, String json) {
        URI uri = createURIWithURLandJSON(url, json);
        HttpGet request = new HttpGet(uri);
        LOGGER.info("REQUEST HERE= "+request);
        return AgentCaller.executeGet(request);
    }

    public static URI createURIWithURLandJSON(String url, String json) {
        return Http.createURI(url, JSON_PARAMETER_KEY, json);
    }
     
    /**
     * @deprecated Use method in Http.
     * @param url
     * @param keyOrValue
     * @return
     */
    @Deprecated
    public static URI createURI(String url, String... keyOrValue) {
    	return Http.createURI(url, keyOrValue);
    }
    
    public static String executeGetWithURLKey(String urlKey, MediaType type, String... keyOrValue) {

        String url = KeyValueManager.get(urlKey);
        URI uri = Http.createURI(url, keyOrValue);
        HttpGet request = new HttpGet(uri);
        if (type != null) {
            request.setHeader(HttpHeaders.ACCEPT, type.type);
        }

        return executeGet(request);
    }

    /**
     * Executes GET request <host>/path?query=<json>
     *
     * @param path
     * @param json
     * @return
     */
    public static String executeGetWithJsonParameter(String path, String json) {
        URIBuilder builder = new URIBuilder().setScheme("http").setHost(getHostPort())
                .setPath(path);

        builder.setParameter(JSON_PARAMETER_KEY, json);

        try {
            HttpGet request = new HttpGet(builder.build());
            request.setHeader("Accept", "application/json");
            request.setHeader("Content-type", "application/json");

            return executeGet(request);
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    /**
     * Returns the JSONObject for the serialized JSON document of parameter with key "query". If there no such key,
     * then a JSONObject is created of the form { "key1": "value1", "key2": "value2", ... }. for the url query component
     * ?key1=value1&key2=value2&...
     *
     * @param request
     * @return
     */
    public static JSONObject readJsonParameter(HttpServletRequest request) {

        try {
            String json = null;
            if (request.getMethod().equals(HttpGet.METHOD_NAME)) {
                json = request.getParameter(JSON_PARAMETER_KEY);
                if (json != null) {
                	JSONObject jo = new JSONObject();
                	if (InputValidator.checkIfValidJSONObject(json)) {
                		jo = new JSONObject(json);//scenario resource, agent etc
                	}else {
                		jo.put(JPSConstants.CONTENT, json);//string content
            			
                	}
                	jo.put(JPSConstants.HEADERS, getAccept(request))
                	.put(JPSConstants.METHOD, request.getMethod())
        			.put(JPSConstants.PATH, request.getPathInfo())
        			.put(JPSConstants.CONTENTTYPE, request.getContentType())
        			.put(JPSConstants.REQUESTURL, request.getRequestURL().toString());
                    return jo;
                }
            }
            

            JSONObject jsonobject = Http.readJsonParameter(request);
            if (request.getMethod().equals(HttpPut.METHOD_NAME)
            		|| request.getMethod().equals(HttpPost.METHOD_NAME)) {
                json =IOUtils.toString(request.getReader()); 
                String json2 = request.getParameter(JSON_PARAMETER_KEY);
                if (json2 != null) {
                	//Since request.getParameterNames doesn't work
                	JSONObject jo = new JSONObject(json2);
                	for(String key : JSONObject.getNames(jo))
                	{
                	  jsonobject.put(key, jo.get(key));
                	}
                }if (InputValidator.checkIfValidJSONObject(json)){
                	JSONObject jo = new JSONObject(json);
                	for(String key : JSONObject.getNames(jo))
                	{
                	  jsonobject.put(key, jo.get(key));
                	}
                }
            	}                
            jsonobject.put(JPSConstants.METHOD, request.getMethod())
            .put(JPSConstants.HEADERS, getAccept(request))
			.put(JPSConstants.CONTENT, json)
			.put(JPSConstants.PATH, request.getPathInfo())
			.put(JPSConstants.CONTENTTYPE, request.getContentType())
			.put(JPSConstants.REQUESTURL, request.getRequestURL().toString());
            return jsonobject;

        } catch (JSONException | IOException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }
    protected static String getAccept(HttpServletRequest req) {
		String accept = null;
		Enumeration<String> acceptList = req.getHeaders(HttpHeaders.ACCEPT);
		if (acceptList.hasMoreElements()) {
			accept = acceptList.nextElement();
		}
		LOGGER.info("accept = " + accept);
		return accept;
	}
    
    /**
     * @deprecated Use method in Http
     * @param response
     * @param json
     * @throws IOException
     */
    @Deprecated
    public static void writeJsonParameter(HttpServletResponse response, JSONObject json) throws IOException {
    	Http.writeJsonParameter(response, json);
    }

    public static String executeGet(HttpGet request) {

        CloseableHttpResponse httpResponse = null;

        try {
            StringBuffer buf = new StringBuffer(request.getMethod()).append(" ").append(request.getURI().getScheme()).append("://")
                    .append(request.getURI().getHost());
            int port = request.getURI().getPort();
            if (port > -1) {
                buf.append(":").append(request.getURI().getPort());
            }

            buf.append(request.getURI().getPath());
            String requestAsString = buf.toString();
            String query = request.getURI().getQuery();
            if (query != null) {
                int length = query.length();
                if (length > 100) {
                    query = query.substring(0, 99);
                }
                buf.append("?").append(query);
            }
            LOGGER.info(buf.toString());
            // use the next line to log the percentage encoded query component
            //LOGGER.info(request.toString());

            httpResponse = HttpClientBuilder.create().build().execute(request);

            if (httpResponse.getStatusLine().getStatusCode() != 200) {
                String body = EntityUtils.toString(httpResponse.getEntity());
                LOGGER.error(body);
                String message = "original request = " + requestAsString;
                if (request.getURI().getQuery() != null) {
                    message += "?" + request.getURI().getQuery();
                }
                LOGGER.info(message);
                throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine() + ", " + message);
            }

            String body = EntityUtils.toString(httpResponse.getEntity());
            LOGGER.debug(body);
            return body;
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        } finally {
            if (httpResponse != null) {
                try {
                    httpResponse.close();
                } catch (IOException e) {
                    e.printStackTrace();
                    LOGGER.error(e.getMessage(), e);
                }
            }
        }
    }

    /** queries URL for data in requestBody
     * checks if HTTPUrlConnection is ok
     * @param url
     * @return requestBody as String
     * @throws IOException
     */
    public static String getRequestBody(String url) {
    	try {
    	URL urlForGetRequest = new URL(url);
	    HttpURLConnection conection = (HttpURLConnection) urlForGetRequest.openConnection();
	    conection.setRequestMethod("GET");
	    String readLine = null;
	    int responseCode = conection.getResponseCode();
	    if (responseCode == HttpURLConnection.HTTP_OK) {
	    	 BufferedReader in = new BufferedReader(new InputStreamReader(conection.getInputStream()));
	    	 StringBuffer response = new StringBuffer();
	    	 while ((readLine = in.readLine()) != null) {
	    		 response.append(readLine);
	    	 	}
	    	 in.close();

		 return response.toString();
	    }else {
	    	throw new JPSRuntimeException("Failure to connect");
	    	
	    }
	    }catch (MalformedURLException e) {
	    	throw new JPSRuntimeException("Malformed URL "+ url + "; try again.");
	    	
	    }catch (ProtocolException e) {
	    	throw new JPSRuntimeException("Protocol Exception "+ url + "; try again.");
	    }catch (IOException e){
	    	throw new JPSRuntimeException("IO Exception "+ url + "; try again.");
	    }
    }
    
    /**
     * @deprecated Use method in Http.
     * @param object
     * @param resp
     */
    @Deprecated
    public static void printToResponse(Object object, HttpServletResponse resp) {
    	Http.printToResponse(object, resp);
    }
    
    
    /**
     * @deprecated Use method in Http.
     * @param object
     * @return
     */
    @Deprecated
    public static String serializeForResponse(Object object) {
    	return Http.serializeForResponse(object);
    }
    
    /**
     * @deprecated Use method in Http.
     * @param s
     * @return
     */
    @Deprecated
    public static String encodePercentage(String s) {
    	return Http.encodePercentage(s);
    }
    
    /**
     * @deprecated Use method in Http.
     * @param s
     * @return
     */
    @Deprecated
    public static String decodePercentage(String s) {
    	return Http.decodePercentage(s);
    }
}
