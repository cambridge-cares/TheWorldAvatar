package uk.ac.cam.cares.jps.base.discovery;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;
import uk.ac.cam.cares.jps.base.util.InputValidator;


public class AgentCaller {

    private static final String JSON_PARAMETER_KEY = "query";
    private static Logger logger = LoggerFactory.getLogger(AgentCaller.class);
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
            builder = new URIBuilder(createURI(path));
        } catch (Exception e) {
            builder = new URIBuilder().setScheme("http").setHost(getHostPort()).setPath(path);
        }
        return builder;
    }

    public static String executeGetWithURL(String url) {
        URI uri = createURI(url);
        HttpGet request = new HttpGet(uri);
        return AgentCaller.executeGet(request);
    }

    public static String executeGetWithURLAndJSON(String url, String json) {
        URI uri = createURIWithURLandJSON(url, json);
        HttpGet request = new HttpGet(uri);
        logger.info("REQUEST HERE= "+request);
        return AgentCaller.executeGet(request);
    }

    public static URI createURIWithURLandJSON(String url, String json) {
        return createURI(url, JSON_PARAMETER_KEY, json);
    }

    public static URI createURI(String url, String... keyOrValue) {

        int j = url.indexOf(':');
        String scheme = url.substring(0, j);
        URIBuilder builder = new URIBuilder().setScheme(scheme);

        url = url.substring(j + 3);
        j = url.indexOf('/');
        String path = url.substring(j);
        builder.setPath(path);

        String host = url.substring(0, j);
        j = host.indexOf(':');
        if (j == -1) {
            builder.setHost(host);
        } else {
            String[] split = host.split(":");
            builder.setHost(split[0]);
            int port = Integer.valueOf(split[1]);
            builder.setPort(port);
        }

        if (!ArrayUtils.isEmpty(keyOrValue)) {
            for (int i = 0; i < keyOrValue.length; i = i + 2) {
                String key = keyOrValue[i];
                String value = keyOrValue[i + 1];
                builder.setParameter(key, value);
            }
        }

        try {
            return builder.build();
        } catch (URISyntaxException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    public static String executeGetWithURLKey(String urlKey, MediaType type, String... keyOrValue) {

        String url = KeyValueManager.get(urlKey);
        URI uri = createURI(url, keyOrValue);
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
            if (request.getMethod().equals(HttpPost.METHOD_NAME)) {
                json = IOUtils.toString(request.getReader());
            } else if (request.getMethod().equals(HttpGet.METHOD_NAME)) {
                json = request.getParameter(JSON_PARAMETER_KEY);
            }
            System.out.println("AgentCaller: json "+ json);
            if (json != null) {
            	JSONObject jo = new JSONObject();
            	if (InputValidator.checkIfValidJSONObject(json)) {
            		jo = new JSONObject(json);
            	}else {
            		jo.put("body", json);
            	}
            	jo.put("method", request.getMethod());
            	jo.put("pathInfo", request.getPathInfo());
            	jo.put("contentType", request.getContentType());
            	jo.put("requestUrl", request.getRequestURL().toString());
            	
                return jo;
            }

            JSONObject jsonobject = new JSONObject();
            Enumeration<String> keys = request.getParameterNames();
            while (keys.hasMoreElements()) {
                String key = keys.nextElement();
                String value = request.getParameter(key);
                jsonobject.put(key, value);
            }
            if (request.getMethod().equals(HttpPut.METHOD_NAME)) {
                json =IOUtils.toString(request.getReader()); 
                String json2 = request.getParameter(JSON_PARAMETER_KEY);
                if (json2 != null) {
                	//Since request.getParameterNames doesn't work
                	JSONObject jo = new JSONObject(json2);
                	for(String key : JSONObject.getNames(jo))
                	{
                	  jsonobject.put(key, jo.get(key));
                	}
                }
            	}                
            jsonobject.put("method", request.getMethod())
			.put("body", json)
			.put("pathInfo", request.getPathInfo())
			.put("contentType", request.getContentType())
			.put("requestUrl", request.getRequestURL().toString());
            return jsonobject;

        } catch (JSONException | IOException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    public static void writeJsonParameter(HttpServletResponse response, JSONObject json) throws IOException {

        response.setContentType("application/json");
        PrintWriter out = response.getWriter();
        String message = json.toString();
        out.print(message);
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
            logger.info(buf.toString());
            // use the next line to log the percentage encoded query component
            //logger.info(request.toString());

            httpResponse = HttpClientBuilder.create().build().execute(request);

            if (httpResponse.getStatusLine().getStatusCode() != 200) {
                String body = EntityUtils.toString(httpResponse.getEntity());
                logger.error(body);
                String message = "original request = " + requestAsString;
                if (request.getURI().getQuery() != null) {
                    message += "?" + request.getURI().getQuery();
                }
                logger.info(message);
                throw new JPSRuntimeException("HTTP response with error = " + httpResponse.getStatusLine() + ", " + message);
            }

            String body = EntityUtils.toString(httpResponse.getEntity());
            logger.debug(body);
            return body;
        } catch (Exception e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        } finally {
            if (httpResponse != null) {
                try {
                    httpResponse.close();
                } catch (IOException e) {
                    e.printStackTrace();
                    logger.error(e.getMessage(), e);
                }
            }
        }
    }

    // TODO-AE this method seems not to be required.
    public static String getRequestBody(final HttpServletRequest req) {
        final StringBuilder builder = new StringBuilder();
        try (final BufferedReader reader = req.getReader()) {
            String line;
            while ((line = reader.readLine()) != null) {
                builder.append(line);
            }
            return builder.toString();
        } catch (final Exception e) {
            return null;
        }
    }

    public static void printToResponse(Object object, HttpServletResponse resp) {

        if (object == null) {
            return;
        }

        String message = serializeForResponse(object);
        resp.setContentType("text/plain");
        resp.setCharacterEncoding("UTF-8");
        try {
            resp.getWriter().print(message);
        } catch (IOException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }
    
    public static String serializeForResponse(Object object) {
        String message = null;
        if (object instanceof String) {
            message = (String) object;
        } else if (object instanceof JSONObject || object instanceof JSONArray) {
        	message = object.toString();
        } else {
            message = new Gson().toJson(object);
        }
        return message;
    }

    public static String encodePercentage(String s) {
        Charset charset = Charset.forName("UTF-8");
        List<BasicNameValuePair> params = Arrays.asList(new BasicNameValuePair("query", s));
        String encoded = URLEncodedUtils.format(params, charset);
        return encoded.substring(6);
    }

    public static String decodePercentage(String s) {
        Charset charset = Charset.forName("UTF-8");
        List<NameValuePair> pair = URLEncodedUtils.parse("query=" + s, charset);
        return pair.get(0).getValue();
    }
}
