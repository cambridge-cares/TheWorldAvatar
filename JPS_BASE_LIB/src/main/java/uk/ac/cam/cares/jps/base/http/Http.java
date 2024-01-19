package uk.ac.cam.cares.jps.base.http;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.NameValuePair;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.google.gson.Gson;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.config.KeyValueManager;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class Http {

    /**
     * Logger for debug/info/error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(Http.class);

    public static final String JSON_PARAMETER_KEY = "query";
    private static String hostPort = null;

    private static synchronized String getHostPort() {
        if (hostPort == null) {
            hostPort = AgentLocator.getProperty("host") + ":" + AgentLocator.getProperty("port");
        }
        return hostPort;
    }

    /**
     * General method to get bare URI builder for a given resource name.
     *
     * @param path Resource the request goes to
     * @return URIBuilder
     */
    private static URIBuilder getUriBuilderForPath(String path) {
        return new URIBuilder().setScheme("http").setHost(getHostPort()).setPath(path);
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

    /**
     * Returns the JSONObject for the serialized JSON document of parameter with key "query". If
     * there no such key, then a JSONObject is created of the form { "key1": "value1", "key2":
     * "value2", ... }. for the url query component ?key1=value1&key2=value2&...
     *
     * @param request
     * @return
     */
    public static JSONObject readJsonParameter(HttpServletRequest request) {

        try {

            String json = request.getParameter(JSON_PARAMETER_KEY);
            if (json != null) {
                return new JSONObject(json);
            }

            JSONObject jsonobject = new JSONObject();
            Enumeration<String> keys = request.getParameterNames();
            while (keys.hasMoreElements()) {
                String key = keys.nextElement();
                String value = request.getParameter(key);
                jsonobject.put(key, value);
            }
            return jsonobject;

        } catch (JSONException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    public static void writeJsonParameter(HttpServletResponse response, JSONObject json) throws IOException {

        response.setContentType("application/json");
        PrintWriter out = response.getWriter();
        String message = json.toString();
        out.print(message);
    }

    public static String getRequestBody(final HttpServletRequest req) {

        // the following code will not preserve line endings
//        final StringBuilder builder = new StringBuilder();
//        try (final BufferedReader reader = req.getReader()) {
//            String line;
//            while ((line = reader.readLine()) != null) {
//                builder.append(line);
//            }
//            return builder.toString();
//        } catch (final Exception e) {
//            return null;
//        }
        // to preserve line endings, use this code
        try (InputStream inputStream = req.getInputStream()){
            return FileUtil.inputStreamToString(inputStream);
        } catch (IOException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    public static void printToResponse(Object body, HttpServletResponse resp) {

        if (body == null) {
            return;
        }
        String bodyAsString = serializeForResponse(body);
        resp.setContentType("text/plain");
        resp.setCharacterEncoding("UTF-8");
        try {
            resp.getWriter().print(bodyAsString);
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

    private static String toString(Object object) {
        if (object instanceof String) {
            return (String) object;
        } else if (object instanceof JSONObject) {
            return ((JSONObject) object).toString();
        }
        return new Gson().toJson(object);
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

    public static URI createURIFlex(String urlOrPathOrKey, Object... params) throws URISyntaxException {
        URI result = null;

        String url = null;
        if (urlOrPathOrKey.startsWith("http")) {
            url = urlOrPathOrKey;
        } else if (urlOrPathOrKey.startsWith("\\") || urlOrPathOrKey.startsWith("/")) {
            URIBuilder builder = getUriBuilderForPath(urlOrPathOrKey);
            url = builder.build().toString();
        } else {
            url = KeyValueManager.get(urlOrPathOrKey);
        }

        if (params.length == 0) {
            result = createURI(url);
        } else if (params.length == 1) {
            if (params[0] == null) {
                result = createURI(url);
            } else {
                String paramAsString = toString(params[0]);
                result = createURI(url, JSON_PARAMETER_KEY, paramAsString);
            }
        } else {

            String[] paramsAsString = new String[params.length];
            for (int i = 0; i < params.length; i++) {
                paramsAsString[i] = params[i].toString();
            }
            result = createURI(url, paramsAsString);
        }

        return result;
    }

    public static String execute(HttpRequestBase request) {
        CloseableHttpClient httpClient = null;
        CloseableHttpResponse response = null;
        try {
            LOGGER.info("request = " + request);
            httpClient = HttpClientBuilder.create().build();
            response = httpClient.execute(request);

            String body = null;
            HttpEntity rsp_entity = response.getEntity();
            if (rsp_entity != null) {
                body = EntityUtils.toString(rsp_entity, "UTF-8");
            }

            String code = "" + response.getStatusLine().getStatusCode();

            if (code.startsWith("2")) {
                return body;
            } else {
                String message = "response with status = " + code;
                message += "\noriginal request = " + request;
                message += "\n" + body;
                LOGGER.error(message);
                throw new JPSRuntimeException(message);
            }
        } catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e.getMessage(), e);
        } finally {
            try {
                if (httpClient != null) {
                    httpClient.close();
                }
                if (response != null) {
                    response.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
                LOGGER.error(e.getMessage(), e);
            }
        }
    }

    public static HttpGet get(String urlOrPathOrKey, String accept, Object... params) {
        try {
            URI uri = createURIFlex(urlOrPathOrKey, params);
            HttpGet request = new HttpGet(uri);
            if (accept != null) {
                request.setHeader(HttpHeaders.ACCEPT, accept);
            }
            return request;
        } catch (URISyntaxException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    public static HttpPut put(String urlOrPathOrKey, Object body, String contentType, String accept, Object... params) {
        return (HttpPut) putOrPost(new HttpPut(), urlOrPathOrKey, body, contentType, accept, params);
    }

    public static HttpPost post(String urlOrPathOrKey, Object body, String contentType, String accept, Object... params) {
        //request.setHeader(HttpHeaders.CONTENT_TYPE, "application/rdf+xml;charset=UTF-8");
        return (HttpPost) putOrPost(new HttpPost(), urlOrPathOrKey, body, contentType, accept, params);
    }

    public static HttpDelete delete(String urlOrPathOrKey, Object... params) {
        try {
            URI uri = createURIFlex(urlOrPathOrKey, params);
            HttpDelete request = new HttpDelete(uri);
            return request;
        } catch (URISyntaxException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }

    private static HttpEntityEnclosingRequestBase putOrPost(HttpEntityEnclosingRequestBase request,
            String urlOrPathOrKey, Object body, String contentType, String accept, Object... params) {
        try {
            URI uri = createURIFlex(urlOrPathOrKey, params);
            request.setURI(uri);
            if (body != null) {
                String bodyAsString = toString(body);
                StringEntity entity = new StringEntity(bodyAsString);
                request.setEntity(entity);
            }
            if (contentType != null) {
                request.setHeader(HttpHeaders.CONTENT_TYPE, contentType);
            }
            if (accept != null) {
                request.setHeader(HttpHeaders.ACCEPT, accept);
            }

            return request;
        } catch (UnsupportedEncodingException | URISyntaxException e) {
            throw new JPSRuntimeException(e.getMessage(), e);
        }
    }
}
