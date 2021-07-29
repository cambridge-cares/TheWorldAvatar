package uk.ac.cam.cares.jps.base.http.test;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import org.apache.http.client.methods.HttpDelete;
import org.apache.http.client.methods.HttpEntityEnclosingRequestBase;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.methods.HttpRequestBase;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.http.Http;

public class HttpTest extends TestCase {

	String url = "http://www.example.com/myapp/webapp";
	String urlWithPort = "http://www.example.com:8000/myapp/webapp";

	String scheme = "http";
	String path = "/myapp/webapp";
	int port = 8000;
	String host = "www.example.com";

	String key1 = "key1";
	String value1 = "value1";
	String key2 = "key2";
	String value2 = "value2";

	String queryString = key1 + "=" + value1 + "&" + key2 + "=" + value2;
	String queryJsonString = "{\"key1\":\"value1\",\"key2\":\"value2\"}";

	String body = "Hello World!";

	String urlCRUD = "http://httpbin.org/";
	String accept = "application/json";
	String contentType = "application/x-www-form-urlencoded";

	public void xxxtestGetHostPort() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException, NoSuchFieldException {
		Http http = new Http();
		assertNotNull(http.getClass().getDeclaredMethod("getHostPort"));
		Method getHostPort = http.getClass().getDeclaredMethod("getHostPort");
		assertNotNull(http.getClass().getDeclaredField("hostPort"));
		Field hostPort = http.getClass().getDeclaredField("hostPort");
		getHostPort.setAccessible(true);
		hostPort.setAccessible(true);

		getHostPort.invoke(http);
		assertNotNull(hostPort.get(http));
//		assertTrue(hostPort.get(http).toString()
//				.contains(":80"));
		assertEquals(http.getClass().getDeclaredFields().length, 3);
		assertEquals(http.getClass().getDeclaredMethods().length, 17);

	}

	public void xxxtestGetUriBuilderForPath() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException, NoSuchFieldException, MalformedURLException {
		Http http = new Http();
		assertNotNull(http.getClass().getDeclaredMethod("getUriBuilderForPath", String.class));
		Method getUriBuilderForPath = http.getClass().getDeclaredMethod("getUriBuilderForPath", String.class);
		getUriBuilderForPath.setAccessible(true);
		String path = "/JPS";

		getUriBuilderForPath.invoke(http, path);
		assertNotNull(getUriBuilderForPath.invoke(http, path));
		String url = new URL(scheme, AgentLocator.getProperty("host"),
				Integer.valueOf(AgentLocator.getProperty("port")), path).toString();
		assertEquals(getUriBuilderForPath.invoke(http, path).toString(), url);

	}

	public void testCreateURI() {
		// method might process some invalid urls

		URI uriObject = Http.createURI(url, key1, value1, key2, value2);

		assertEquals(uriObject.getScheme(), scheme);
		assertEquals(uriObject.getPath(), path);
		assertEquals(uriObject.getHost(), host);
		assertEquals(uriObject.getQuery(), queryString);

		uriObject = Http.createURI(urlWithPort);

		assertEquals(uriObject.getScheme(), scheme);
		assertEquals(uriObject.getPath(), path);
		assertEquals(uriObject.getHost(), host);
		assertEquals(uriObject.getPort(), port);
		assertEquals(uriObject.getQuery(), null);

		try {
			Http.createURI("aslkjasd/alskjda");
		} catch (StringIndexOutOfBoundsException e) {
			assertTrue(e instanceof StringIndexOutOfBoundsException);
			assertEquals(e.getMessage(), "String index out of range: -1");
		}

	}

	public void testReadJsonParameter() {

		// create test request object
		MockHttpServletRequest request = new MockHttpServletRequest();
		request.setServerName(host);
		request.setRequestURI(path);
		request.setQueryString(queryString);

		// check that test request object has been set correctly, default port is 80
		String url = request.getRequestURL() + "?" + request.getQueryString();
		assertEquals(url, "http://" + host + ":80" + path + "?" + queryString);

		// parameters have to be set explicitly in the test request object
		request.addParameter(key1, value1);
		request.addParameter(key2, value2);

		// test getParameterNames part
		assertEquals(Http.readJsonParameter(request).toString(), queryJsonString);

		// add query parameter explicitly in wrong format
		request.addParameter("query", "key1=value1&key2=value2");

		// test getParameter("query") part
		try {
			Http.readJsonParameter(request).toString();
		} catch (Exception e) {
			assertTrue(e instanceof JPSRuntimeException);
			assertEquals(e.getMessage(), "A JSONObject text must begin with '{' at 1 [character 2 line 1]");
		}

		// add query parameter explicitly in right format
		request.setParameter("query", queryJsonString);

		// test getParameter("query") part
		assertEquals(Http.readJsonParameter(request).toString(), queryJsonString);

	}

	public void testWriteJsonParameter() throws IOException {

		// create test response object and json object
		MockHttpServletResponse response = new MockHttpServletResponse();
		JSONObject json = new JSONObject(queryJsonString);

		// test function
		Http.writeJsonParameter(response, json);

		if (response.getContentType() != null) {
			assertEquals(response.getContentType(), "application/json");
			assertEquals(response.getContentAsString(), queryJsonString);
		}

	}

	public void testGetRequestBody() {

		// create test request object
		MockHttpServletRequest request = new MockHttpServletRequest();
		request.setServerName(host);
		request.setRequestURI(path);
		request.setQueryString(queryString);
		request.addParameter(key1, value1);
		request.addParameter(key2, value2);
		request.addParameter("query", queryString);

		byte[] content = body.getBytes();

		try {
			Http.getRequestBody(request);
		} catch (Exception e) {
			assertTrue(e instanceof NullPointerException);
			assertEquals(e.getMessage(), null);
		}

		request.setContent(content);

		assertEquals(Http.getRequestBody(request), body);

	}

	public void testPrintToResponse() throws UnsupportedEncodingException {

		// create test response object
		MockHttpServletResponse response = new MockHttpServletResponse();

		Http.printToResponse(null, response);
		assertEquals(response.getContentAsString(), "");
		assertEquals(response.getContentType(), null);
		assertEquals(response.getCharacterEncoding(), "ISO-8859-1");

		Http.printToResponse(body, response);
		assertEquals(response.getContentAsString(), body);
		assertEquals(response.getContentType(), "text/plain");
		assertEquals(response.getCharacterEncoding(), "UTF-8");

	}

	public void testToString() throws NoSuchMethodException, SecurityException, IllegalAccessException,
			IllegalArgumentException, InvocationTargetException {
		Http http = new Http();
		assertNotNull(http.getClass().getDeclaredMethod("toString", Object.class));
		Method toString = http.getClass().getDeclaredMethod("toString", Object.class);
		toString.setAccessible(true);
		String str = null;
		JSONObject jo = null;
		int n = 123;

		assertEquals(toString.invoke(http, str), "null");
		assertEquals(toString.invoke(http, jo), "null");
		assertEquals(toString.invoke(http, n), "123");

		str = queryJsonString;
		assertEquals(toString.invoke(http, str), "{\"key1\":\"value1\",\"key2\":\"value2\"}");
		jo = new JSONObject(str);
		assertEquals(toString.invoke(http, jo), "{\"key1\":\"value1\",\"key2\":\"value2\"}");

	}

	public void testEncodePercentage() throws NoSuchMethodException, SecurityException {
		Http http = new Http();
		assertNotNull(http.getClass().getDeclaredMethod("encodePercentage", String.class));
		String encode = Http.encodePercentage(queryString);

		assertEquals(encode, "key1%3Dvalue1%26key2%3Dvalue2");

	}

	public void testDecodePercentage() throws NoSuchMethodException, SecurityException {
		Http http = new Http();
		assertNotNull(http.getClass().getDeclaredMethod("decodePercentage", String.class));
		String encode = "key1%3Dvalue1%26key2%3Dvalue2";
		String decode = Http.decodePercentage(encode);

		assertEquals(decode, queryString);

	}

	public void xxxtestCreateURIFlex() throws URISyntaxException {

		// case 1
		URI uriObject = Http.createURIFlex(url, key1, value1, key2, value2);

		assertEquals(uriObject.getScheme(), scheme);
		assertEquals(uriObject.getPath(), path);
		assertEquals(uriObject.getHost(), host);
		assertEquals(uriObject.getQuery(), queryString);

		// case 2
		String path = "/myapp/webapp";
		uriObject = Http.createURIFlex(path, key1, value1, key2, value2);

		assertEquals(uriObject.getScheme(), scheme);
		assertEquals(uriObject.getPath(), path);
		assertEquals(uriObject.getHost(), "localhost");
		assertEquals(uriObject.getQuery(), queryString);

		// case 3 relies on KeyValueManager
//		String key = "dataset.airquality.url";
//		uriObject = Http.createURIFlex(key);
//		
//		assertEquals(uriObject.getScheme(), scheme);
//		assertEquals(uriObject.getPath(), "/jps/data/airquality");
//		assertEquals(uriObject.getHost(), "localhost");
//		assertEquals(uriObject.getQuery(), null);

	}

	public void testExecute() throws NoSuchMethodException, SecurityException, MalformedURLException {
		Http http = new Http();
		assertNotNull(http.getClass().getDeclaredMethod("execute", HttpRequestBase.class));
		HttpRequestBase hrb = null;

		try {
			Http.execute(hrb);
		} catch (Exception e) {
			assertEquals(e.getMessage(), "HTTP request may not be null");
		}
		
		try {
			hrb = new HttpGet(url);
			Http.execute(hrb);
		} catch (Exception e) {
			assertTrue(e.getMessage().contains("response with status = 404"));
		}
		
		hrb = Http.get(urlCRUD + "get", accept);
		Http.execute(hrb);
		assertNotNull(Http.execute(hrb));
		JSONObject jo = new JSONObject(Http.execute(hrb));
		assertEquals(jo.get("url"), "http://httpbin.org/get");

	}

	public void testGet() {

		HttpGet request = Http.get(urlCRUD + "get", accept);
		
		assertEquals(request.toString().substring(0, 3), "GET");

		JSONObject jo = new JSONObject(Http.execute(request));
		JSONObject headers = (JSONObject) jo.get("headers");

		assertEquals(headers.get("Accept"), accept);
		assertEquals(headers.get("Host"), "httpbin.org");

		request = Http.get(urlCRUD + "get", null);
		jo = new JSONObject(Http.execute(request));
		headers = (JSONObject) jo.get("headers");

		try {
			headers.get("Accept");
		} catch (Exception e) {
			assertTrue(e instanceof JSONException);
			assertEquals(e.getMessage(), "JSONObject[\"Accept\"] not found.");
		}
		assertEquals(headers.get("Host"), "httpbin.org");

	}

	public void testPut() {

		HttpPut request = Http.put(urlCRUD + "put", queryString, contentType, accept, new TestObject());

		assertEquals(request.toString().substring(0, 3), "PUT");

		JSONObject jo = new JSONObject(Http.execute(request));
		JSONObject headers = (JSONObject) jo.get("headers");

		assertEquals(jo.get("form").toString(), queryJsonString);
		assertEquals(headers.get("Accept"), accept);

	}

	public void testPost() {

		JSONObject params = new JSONObject(queryJsonString);

		HttpPost request = Http.post(urlCRUD + "post", queryString, contentType, accept, params);

		assertEquals(request.toString().substring(0, 4), "POST");

		JSONObject jo = new JSONObject(Http.execute(request));
		JSONObject headers = (JSONObject) jo.get("headers");

		assertEquals(jo.get("form").toString(), queryJsonString);
		assertEquals(headers.get("Accept"), accept);

	}

	public void testDelete() {
		HttpDelete request = Http.delete(urlCRUD + "delete", (Object) null);

		assertEquals(request.toString().substring(0, 6), "DELETE");
	}

	public void testPutOrPost() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		Http http = new Http();
		assertNotNull(http.getClass().getDeclaredMethod("putOrPost", HttpEntityEnclosingRequestBase.class, String.class,
				Object.class, String.class, String.class, Object[].class));
		Method putOrPost = http.getClass().getDeclaredMethod("putOrPost", HttpEntityEnclosingRequestBase.class, String.class,
				Object.class, String.class, String.class, Object[].class);
		putOrPost.setAccessible(true);
		String testUrl = urlCRUD;
		String body = null;
		String contentType = null;
		String accept = null;
		Object obj = null;
		HttpEntityEnclosingRequestBase heerb = Http.put(testUrl + "put", body, contentType, accept, obj);
		
		heerb = Http.put(testUrl + "put", body, contentType, accept, obj);
		String putRequest = "PUT http://httpbin.org/ HTTP/1.1";
		assertEquals(putOrPost.invoke(http, heerb, testUrl, body, contentType, accept, new Object[] {obj}).toString(), putRequest);
		
		JSONObject params = new JSONObject(queryJsonString);
		heerb = Http.post(urlCRUD + "post", queryString, contentType, accept, params);
		String postRequest = "POST http://httpbin.org/ HTTP/1.1";
		assertEquals(putOrPost.invoke(http, heerb, testUrl, body, contentType, accept, new Object[] {obj}).toString(), postRequest);
		
		body = this.body;
		contentType = this.contentType;
		accept = this.accept;
		obj = new TestObject();
		heerb = Http.put(testUrl + "put", body, contentType, accept, obj);
		putRequest = "PUT http://httpbin.org/?query=%7B%22key%22%3A%22key_0%22%2C%22value%22%3A%22value_0%22%7D HTTP/1.1";
		assertEquals(putOrPost.invoke(http, heerb, testUrl, body, contentType, accept, new Object[] {obj}).toString(), putRequest);
		heerb = Http.post(urlCRUD + "post", queryString, contentType, accept, params);
		postRequest = "POST http://httpbin.org/?query=%7B%22key%22%3A%22key_0%22%2C%22value%22%3A%22value_0%22%7D HTTP/1.1";
		assertEquals(putOrPost.invoke(http, heerb, testUrl, body, contentType, accept, new Object[] {obj}).toString(), postRequest);
		
		try {
			String key = "host";
			heerb = Http.put(key, body, contentType, accept, obj);
			putOrPost.invoke(obj, http, heerb, testUrl, body, contentType, accept, new Object[] {obj});
		} catch (Exception e) {
			assertEquals(e.getMessage(), "String index out of range: -1");
		}
	
	}

}
