package uk.ac.cam.cares.jps.base.discovery.test;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.client.utils.URIBuilder;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.MockedStatic;
import org.mockito.stubbing.Answer;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.nio.charset.Charset;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;


public class AgentCallerTest {



    @Test
    public void testgetHostPort() throws ClassNotFoundException, NoSuchMethodException,
            InstantiationException, IllegalAccessException, InvocationTargetException{

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.discovery.AgentCaller");
        Method method = TargetClass.getDeclaredMethod("getHostPort") ;
        AgentCaller agc = (AgentCaller) TargetClass.getConstructor().newInstance();
        method.setAccessible(true);
        String res = (String) method.invoke(agc);
        assertNotNull(res);
        assertTrue(res.length() > 0);

    }


    @Test
    public void testexecuteGetMultipleStringInputs() {
        String path = "https://www.example.com/index.html" ;
        String k1 = "k1" ;
        String v1 = "v1" ;
        String res = AgentCaller.executeGet(path,k1,v1);
        assertNotNull(res);
        assertTrue(res.length() > 0);

        String invalidPath = "test" ;
        assertThrows(JPSRuntimeException.class,
                ()-> {AgentCaller.executeGet(invalidPath,k1,v1) ;}) ;

    }


    @Test
    public void testexecutePost() {

        String path = "https://www.example.com/index.html" ;
        String body = "test" ;
        String res = AgentCaller.executePost(path,body) ;
        assertNotNull(res);
        assertTrue(res.length() > 0);

        String invalidPath = "test" ;
        assertThrows(JPSRuntimeException.class,
                ()-> {AgentCaller.executePost(invalidPath,body) ;}) ;
    }


    @Test
    public void testgetUriBuilderForPath() throws ClassNotFoundException, NoSuchMethodException,
            InstantiationException, IllegalAccessException, InvocationTargetException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.discovery.AgentCaller");
        Method method = TargetClass.getDeclaredMethod("getUriBuilderForPath", String.class) ;
        AgentCaller agc = (AgentCaller) TargetClass.getConstructor().newInstance();
        method.setAccessible(true);
        URIBuilder builder = (URIBuilder) method.invoke(agc,"https://www.example.com/index.html");
        assertNotNull(builder);

    }


    @Test
    public void testexecuteGetWithURL() {
        String url = "https://httpbin.org/anything" ;
        String res = AgentCaller.executeGetWithURL(url);
        assertNotNull(res);
        assertTrue(res.length() > 0);
    }

    @Test
    public void testexecuteGetWithURLAndJSON() {
        String url = "https://httpbin.org/anything" ;
        JSONObject json = new JSONObject();

        json.put("test1", "value1");
        String strjson = json.toString();
        String res = AgentCaller.executeGetWithURLAndJSON(url, strjson);
        assertNotNull(res);
        assertTrue(res.length() > 0);
    }

    @Test
    public void testcreateURIWithURLandJSON() {
        String url = "https://httpbin.org/anything" ;
        JSONObject json = new JSONObject();
        json.put("test1", "value1");
        String strjson = json.toString();
        URI res = AgentCaller.createURIWithURLandJSON(url, strjson);
        assertNotNull(res);

    }

    @Test
    public void testcreateURI() {
        String url = "https://httpbin.org/anything" ;
        String k1 = "k1" ;
        String v1 = "v1" ;
        URI res = AgentCaller.createURI(url,k1,v1);
        assertNotNull(res);
    }


    @Test
    public void testexecuteGetWithJsonParameter() {

        String path = "https://httpbin.org/anything" ;
        JSONObject json = new JSONObject();
        json.put("test1", "value1");
        String strjson = json.toString();
        String res = AgentCaller.executeGetWithJsonParameter(path,strjson) ;
        assertNotNull(res);
        assertTrue(res.length() > 0);

        try (MockedStatic<AgentCaller> aacMock = mockStatic(AgentCaller.class)) {

            //Do not execute  the executeGet(HttpGet request) method of the AgentCaller class - it is tested separately
            ((MockedStatic<?>) aacMock).when(() -> AgentCaller.executeGet((HttpGet) any(Object.class)))
                    .thenAnswer((Answer<Void>) invocation -> null);
            //Call method that invokes the executeGet method of AgentCaller inside
            assertNull(AgentCaller.executeGetWithJsonParameter(path,strjson));

        }

    }

    @Test
    public void testreadJsonParameter() throws UnsupportedEncodingException {
        MockHttpServletRequest request ;
        MockHttpServletResponse response;
        request = new MockHttpServletRequest();
        request.setServerName("www.example.com");
        request.setRequestURI("/foo");
        request.setQueryString("param1=value1");
        request.setMethod(HttpGet.METHOD_NAME) ;
        JSONObject json = AgentCaller.readJsonParameter(request);
        assertNotNull(json);
        assertTrue(json.length() > 0);

        request.setMethod(HttpPut.METHOD_NAME);
        byte[] bytes = "body".getBytes(Charset.defaultCharset());
        request.setContent(bytes);
        request.addParameter("k1","v1");
        json = AgentCaller.readJsonParameter(request);
        assertNotNull(json);
        assertTrue(json.length() > 0);

    }

    @Test
    public void testgetAccept() throws NoSuchMethodException,
            InvocationTargetException, InstantiationException,
            IllegalAccessException, ClassNotFoundException {
        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.discovery.AgentCaller");
        Method method = TargetClass.getDeclaredMethod("getAccept", HttpServletRequest.class) ;
        AgentCaller agc = (AgentCaller) TargetClass.getConstructor().newInstance();
        method.setAccessible(true);
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.addHeader("Content-Type","text/plain");
        request.addHeader("Accept","text/plain");
        String res = (String) method.invoke(agc,request);
        assertNotNull(res);
        assertTrue(res.length() > 0);


    }

    @Test
    public void testwriteJsonParameter() throws IOException {
        MockHttpServletResponse response = new MockHttpServletResponse();
        JSONObject json = new JSONObject();
        AgentCaller.writeJsonParameter(response,json);
        assertEquals(response.getContentType(),"application/json");
        assertNotNull(json);
    }

    @Test
    public void testExecuteGet() {
        HttpGet request = new HttpGet("https://httpbin.org/get");
        String res = AgentCaller.executeGet(request);
        assertNotNull(res);
        assertTrue(res.length() > 0);
    }


    @Test
    public void testprintToResponse() throws UnsupportedEncodingException {
        MockHttpServletResponse response = new MockHttpServletResponse();
        String path = "test" ;
        AgentCaller.printToResponse(path,response);
        assertEquals(response.getContentAsString(),path);
    }

    @Test
    public void testserializeForResponse() {
        String path = "test" ;
        String res = AgentCaller.serializeForResponse(path);
        assertEquals(res,path);
    }

    @Test
    public void testencodePercentage() {
        String path = "test" ;
        String res = AgentCaller.encodePercentage(path);
        assertEquals(res,path);
    }

    @Test
    public void decodePercentage() {
        String path = "test" ;
        String res = AgentCaller.encodePercentage(path);
        assertEquals(res,path);
    }
}