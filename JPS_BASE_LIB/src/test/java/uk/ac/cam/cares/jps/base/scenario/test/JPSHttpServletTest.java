package uk.ac.cam.cares.jps.base.scenario.test;

import static org.junit.Assert.*;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.junit.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.runner.RunWith;
import org.junit.Before;
import org.junit.After;


import org.json.JSONObject;

import org.junit.runner.RunWith;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.Response;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.atMost;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import org.mockito.runners.MockitoJUnitRunner;

import org.springframework.mock.web.MockHttpServletRequest ;
import org.springframework.mock.web.MockHttpServletResponse ;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.* ;

import java.lang.ClassNotFoundException;
import java.lang.NoSuchMethodException;
import java.lang.IllegalAccessException;
import java.lang.InstantiationException;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;

import org.apache.logging.log4j.Logger;



public class JPSHttpServletTest {

    private MockHttpServletRequest request ;
    private MockHttpServletResponse response;
    private final ByteArrayOutputStream out = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;

    @Before
    public void init() {
        request = new MockHttpServletRequest();
        response = new MockHttpServletResponse();
        System.setOut(new PrintStream(out));
    }

    @BeforeEach
    public void setUp() {

//        request.setServerName("www.example.com");
//        request.setRequestURI("/foo");
//        request.setQueryString("param1=value1");
//        response = new MockHttpServletResponse();
//        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
//        System.setOut(new PrintStream(out));
    }

    @AfterEach
    public void tearDown() {
        out.reset();
    }


    @Test
    public void testdoGet() throws ClassNotFoundException,NoSuchMethodException,
            IllegalAccessException,InvocationTargetException,
            InstantiationException,UnsupportedEncodingException {


        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("doGet",HttpServletRequest.class,HttpServletResponse.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        request.setMethod(HttpGet.METHOD_NAME) ;
        method.invoke(jhs,request,response);
        String pattern1 = "DO GET RESPONSE BODY: 1 ";
        String pattern2 = "HANDLING REQUEST 3: ";
        assertTrue(out.toString().contains(pattern1));
        assertTrue(out.toString().contains(pattern2));
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertFalse(response.getStatus() == Response.Status.SERVICE_UNAVAILABLE.getStatusCode());
    }

    @Test
    public void testdoPost() throws ClassNotFoundException,NoSuchMethodException,
            IllegalAccessException,InvocationTargetException,
            InstantiationException,UnsupportedEncodingException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("doPost",HttpServletRequest.class,HttpServletResponse.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        request.setMethod(HttpPost.METHOD_NAME) ;
        byte[] bytes = "body".getBytes(Charset.defaultCharset());
        request.setContent(bytes);
//        request.addParameter("k1","v1");
        method.invoke(jhs,request,response);
        String pattern1 = "HANDLING REQUEST 2: ";
        String pattern2 = "HANDLING REQUEST 3: ";
        assertTrue(out.toString().contains(pattern1));
        assertFalse(out.toString().contains(pattern2));
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertFalse(response.getStatus() == Response.Status.SERVICE_UNAVAILABLE.getStatusCode());

    }

    @Test
    public void testdoPut() throws ClassNotFoundException,NoSuchMethodException,
            IllegalAccessException,InvocationTargetException,
            InstantiationException,UnsupportedEncodingException {
        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("doPut",HttpServletRequest.class,HttpServletResponse.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        request.setMethod(HttpPut.METHOD_NAME) ;
        byte[] bytes = "body".getBytes(Charset.defaultCharset());
        request.setContent(bytes);
//        request.addParameter("k1","v1");
        method.invoke(jhs,request,response);
        String pattern1 = "HANDLING REQUEST 2: ";
        String pattern2 = "HANDLING REQUEST 3: ";
        assertTrue(out.toString().contains(pattern1));
        assertFalse(out.toString().contains(pattern2));
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertFalse(response.getStatus() == Response.Status.SERVICE_UNAVAILABLE.getStatusCode());


    }

    @Test
    public void testhandleRequest() throws ClassNotFoundException,NoSuchMethodException,
            IllegalAccessException,InvocationTargetException,
            InstantiationException,UnsupportedEncodingException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("handleRequest",HttpServletRequest.class,HttpServletResponse.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        request.setMethod(HttpGet.METHOD_NAME) ;
        method.invoke(jhs,request,response);
        String pattern1 = "HANDLING REQUEST 2: ";
        String pattern2 = "HANDLING REQUEST 3: ";
        assertTrue(out.toString().contains(pattern1));
        assertTrue(out.toString().contains(pattern2));
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertFalse(response.getStatus() == Response.Status.SERVICE_UNAVAILABLE.getStatusCode());

        out.reset();


        request.setMethod(HttpPut.METHOD_NAME) ;
        byte[] bytes = "body".getBytes(Charset.defaultCharset());
        request.setContent(bytes);
        request.addParameter("k1","v1");
        method.invoke(jhs,request,response);
        assertTrue(out.toString().contains(pattern1));
        assertFalse(out.toString().contains(pattern2));
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertTrue(response.getStatus() >= 200 && response.getStatus() <= 299);


    }

    @Test
    public void testdoGetJPS() throws ClassNotFoundException,NoSuchMethodException,
            IllegalAccessException,InvocationTargetException,
            InstantiationException,UnsupportedEncodingException {
        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("doGetJPS",HttpServletRequest.class,HttpServletResponse.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        request.setMethod(HttpGet.METHOD_NAME) ;
        method.invoke(jhs,request,response);
        String pattern = "DO GET JPS: ";
        assertTrue(out.toString().contains(pattern));
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertFalse(response.getStatus() == Response.Status.SERVICE_UNAVAILABLE.getStatusCode());


    }

    @Test
    public void testDoPostJPSwithoutReqBody() throws ClassNotFoundException,NoSuchMethodException,
            IllegalAccessException,InvocationTargetException,
            InstantiationException,UnsupportedEncodingException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("doPostJPS",HttpServletRequest.class,HttpServletResponse.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        request.setMethod(HttpPost.METHOD_NAME) ;
        byte[] bytes = "body".getBytes(Charset.defaultCharset());
        request.setContent(bytes);
        request.addParameter("k1","v1");
        method.invoke(jhs,request,response);
        String pattern = "DO GET RESPONSE BODY: ";
        assertTrue(out.toString().contains(pattern));
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertFalse(response.getStatus() == Response.Status.SERVICE_UNAVAILABLE.getStatusCode());
        assertTrue(response.getStatus() >= 200 && response.getStatus() <= 299);

    }

    @Test
    public void testDoPostJPSwithReqBody() throws ClassNotFoundException,NoSuchMethodException,
            IllegalAccessException,InvocationTargetException,
            InstantiationException,UnsupportedEncodingException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("doPostJPS",HttpServletRequest.class,HttpServletResponse.class, JSONObject.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        request.setMethod(HttpPost.METHOD_NAME) ;
        byte[] bytes = "body".getBytes(Charset.defaultCharset());
        request.setContent(bytes);
        request.addParameter("k1","v1");
        JSONObject json = new JSONObject();
        method.invoke(jhs,request,response,json);
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertTrue(response.getStatus() >= 200 && response.getStatus() <= 299);

    }

    @Test
    public void testdoPutJPS() throws ClassNotFoundException,NoSuchMethodException,
            IllegalAccessException,InvocationTargetException,
            InstantiationException,UnsupportedEncodingException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("doPutJPS",HttpServletRequest.class,HttpServletResponse.class, JSONObject.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        request.setMethod(HttpPut.METHOD_NAME) ;
        byte[] bytes = "body".getBytes(Charset.defaultCharset());
        request.setContent(bytes);
        request.addParameter("k1","v1");
        JSONObject json = new JSONObject();
        method.invoke(jhs,request,response,json);
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertTrue(response.getStatus() >= 200 && response.getStatus() <= 299);


    }

    @Test
    public void testdoHttpJPSwithoutReqBody() throws ClassNotFoundException,NoSuchMethodException,
            IllegalAccessException,InvocationTargetException,
            InstantiationException,UnsupportedEncodingException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("doHttpJPS",HttpServletRequest.class,HttpServletResponse.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        request.setMethod(HttpGet.METHOD_NAME) ;
        byte[] bytes = "body".getBytes(Charset.defaultCharset());
        request.setContent(bytes);
        request.addParameter("k1","v1");
        method.invoke(jhs,request,response);
        String pattern = "DO GET RESPONSE BODY: ";
        assertTrue(out.toString().contains(pattern));
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertFalse(response.getStatus() == Response.Status.SERVICE_UNAVAILABLE.getStatusCode());
        assertTrue(response.getStatus() >= 200 && response.getStatus() <= 299);

    }

    @Test
    public void testdoHttpJPSwithReqBody() throws ClassNotFoundException,NoSuchMethodException,
            IllegalAccessException,InvocationTargetException,
            InstantiationException,UnsupportedEncodingException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("doHttpJPS",HttpServletRequest.class,HttpServletResponse.class,JSONObject.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        request.setMethod(HttpPut.METHOD_NAME) ;
        byte[] bytes = "body".getBytes(Charset.defaultCharset());
        request.setContent(bytes);
        request.addParameter("k1","v1");
        JSONObject json = new JSONObject();
        method.invoke(jhs,request,response,json);
        assertFalse(response.getStatus() == Response.Status.BAD_REQUEST.getStatusCode());
        assertTrue(response.getStatus() >= 200 && response.getStatus() <= 299);


    }
    @Test
    public void randomtest() throws UnsupportedEncodingException {

        String pattern = "DO GET ";
        assertFalse(out.toString().contains(pattern));
        MockHttpServletResponse response2 = new MockHttpServletResponse();
        System.setOut(System.out);
        System.out.println(response2.getStatus());
        assertTrue(response2.getStatus() >= 400 );
    }

    @Test
    public void testsetLogger() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("setLogger") ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);
        method.invoke(jhs);
        Field logger = TargetClass.getDeclaredField("logger") ;
        logger.setAccessible(true);
        Logger log = (Logger) logger.get(jhs);
        assertNotNull(log);
        assertTrue(log.isErrorEnabled());

    }


    @Test
    public void testgetResponseBodywithoutrequestParams() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {
        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("getResponseBody",HttpServletRequest.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);
        String res = (String) method.invoke(jhs,request);
        assertNotNull(res);
        assertTrue(res.length() > 0);

    }

    @Test
    public void testgetResponseBodywithrequestParams() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("getResponseBody",HttpServletRequest.class,JSONObject.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);
        JSONObject json = new JSONObject();
        String res = (String) method.invoke(jhs,request,json);
        System.out.println(res);
        assertNotNull(res);
        assertTrue(res.length() > 0);


    }



    @Test
    public void testProcessRequestParameterswithoutServletRequest() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {
        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("processRequestParameters",JSONObject.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);
        JSONObject json = new JSONObject();
        JSONObject res = (JSONObject) method.invoke(jhs,json);
        assertNotNull(res);
        assertTrue(res.length() == 0);


    }

    @Test
    public void testProcessRequestParameterswithServletRequest() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("processRequestParameters",JSONObject.class,HttpServletRequest.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);
        JSONObject json = new JSONObject();
        JSONObject res = (JSONObject) method.invoke(jhs,json,request);
        assertNotNull(res);
        assertTrue(res.length() == 0);


    }




    @Test
    public void testExecute() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("execute",String.class,String.class,String.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        String path = "HelloServlet-0.0.1-SNAPSHOT/sayhello" ;
        String jsonInput = "json" ;

        String res = (String) method.invoke(jhs,path,jsonInput,HttpGet.METHOD_NAME);
        assertNotNull(res);
        assertTrue(res.length() > 0);

        path = "https://httpbin.org/anything" ;
        res = (String) method.invoke(jhs,path,jsonInput,HttpPost.METHOD_NAME);
        assertNotNull(res);
        assertTrue(res.length() > 0);

        String finalPath = path;
        // It has been verified that the InvocationTargetException is caused by an IllegalStateException thrown by
        // the execute method of the JPSHttpServlet class.
        assertThrows(InvocationTargetException.class,
                ()-> {method.invoke(jhs, finalPath,jsonInput,"invalidMethod") ;}) ;

    }


    @Test
    public void testEnableScenariowithServletRequest() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("enableScenario", HttpServletRequest.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        JSONObject json = (JSONObject) method.invoke(jhs,request);
        assertNotNull(json);
        assertTrue(json.length() > 0);
    }

    @Test
    public void testEnableScenariowithOneString() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("enableScenario", String.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        String scenariourl = "http://localhost:8080" ;
        assertNull(JPSContext.getJpsContext());
        assertNull(JPSContext.get(JPSConstants.SCENARIO_URL)) ;
        JSONObject json = (JSONObject) method.invoke(jhs,scenariourl);
        assertNotNull(JPSContext.getJpsContext());
        assertEquals(JPSContext.get(JPSConstants.SCENARIO_URL),scenariourl) ;

    }

    @Test
    public void testEnableScenariowithTwoStrings() throws ClassNotFoundException, NoSuchMethodException,
            InvocationTargetException, IllegalAccessException {

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("enableScenario", String.class,String.class) ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);
        method.setAccessible(true);

        String scenariourl = "http://localhost:8080" ;
        String usecaseurl = "https://httpbin.org/anything" ;
        assertNull(JPSContext.getJpsContext());
        assertNull(JPSContext.get(JPSConstants.SCENARIO_URL)) ;
        assertNull(JPSContext.get(JPSConstants.SCENARIO_USE_CASE_URL)) ;
        JSONObject json = (JSONObject) method.invoke(jhs,scenariourl,usecaseurl);
        assertNotNull(JPSContext.getJpsContext());
        assertEquals(JPSContext.get(JPSConstants.SCENARIO_URL),scenariourl) ;
        assertEquals(JPSContext.get(JPSConstants.SCENARIO_USE_CASE_URL),usecaseurl) ;


    }



    @Test
    public void testdisableScenario() throws ClassNotFoundException, InvocationTargetException,
            NoSuchMethodException, IllegalAccessException {
        assertNull(JPSContext.getJpsContext());
        assertNull(JPSContext.get(JPSConstants.SCENARIO_URL)) ;
        assertNull(JPSContext.get(JPSConstants.SCENARIO_USE_CASE_URL)) ;
        testEnableScenariowithTwoStrings();
        String scenariourl = "http://localhost:8080" ;
        String usecaseurl = "https://httpbin.org/anything" ;
        assertNotNull(JPSContext.getJpsContext());
        assertEquals(JPSContext.get(JPSConstants.SCENARIO_URL),scenariourl) ;
        assertEquals(JPSContext.get(JPSConstants.SCENARIO_USE_CASE_URL),usecaseurl) ;

        Class<?> TargetClass = Class.forName("uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet");
        Method method = TargetClass.getDeclaredMethod("disableScenario") ;
        JPSHttpServlet jhs = mock(JPSHttpServlet.class,CALLS_REAL_METHODS);

        method.invoke(jhs);
        assertNotNull(JPSContext.getJpsContext());
        assertNull(JPSContext.get(JPSConstants.SCENARIO_URL)) ;
        assertNull(JPSContext.get(JPSConstants.SCENARIO_USE_CASE_URL)) ;


    }
}