package uk.ac.cam.cares.jps.accessagent.test;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.jupiter.api.Assertions;
import org.mockito.Mockito;
import org.springframework.mock.web.MockHttpServletRequest;
import uk.ac.cam.cares.jps.accessagent.RDBAccessAgent;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RDBStoreRouter;

import javax.ws.rs.BadRequestException;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

public class RDBAccessAgentTest {

    /** assert that RDBANew is created
     */
    @Test
    public void testNewRDBAgent() {
        RDBAccessAgent rdbAccessAgent = null;
        try {
            rdbAccessAgent = new RDBAccessAgent();
        } finally {
            assertNotNull(rdbAccessAgent);
        }
    }

    @Test
    public void testProcessRequestParameters() {

        RDBAccessAgent agent = Mockito.spy(RDBAccessAgent.class);
        Mockito.doReturn(true).when(agent).validateInput(any(JSONObject.class));
        Mockito.doReturn(null).when(agent).performGet(any(JSONObject.class));

        JSONObject requestParams;

        MockHttpServletRequest request = new MockHttpServletRequest();
        request.setServletPath(RDBAccessAgent.ACCESS_RDB_URL);

        //test http get
        requestParams = new JSONObject();
        requestParams.put(JPSConstants.METHOD, HttpGet.METHOD_NAME);
        agent.processRequestParameters(requestParams, request);
        verify(agent).validateInput(requestParams);
        verify(agent).performGet(requestParams);
    }

    @Test
    public void testProcessRequestParametersClearCache() {

        RDBAccessAgent agent = new RDBAccessAgent();

        MockHttpServletRequest request = new MockHttpServletRequest();
        request.setServletPath(RDBAccessAgent.CLEAR_CACHE_URL);

        JSONObject requestParams;
        requestParams = new JSONObject();
        requestParams.put(JPSConstants.METHOD, HttpGet.METHOD_NAME);

        JSONObject response;
        response = agent.processRequestParameters(requestParams, request);
        assertEquals( "Cache cleared.", response.getString(JPSConstants.RESULT_KEY));
        assertTrue(RDBStoreRouter.getInstance().isCacheEmpty());
    }

    @Test
    public void testProcessRequestParametersClearCacheThrows() {

        RDBAccessAgent agent = new RDBAccessAgent();

        MockHttpServletRequest request = new MockHttpServletRequest();
        request.setMethod(HttpPost.METHOD_NAME);
        request.setServletPath(RDBAccessAgent.CLEAR_CACHE_URL);

        //Http post throws JPSRuntimeException
        JSONObject requestParams;
        requestParams = new JSONObject();
        requestParams.put(JPSConstants.METHOD, HttpPost.METHOD_NAME);

        Assertions.assertThrows(JPSRuntimeException.class, ()->{agent.processRequestParameters(requestParams, request);});
    }

    @Test
    public void testClearCache() {
        RDBAccessAgent agent = new RDBAccessAgent();
        JSONObject response = agent.clearCache();
        assertEquals( "Cache cleared.", response.getString(JPSConstants.RESULT_KEY));
        assertTrue(RDBStoreRouter.getInstance().isCacheEmpty());
    }

    @Test
    public void testValidateInput() throws JSONException {
        JSONObject jo = new JSONObject()
                .put(JPSConstants.TARGETIRI,  "http://www.example.com/test")
                .put(JPSConstants.REQUESTURL, "http://www.example.com/access-agent/rdbaccess")
                .put(JPSConstants.METHOD, "GET");

        // Empty request params
        RDBAccessAgent agent = new RDBAccessAgent();
        assertThrows(BadRequestException.class, () -> agent.validateInput(new JSONObject()));

        // valid input
        assertTrue(agent.validateInput(jo));

        // Invalid method
        jo.put(JPSConstants.METHOD , "POST");
        assertFalse(agent.validateInput(jo));

        // No targetIRI
        jo.put(JPSConstants.METHOD, "GET");
        jo.remove(JPSConstants.TARGETIRI );
        assertFalse(agent.validateInput(jo));
    }

    @Test
    public void testPerformGet() {

        String url = "jdbc:postgresql://localhost:5432/";
        RDBAccessAgent agent = Mockito.spy(RDBAccessAgent.class);
        Mockito.doReturn(url).when(agent).getRDBUrl(any(String.class));

        JSONObject jo = new JSONObject();
        jo.put(JPSConstants.REQUESTURL, "http://www.example.com/access-agent/rdbaccess")
                .put(JPSConstants.METHOD, "GET")
                .put(JPSConstants.TARGETIRI,  "http://www.example.com/test");

        String result = agent.performGet(jo);

        assertEquals(url,result);
    }

}
