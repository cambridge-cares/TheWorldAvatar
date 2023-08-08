package uk.ac.cam.cares.jps.base.query.test;

import org.json.JSONObject;
import org.junit.Test;
import org.junit.jupiter.api.BeforeAll;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.JPSConstants;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.query.RDBAccessAgentCaller;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;

import static org.junit.jupiter.api.Assertions.*;

public class RDBAccessAgentCallerTest {

    @BeforeAll
    static void setAccessAgentHostUrl(){
        //set default accessagent_host to value in jps.properties
        RDBAccessAgentCaller.accessAgentHost = KeyValueMap.getInstance().get(IKeys.URL_ACCESSAGENT_HOST);
    }

    @Test
    public void testGetBaseWorldUrl() {

        String url;
        String result;
        String expected = "http://"+KeyValueMap.getInstance().get(IKeys.URL_ACCESSAGENT_HOST)+ JPSConstants.RDB_ACCESS_AGENT_PATH;
        String expectedLocal = "http://localhost:48888"+JPSConstants.RDB_ACCESS_AGENT_PATH;

        url = "http://www.theworldavatar.com:83/kb/agents/Service__OpenWeatherMap.owl%23Service";
        result = RDBAccessAgentCaller.getBaseWorldUrlforRDBAccess(url);
        assertEquals(expected,result);

        url = "http://www.theworldavatar.com:83/kb/test";
        result = RDBAccessAgentCaller.getBaseWorldUrlforRDBAccess(url);
        assertEquals(expected,result);

        url = "http://localhost:48888/kb/test";
        result = RDBAccessAgentCaller.getBaseWorldUrlforRDBAccess(url);
        assertEquals(expectedLocal,result);

        url = "test";
        result = RDBAccessAgentCaller.getBaseWorldUrlforRDBAccess(url);
        assertEquals(expected,result);
    }

    @Test
    public void testCreateRDBAccessRequestUrl() throws UnsupportedEncodingException, URISyntaxException {

        String expectedPath = JPSConstants.RDB_ACCESS_AGENT_PATH;
        String expectedRequestUrl = "http://"+KeyValueMap.getInstance().get(IKeys.URL_ACCESSAGENT_HOST)+expectedPath;
        String expectedLocalRequestUrl = "http://localhost:48888"+expectedPath;

        //target url with default host
        String targetUrl = "http://www.theworldavatar.com:83/kb/test";

        Object[] result = RDBAccessAgentCaller.createRDBAccessRequestUrl(targetUrl);
        assertNotNull(result);

        String requestUrl = (String) result[0];
        assertEquals(expectedRequestUrl,requestUrl);
        URI uri = new URI(URLDecoder.decode(requestUrl,"UTF-8"));
        assertEquals(expectedPath,uri.getPath());

        JSONObject joparams = (JSONObject) result[1];
        assertEquals(targetUrl,joparams.getString(JPSConstants.TARGETIRI));

        //////////////////////
        //targetUrl is a namespace
        targetUrl = "test";

        result =  RDBAccessAgentCaller.createRDBAccessRequestUrl(targetUrl);
        assertNotNull(result);

        requestUrl = (String) result[0];
        assertEquals(expectedRequestUrl,requestUrl);
        uri = new URI(URLDecoder.decode(requestUrl,"UTF-8"));
        assertEquals(expectedPath,uri.getPath());

        joparams = (JSONObject) result[1];
        assertEquals(targetUrl,joparams.getString(JPSConstants.TARGETIRI));

        //////////////////////
        //local target url
        targetUrl = "http://localhost:48888/kb/test";

        result = RDBAccessAgentCaller.createRDBAccessRequestUrl(targetUrl);
        assertNotNull(result);

        requestUrl = (String) result[0];
        assertEquals(expectedLocalRequestUrl, requestUrl);
        uri = new URI(URLDecoder.decode(requestUrl, "UTF-8"));
        assertEquals(expectedPath, uri.getPath());

        joparams = (JSONObject) result[1];
        assertEquals(targetUrl, joparams.getString(JPSConstants.TARGETIRI));
    }
}
