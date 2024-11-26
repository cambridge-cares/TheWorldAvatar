package uk.ac.cam.cares.jps.agent.assetmanager;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.entity.ContentType;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.jena.base.Sys;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.isA;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.*;
import org.junit.experimental.runners.Enclosed;
import org.junit.rules.TemporaryFolder;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.semanticweb.owlapi.util.IRIComparator;
import org.apache.commons.io.IOUtils;

import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import wiremock.com.jayway.jsonpath.internal.function.text.Length;
import wiremock.org.eclipse.jetty.util.ajax.JSON;

import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.utility.DockerImageName;

import java.io.*;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;

public class retrieveTest extends EndpointTest {
      /*
     * Functionality tested:
    *       "/retrieve", 
            "/retrievebydocs", 
            "/getuidata", 
            "/instantiate",
            "/addmaintenance",
            "/updatetime",
            "/deletemaintenance",
            "/delete"
        Functionality not tested (relies on other agent / printer):
            "/print", 
            "/printbulk",
            "/addmanual",
            "/addassetimage",
            "/addpurchdocfile"
            
     */

    /*
    ==EXPECTED BEHAVIOUR TESTED==
    Test fail when content is invalid
            - ID is invalid IRI/ID
            - ID does not exist

    //Test succeed
        - ID using assetID or IRI
        - Asset in either lab or office namespace
        - Check response, ensure valid content
        - with/ without timeseries - todo
    */
        
    //read example test data
    JSONObject doesNotExist, invalid;
    JSONObject inLab, justID, justIRI;
    
    @Before
    public void importJSONfiles () throws Exception{
        try {
            doesNotExist = parseJSON(folderReqBody  + "/retrieve/fail/retrieve_doesNotExist.json");
            invalid = parseJSON(folderReqBody  + "/retrieve/fail/retrieve_invalid.json");
            
            justID = parseJSON(folderReqBody  + "/retrieve/success/retrieve_justID.json");
            justIRI = parseJSON(folderReqBody  + "/retrieve/success/retrieve_justIRI.json");
            inLab = parseJSON(folderReqBody  + "/retrieve/success/retrieve_inLab.json");
            
        } catch (Exception e) {
            throw new Exception("Test Failed when importing request body from file:: " + e);
        }
    }

    @Before
    private void createExampleData () throws Exception{
        // Reuse instances from testInstantiate
        JSONObject defaultBody, existingIRI, inLab, outOfCARES;
        try{
            defaultBody = parseJSON(folderReqBody  + "/init/success/init_default.json");
            existingIRI = parseJSON(folderReqBody  + "/init/success/init_existingIRI.json");
            inLab = parseJSON(folderReqBody  + "/init/success/init_inLab.json");
            outOfCARES = parseJSON(folderReqBody  + "/init/success/init_outOfCARES.json");
        }
        catch (Exception e){
            throw new Exception("Test Failed when sample init request body from file:: " + e);
        }
        try {
            //TODO Check if init succeed from returned value. Currently assumes it succeed
            agent.getRequestParameters(defaultBody, "/instantiate");
            agent.getRequestParameters(existingIRI, "/instantiate");
            agent.getRequestParameters(inLab, "/instantiate");
            agent.getRequestParameters(outOfCARES, "/instantiate");
        } catch (Exception e) {
            throw new Exception("Failed to create sample instances:: " + e);
        }
    }

    //Test calling agents
    //@Ignore("Reuiqres docker installed")
    @Test
    public void testRetrieveDoesNotExist () {
        JSONObject result = agent.getRequestParameters(doesNotExist, "/retrieve");
        assertTrue(result.getJSONArray("Result").getString(0).contains("Failed to get ID/IRI."));
    }

    //@Ignore("Reuiqres docker installed")
    @Test
    public void testRetrieveInvalidID () {
        JSONObject result = agent.getRequestParameters(invalid, "/retrieve");
        assertTrue(result.getJSONArray("Result").getString(0).contains("Failed to get ID/IRI."));
    }

    //The following tests are run after the instantiate tests
    //@Ignore("Reuiqres docker installed")
    @Test
    public void testRetrieveJustID () {
        JSONObject result = agent.getRequestParameters(justID, "/retrieve");
        assertTrue(result.getJSONArray("ID").getString(0).equals("2023-10-27/4"));
        assertTrue(result.getJSONArray("Result").length() == 3);
    }

    //@Ignore("Reuiqres docker installed")
    @Test
    public void testRetrieveJustIRI () {
        JSONObject result = agent.getRequestParameters(justIRI, "/retrieve");
        assertTrue(result.getJSONArray("ID").getString(1).equals("https://www.theworldavatar.com/kg/ontodevice/Device_fdaa745d-c447-4a90-ab13-b00e0d95675c"));
        assertTrue(result.getJSONArray("Result").length() == 3);
    }

        //@Ignore("Reuiqres docker installed")
    @Test
    public void TestRetrieveUIData() {
        /*Test succeed
            - Just call it, should work regardless
        */
        JSONObject param = new JSONObject("{\"assetData\": {}}");
        JSONObject result = agent.getRequestParameters(param, "/getuidata");
        assertTrue(result.has("result"));
        String[] keyArr = {"Type", "User", "Invoice", "Element", "Manufacturer", "PurchaseOrder", "Supplier", "DeliveryOrder", "Workspace"};
        for (String key : keyArr) {
            assertTrue(result.getJSONObject("result").has(key));
        }
        
    }
}