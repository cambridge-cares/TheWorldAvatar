package uk.ac.cam.cares.jps.base.query.test;

import org.apache.http.*;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.LaxRedirectStrategy;
import org.apache.http.message.BasicStatusLine;
import org.apache.http.util.EntityUtils;
import org.json.CDL;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService;

import java.lang.reflect.Field;
import java.net.URI;


public class SparqlOverHttpServiceTest {
    private SparqlOverHttpService testS;
    private String sparql = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
    private String queryUrl = "http://test.com/sparql/query";
    private String updateUrl = "http://test.com/sparql/update";

    @Test
    public void testToString() {

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, queryUrl, updateUrl);
        String assertStr = "SparqlOverHttpService[type=RDF4J, " +
                "query url=http://test.com/sparql/query, " +
                "update url=http://test.com/sparql/update";

        Assert.assertEquals(assertStr, testS.toString());
    }

    @Test
    public void testExecutePost() throws Exception{

        // Test with RDFStoreType Blazegraph
        try (MockedConstruction<RemoteStoreClient> mocked = Mockito.mockConstruction(RemoteStoreClient.class,
                (mock, context) -> {
                    Mockito.when(mock.executeUpdate()).thenReturn(1);
                })) {
            // Make the kbClient accessible
            Field kbClientField = SparqlOverHttpService.class.getDeclaredField("kbClient");
            kbClientField.setAccessible(true);
            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.BLAZEGRAPH, queryUrl, updateUrl);
            // Verify that the kbClient is not set
            Assert.assertNull(kbClientField.get(testS));
            // Verify output of the executePost
            Assert.assertEquals("1", testS.executePost(formInsertQuery()));
            // kbClient should now be set with a mock and the setUpdateEndpoint should have been invoked
            Mockito.verify((StoreClientInterface) kbClientField.get(testS)).setUpdateEndpoint(updateUrl);
            // The setQuery should have been invoked as well with the provided query
            Mockito.verify((StoreClientInterface) kbClientField.get(testS)).setQuery(formInsertQuery());
        }

        // Test with RDFStoreType Fuseki

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.FUSEKI, queryUrl, updateUrl);
        // Set-up dummy post response
        CloseableHttpResponse dummyResponse = Mockito.mock(CloseableHttpResponse.class, Mockito.RETURNS_DEEP_STUBS);
        Mockito.when(dummyResponse.getEntity()).thenReturn(new StringEntity("test response"));
        Mockito.when(dummyResponse.getStatusLine().getStatusCode()).thenReturn(204, 500);
        try (MockedStatic<HttpClientBuilder> httpClientB = Mockito.mockStatic(HttpClientBuilder.class, Mockito.RETURNS_DEEP_STUBS)) {
            httpClientB.when(() -> HttpClientBuilder.create().build().execute(Mockito.any(HttpPost.class))).thenReturn(dummyResponse);
            Assert.assertEquals("test response", testS.executePost(formInsertQuery()));
            // Run the method again, this time there should be an exception as the status code is 500
            StatusLine dummyStatus = new BasicStatusLine(new ProtocolVersion("HTTP", 1, 1), 500, "error");
            Mockito.when(dummyResponse.getStatusLine()).thenReturn(dummyStatus);
            Mockito.when(dummyResponse.getAllHeaders()).thenReturn(new Header[] {});
            try {
                testS.executePost(formInsertQuery());
                Assert.fail();
            }
            catch (JPSRuntimeException e) {
                Assert.assertEquals("HTTP response with error = " + dummyStatus + "\n" + "test response", e.getMessage());
            }
        }

        // Test with RDFStoreType RDF4J

    }

    @Test
    public void testExecuteGet() throws Exception{
        HttpResponse expected;
        URI uri = null;
        HttpGet request = new HttpGet(uri);

        JSONArray jsonArray = new JSONArray();
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("count", "1");
        jsonArray.put(jsonObject);
        try (MockedConstruction<RemoteStoreClient> mocked = Mockito.mockConstruction(RemoteStoreClient.class,
                (mock, context) -> {
                    // further stubbings ...
                   Mockito.when(mock.executeQuery()).thenReturn(jsonArray);
                })) {
            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.BLAZEGRAPH, queryUrl, updateUrl);
            Assert.assertEquals(CDL.toString(jsonArray), testS.executeGet(sparql));
        }

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, queryUrl, updateUrl);
        uri = AgentCaller.createURI(queryUrl, "query", queryUrl, "Accept", MediaType.TEXT_CSV.type);
        request = new HttpGet(uri);
        request.setHeader(HttpHeaders.ACCEPT, MediaType.TEXT_CSV.type);
        expected = HttpClientBuilder.create().build().execute(request);
        Assert.assertEquals(testS.executeGet(sparql),  EntityUtils.toString(expected.getEntity()));

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.FUSEKI, queryUrl, updateUrl);
        uri = AgentCaller.createURI(queryUrl, "query", queryUrl);
        request = new HttpGet(uri);
        request.setHeader(HttpHeaders.ACCEPT, MediaType.TEXT_CSV.type);
        expected = HttpClientBuilder.create().build().execute(request);
        Assert.assertEquals(testS.executeGet(sparql),  EntityUtils.toString(expected.getEntity()));

    }

    private static String formInsertQuery(){
        String query = "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>\n";
        query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
        query = query.concat("INSERT DATA { <http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl#ArrheniusCoefficient_182161099217501> ontokin:hasTemperatureExponent \"-0.7\" }");
        return query;
    }


}