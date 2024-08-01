package uk.ac.cam.cares.jps.base.query;

import java.lang.reflect.Field;

import org.apache.http.Header;
import org.apache.http.ProtocolVersion;
import org.apache.http.StatusLine;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.message.BasicStatusLine;
import org.json.CDL;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;


public class SparqlOverHttpServiceTest {
    private SparqlOverHttpService testS;
    private String sparql = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
    private String queryUrl = "http://test.com/sparql/query";
    private String updateUrl = "http://test.com/sparql/update";

    @Test
    public void testInit() throws Exception{
        Field testType = SparqlOverHttpService.class.getDeclaredField("type");
        testType.setAccessible(true);
        Field testQuery = SparqlOverHttpService.class.getDeclaredField("sparqlServiceURIForQuery");
        testQuery.setAccessible(true);
        Field testUpdate = SparqlOverHttpService.class.getDeclaredField("sparqlServiceURIForUpdate");
        testUpdate.setAccessible(true);

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, "http://test.com/sparql");
        Assert.assertEquals(testQuery.get(testS), "http://test.com/sparql");
        Assert.assertEquals(testUpdate.get(testS), "http://test.com/sparql/statements");
        Assert.assertEquals(testType.get(testS).toString(), "RDF4J");

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.FUSEKI, "http://test.com/sparql");
        Assert.assertEquals(testQuery.get(testS), "http://test.com/sparql/query");
        Assert.assertEquals(testUpdate.get(testS), "http://test.com/sparql/update");
        Assert.assertEquals(testType.get(testS).toString(), "FUSEKI");

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.BLAZEGRAPH, "http://test.com/sparql");
        Assert.assertEquals(testQuery.get(testS), "http://test.com/sparql");
        Assert.assertEquals(testUpdate.get(testS), "http://test.com/sparql/update");
        Assert.assertEquals(testType.get(testS).toString(), "BLAZEGRAPH");

    }

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
            Mockito.verify((TripleStoreClientInterface) kbClientField.get(testS)).setUpdateEndpoint(updateUrl);
            // The setQuery should have been invoked as well with the provided query
            Mockito.verify((TripleStoreClientInterface) kbClientField.get(testS)).setQuery(formInsertQuery());
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
        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, queryUrl, updateUrl);
        // Set-up dummy post response
        CloseableHttpResponse dummyResponse2 = Mockito.mock(CloseableHttpResponse.class, Mockito.RETURNS_DEEP_STUBS);
        Mockito.when(dummyResponse2.getEntity()).thenReturn(new StringEntity("test response"));
        Mockito.when(dummyResponse2.getStatusLine().getStatusCode()).thenReturn(204, 500);
        try (MockedStatic<HttpClientBuilder> httpClientB = Mockito.mockStatic(HttpClientBuilder.class, Mockito.RETURNS_DEEP_STUBS)) {
            httpClientB.when(() -> HttpClientBuilder.create().build().execute(Mockito.any(HttpPost.class))).thenReturn(dummyResponse2);
            Assert.assertEquals("test response", testS.executePost(formInsertQuery()));
            // Run the method again, this time there should be an exception as the status code is 500
            StatusLine dummyStatus2 = new BasicStatusLine(new ProtocolVersion("HTTP", 1, 1), 500, "error");
            Mockito.when(dummyResponse2.getStatusLine()).thenReturn(dummyStatus2);
            Mockito.when(dummyResponse2.getAllHeaders()).thenReturn(new Header[] {});
            try {
                testS.executePost(formInsertQuery());
                Assert.fail();
            }
            catch (JPSRuntimeException e) {
                Assert.assertEquals("HTTP response with error = " + dummyStatus2 + "\n" + "test response", e.getMessage());
            }
        }
    }

    @Test
    public void testExecuteGet() throws Exception{

        // Test with RDFStoreType Blazegraph
        JSONArray jsonArray = new JSONArray();
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("count", "1");
        jsonArray.put(jsonObject);
        try (MockedConstruction<RemoteStoreClient> mocked = Mockito.mockConstruction(RemoteStoreClient.class,
                (mock, context) -> {
                   Mockito.when(mock.executeQuery()).thenReturn(jsonArray);
                })) {
            Field kbClientField = SparqlOverHttpService.class.getDeclaredField("kbClient");
            kbClientField.setAccessible(true);
            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.BLAZEGRAPH, queryUrl, updateUrl);
            Assert.assertNull(kbClientField.get(testS));
            Assert.assertEquals(CDL.toString(jsonArray), testS.executeGet(sparql));
            Mockito.verify((TripleStoreClientInterface) kbClientField.get(testS)).setQueryEndpoint(queryUrl);
            Mockito.verify((TripleStoreClientInterface) kbClientField.get(testS)).setQuery(sparql);
        }

        // Test with RDFStoreType RDF4J
        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, queryUrl, updateUrl);
        // Set-up dummy post response
        CloseableHttpResponse dummyResponse2 = Mockito.mock(CloseableHttpResponse.class, Mockito.RETURNS_DEEP_STUBS);
        Mockito.when(dummyResponse2.getEntity()).thenReturn(new StringEntity("test response"));
        Mockito.when(dummyResponse2.getStatusLine().getStatusCode()).thenReturn(200,500);
        try (MockedStatic<HttpClientBuilder> httpClientB = Mockito.mockStatic(HttpClientBuilder.class, Mockito.RETURNS_DEEP_STUBS)) {
            httpClientB.when(() -> HttpClientBuilder.create().build().execute(Mockito.any(HttpGet.class))).thenReturn(dummyResponse2);
            Assert.assertEquals("test response", testS.executeGet(sparql));
            // Run the method again, this time there should be an exception as the status code is 500
            StatusLine dummyStatus2 = new BasicStatusLine(new ProtocolVersion("HTTP", 1, 1), 500, "error");
            Mockito.when(dummyResponse2.getStatusLine()).thenReturn(dummyStatus2);
            Mockito.when(dummyResponse2.getAllHeaders()).thenReturn(new Header[] {});
            try {
                testS.executeGet(sparql);
                Assert.fail();
            }
            catch (JPSRuntimeException e) {
                Assert.assertEquals("HTTP response with error = " + dummyStatus2 + "\n" + "test response", e.getMessage());
            }
        }

        // Test with RDFStoreType Fuseki
        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.FUSEKI, queryUrl, updateUrl);
        CloseableHttpResponse dummyResponse = Mockito.mock(CloseableHttpResponse.class, Mockito.RETURNS_DEEP_STUBS);
        Mockito.when(dummyResponse.getEntity()).thenReturn(new StringEntity("test response"));
        Mockito.when(dummyResponse.getStatusLine().getStatusCode()).thenReturn(200,500);
        try (MockedStatic<HttpClientBuilder> httpClientB = Mockito.mockStatic(HttpClientBuilder.class, Mockito.RETURNS_DEEP_STUBS)) {
            httpClientB.when(() -> HttpClientBuilder.create().build().execute(Mockito.any(HttpGet.class))).thenReturn(dummyResponse);
            Assert.assertEquals("test response", testS.executeGet(sparql));
            StatusLine dummyStatus = new BasicStatusLine(new ProtocolVersion("HTTP", 1, 1), 500, "error");
            Mockito.when(dummyResponse.getStatusLine()).thenReturn(dummyStatus);
            Mockito.when(dummyResponse.getAllHeaders()).thenReturn(new Header[] {});
            try {
                testS.executeGet(sparql);
                Assert.fail();
            }
            catch (JPSRuntimeException e) {
                Assert.assertEquals("HTTP response with error = " + dummyStatus + "\n" + "test response", e.getMessage());
            }
        }

    }

    private static String formInsertQuery(){
        String query = "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>\n";
        query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
        query = query.concat("INSERT DATA { <http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl#ArrheniusCoefficient_182161099217501> ontokin:hasTemperatureExponent \"-0.7\" }");
        return query;
    }


}