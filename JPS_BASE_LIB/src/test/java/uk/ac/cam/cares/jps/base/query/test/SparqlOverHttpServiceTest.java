package uk.ac.cam.cares.jps.base.query.test;

import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClientBuilder;
import org.json.CDL;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService;


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
//        HttpResponse expected = Mockito.mock(HttpResponse.class);
        HttpResponse expected;
        HttpClient httpClient = Mockito.mock(HttpClient.class);
        HttpPost request = new HttpPost(updateUrl);
        request.setEntity(new StringEntity(formInsertQuery()));

//        Mockito.when(statusLine.getStatusCode()).thenReturn(200);

//        Mockito.when(request.setEntity(Mockito.any(HttpEntity.class))).thenAnswer(
//                new Answer() {
//                    public Object answer(InvocationOnMock invocation) {
//                        Object[] args = invocation.getArguments();
//                        Object mock = invocation.getMock();
//                        request.setHeader();
//                        return request;
//                    }
//                });

        try (MockedConstruction<RemoteStoreClient> mocked = Mockito.mockConstruction(RemoteStoreClient.class,
                (mock, context) -> {
                    // further stubbings ...
                    Mockito.when(mock.executeUpdate()).thenReturn(1);
                })) {

            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.BLAZEGRAPH, queryUrl, updateUrl);
            Assert.assertEquals("1", testS.executePost(formInsertQuery()));;
        }

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.FUSEKI, queryUrl, updateUrl);
        request.setHeader(HttpHeaders.CONTENT_TYPE, "application/sparql-update");
        expected = HttpClientBuilder.create().build().execute(request);
        expected.setStatusCode(200);
        Mockito.when(httpClient.execute(request)).thenReturn(expected);
        Assert.assertTrue(testS.executePost(formInsertQuery()).contains(expected.getEntity().toString()));

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, queryUrl, updateUrl);
        Assert.assertTrue(testS.executePost(formInsertQuery()).contains(expected.getEntity().toString()));



    }

    @Test
    public void testExecuteGet() throws Exception{
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

        try{
            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, queryUrl, updateUrl);
            Assert.assertTrue(testS.executeGet(sparql).contains("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">"));

            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.FUSEKI, queryUrl, updateUrl);
            Assert.assertTrue(testS.executeGet(sparql).contains("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">"));
        }catch (Exception e){
            Assert.assertTrue(e.getMessage().contains("HTTP response with error = "));
        }

    }

    private static String formInsertQuery(){
        String query = "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>\n";
        query = query.concat("PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n");
        query = query.concat("INSERT DATA { <http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl#ArrheniusCoefficient_182161099217501> ontokin:hasTemperatureExponent \"-0.7\" }");
        return query;
    }


}