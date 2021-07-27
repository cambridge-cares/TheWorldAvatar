package uk.ac.cam.cares.jps.base.query.test;

import com.sun.jndi.toolkit.url.Uri;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpResponse;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.client.LaxRedirectStrategy;
import org.apache.http.util.EntityUtils;
import org.json.CDL;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.web.client.MockRestServiceServer;
import org.springframework.web.client.RestTemplate;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.discovery.MediaType;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.ResourcePathConverter;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService;
import uk.ac.cam.cares.jps.base.scenario.JPSContext;

import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;

import static java.util.Objects.requireNonNull;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;


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
        HttpResponse mockResp = Mockito.mock(HttpResponse.class, RETURNS_DEEP_STUBS);

        try (MockedConstruction<RemoteStoreClient> mocked = Mockito.mockConstruction(RemoteStoreClient.class,
                (mock, context) -> {
                    // further stubbings ...
                    Mockito.when(mock.executeUpdate()).thenReturn(1);
                })) {

            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.BLAZEGRAPH, queryUrl, updateUrl);
            Assert.assertEquals("1", testS.executePost(formInsertQuery()));;
        }

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.FUSEKI, queryUrl, updateUrl);

        HttpResponse expected;
        HttpPost request = new HttpPost(updateUrl);
        request.setEntity(new StringEntity(formInsertQuery()));
        request.setHeader(HttpHeaders.CONTENT_TYPE, "application/sparql-update");
        expected = HttpClientBuilder.create().setRedirectStrategy(new LaxRedirectStrategy()).build().execute(request);

        try (MockedStatic<HttpClientBuilder> httpClientB = Mockito.mockStatic(HttpClientBuilder.class,RETURNS_DEEP_STUBS)) {
            httpClientB.when(() -> HttpClientBuilder.create().build().execute(request)).thenReturn(expected);

        }
        Assert.assertEquals(testS.executePost(formInsertQuery()), EntityUtils.toString(expected.getEntity()));

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, queryUrl, updateUrl);
        Assert.assertTrue(testS.executePost(formInsertQuery()).contains(expected.getEntity().toString()));

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