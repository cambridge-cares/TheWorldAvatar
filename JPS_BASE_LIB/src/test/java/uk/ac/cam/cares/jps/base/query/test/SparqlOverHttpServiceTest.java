package uk.ac.cam.cares.jps.base.query.test;

import org.json.CDL;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService;


public class SparqlOverHttpServiceTest {
    private SparqlOverHttpService testS;
    String sparql = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
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

        try (MockedConstruction<RemoteStoreClient> mocked = Mockito.mockConstruction(RemoteStoreClient.class,
                (mock, context) -> {
                    // further stubbings ...
                    Mockito.when(mock.executeUpdate()).thenReturn(1);
                })) {

            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.BLAZEGRAPH, queryUrl, updateUrl);
            Assert.assertEquals("1", testS.executePost(formInsertQuery()));;
        }

        try{
            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.FUSEKI, queryUrl, updateUrl);
            Assert.assertTrue(testS.executePost(formInsertQuery()).contains("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">"));

            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, queryUrl, updateUrl);
            Assert.assertTrue(testS.executePost(formInsertQuery()).contains("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">"));
        }catch (Exception e){
            Assert.assertTrue(e.getMessage().contains("HTTP response with error = "));
        }


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