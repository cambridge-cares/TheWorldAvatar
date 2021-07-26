package uk.ac.cam.cares.jps.base.query.test;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService;

import static org.mockito.ArgumentMatchers.anyString;

public class SparqlOverHttpServiceTest {
    SparqlOverHttpService testS;
    String queryEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
    String updateEndpoint = "http://localhost:8080/blazegraph/namespace/ontokin/sparql";
    String userName = "user";
    String password = "password";
    String sparql = "SELECT ?o WHERE {<http://www.theworldavatar.com/kb/species/species.owl#species_1> <http://www.w3.org/2008/05/skos#altLabel> ?o.}";
    String update = "INSERT { <http://test/test> dc:test  \"This is a test\" }";
    @Test
    public void testToString() {
        String testUrl = "http://test.com/triples";
        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, testUrl);
        String assertStr = "SparqlOverHttpService[type=RDF4J, query url=http://test.com/triples, update url=http://test.com/triples/statements";

        Assert.assertEquals(assertStr, testS.toString());
    }

    @Test
    public void testExecutePost() throws Exception{
        String testUrl = "http://test.com/triples";

//        mockHttpClient = Mockito.mock(HttpClient.class);
//        Mockito.when(mockHttpClient.execute(Mockito.isA(HttpGet.class))).thenReturn(mockHttpResponse);


        try (MockedConstruction<RemoteStoreClient> mocked = Mockito.mockConstruction(RemoteStoreClient.class)) {
            RemoteStoreClient mockRSC = new RemoteStoreClient(queryEndpoint, updateEndpoint, sparql, userName, password);
//            RemoteStoreClient mockRSC = new RemoteStoreClient();
//            Mockito.doReturn(1).when(mockRSC).executeUpdate();
            Mockito.when(mockRSC.executeUpdate()).thenReturn(1);
            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.BLAZEGRAPH, "http://localhost:8080/blazegraph/namespace/ontokin/sparql");
            Assert.assertEquals("1", testS.executePost("test"));
        }
        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, testUrl);
        Assert.assertTrue(testS.executePost(update).contains(formGetRDF4J()));


    }

    @Test
    public void testExecuteGet() throws Exception{
        String testUrl = "http://test.com/triples";

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.FUSEKI, testUrl);
        Assert.assertEquals("", testS.executeGet(sparql));

        try (MockedConstruction<RemoteStoreClient> mocked = Mockito.mockConstruction(RemoteStoreClient.class)) {
            RemoteStoreClient mockRSC = new RemoteStoreClient();
//            RemoteStoreClient kbClient = Mockito.spy(RemoteStoreClient.class);
            JSONArray jsonArray = new JSONArray();
            JSONObject jsonObject = new JSONObject();
            jsonObject.put("count", "1");
            jsonArray.put(jsonObject);
            mockRSC.setQueryEndpoint(queryEndpoint);
            Mockito.doReturn(jsonArray).when(mockRSC).executeQuery();

            testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.BLAZEGRAPH, testUrl);
            Assert.assertEquals("1", testS.executeGet(sparql));
        }

        testS = new SparqlOverHttpService(SparqlOverHttpService.RDFStoreType.RDF4J, testUrl);
        Assert.assertTrue(testS.executeGet(sparql).contains(formGetRDF4J()));




    }

    private static String formGetRDF4J(){
        String respond1 = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n" +
                "<html>\n" +
                "<head>\n" +
                "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n" +
                "<meta http-equiv=\"Content-Script-Type\" content=\"text/javascript\">\n" +
                "<script type=\"text/javascript\">\n" +
                "function getCookie(c_name) { // Local function for getting a cookie value\n" +
                "    if (document.cookie.length > 0) {\n" +
                "        c_start = document.cookie.indexOf(c_name + \"=\");\n" +
                "        if (c_start!=-1) {\n" +
                "        c_start=c_start + c_name.length + 1;\n" +
                "        c_end=document.cookie.indexOf(\";\", c_start);\n" +
                "\n" +
                "        if (c_end==-1) \n" +
                "            c_end = document.cookie.length;\n" +
                "\n" +
                "        return unescape(document.cookie.substring(c_start,c_end));\n" +
                "        }\n" +
                "    }\n" +
                "    return \"\";\n" +
                "}\n" +
                "function setCookie(c_name, value, expiredays) { // Local function for setting a value of a cookie\n" +
                "    var exdate = new Date();\n" +
                "    exdate.setDate(exdate.getDate()+expiredays);\n" +
                "    document.cookie = c_name + \"=\" + escape(value) + ((expiredays==null) ? \"\" : \";expires=\" + exdate.toGMTString()) + \";path=/\";\n" +
                "}\n" +
                "function getHostUri() {\n" +
                "    var loc = document.location;\n" +
                "    return loc.toString();\n" +
                "}\n" +
                "setCookie('YPF8827340282Jdskjhfiw_928937459182JAX666', '103.6.150.135', 10);\n" +
                "try {  \n" +
                "    location.reload(true);  \n" +
                "} catch (err1) {  \n" +
                "    try {  \n" +
                "        location.reload();  \n" +
                "    } catch (err2) {  \n" +
                "    \tlocation.href = getHostUri();  \n" +
                "    }  \n" +
                "}\n" +
                "</script>\n" +
                "</head>\n" +
                "<body>\n" +
                "<noscript>This site requires JavaScript and Cookies to be enabled. Please change your browser settings or upgrade your browser.</noscript>\n" +
                "</body>\n" +
                "</html>\n";
        return respond1;
    }


}