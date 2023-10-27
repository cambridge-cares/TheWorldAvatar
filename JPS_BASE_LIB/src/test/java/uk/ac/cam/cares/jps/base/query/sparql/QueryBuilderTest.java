package uk.ac.cam.cares.jps.base.query.sparql;

import org.junit.Assert;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.query.sparql.QueryBuilder;



public class QueryBuilderTest {
    private QueryBuilder qbuilder = new QueryBuilder();

    @Test
    public void testSelect() {
        Assert.assertEquals("SELECT ?x ", qbuilder.select("?x").toString());
    }

    @Test
    public void testA() {
        Assert.assertEquals("?o a owl:test", qbuilder.a("?o", "owl", "test").toString());
    }

    @Test
    public void testProp() {
        //test single path
        Assert.assertEquals("?o rdfs:jps ?x", qbuilder.prop("?o", "?x", "rdfs", "jps").toString());
        //test several path
        Assert.assertEquals("?o rdf:worldavatar/rdfs:jps ?x", qbuilder.prop("?o", "?x", "rdf", "worldavatar", "rdfs", "jps").toString());
    }

    @Test
    public void testBuild() throws Exception {
        String b = "PREFIX owl:<http://www.w3.org/2002/07/owl#> \r\n" +
                "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> \r\n" +
                "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#> \r\n" +
                "SELECT ?x \r\nWHERE {\r\n?o a owl:test .\r\n?o rdf:worldavatar/rdfs:jps ?x .\r\n}";

        qbuilder.select("?x");
        qbuilder.a("?o", "owl", "test");
        qbuilder.prop("?o", "?x", "rdf", "worldavatar", "rdfs", "jps");

        Assert.assertEquals(b, qbuilder.build().toString());
    }
}