package uk.ac.cam.cares.jps.base.query.sparql.test;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.query.sparql.QueryBuilder;

import static org.junit.Assert.*;

public class QueryBuilderTest {
    private QueryBuilder qbuilder = new QueryBuilder();

    @Test @Before
    public void testSelect() {
        Assert.assertEquals("SELECT ?x ", qbuilder.select("?x").toString());
    }

    @Test @Before
    public void testA() {
        Assert.assertEquals("?o a owl:test", qbuilder.a("?o", "owl", "test").toString());
    }

    @Test @Before
    public void testProp() {
        //test single path
        Assert.assertEquals("?o rdfs:jps ?x", qbuilder.prop("?o", "?x", "rdfs", "jps").toString());
        //test several path
        Assert.assertEquals("?o rdf:worldavatar/rdfs:jps ?x", qbuilder.prop("?o", "?x", "rdf", "worldavatar", "rdfs", "jps").toString());
    }

    @Test
    public void testBuild() {
        String b = "PREFIX owl:<http://www.w3.org/2002/07/owl#> \r\n" +
                "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#> \r\n" +
                "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> \r\n" +
                "SELECT ?x \r\nWHERE {\r\n?o a owl:test .\r\n?o rdfs:jps ?x .\r\n?o rdf:worldavatar/rdfs:jps ?x .\r\n}";
        Assert.assertEquals(b, qbuilder.build().toString());
    }
}