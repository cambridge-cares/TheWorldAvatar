package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.SelectBuilder;

import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Statement;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.JunitTestUtils;


import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class QueryHandlerTest {
    private static final String inst = "Storey_514";
    private static final String secondInst = "Storey_654";
    private static final String testClass = "Storey";
    private static final String testConstructClass = "Building";


    @Test
    void testInitSelectQueryBuilder() {
        SelectBuilder builder = QueryHandler.initSelectQueryBuilder();
        List<String> expected = this.genInitQuery();
        expected.forEach(line -> assertTrue(builder.buildString().contains(line)));
    }

    @Test
    void testExecSelectQuery() {
        Model sampleModel = this.genSampleModel();
        List<String> expected = new ArrayList<>();
        ResultSet results = QueryHandler.execSelectQuery(this.genSampleSelectQuery(), sampleModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            expected.add(soln.get("storey").toString());
        }
        assertTrue(expected.contains(JunitTestUtils.bimUri + inst));
        assertTrue(expected.contains(JunitTestUtils.bimUri + secondInst));
    }

    @Test
    void testQueryConstructStatementsAsSet() {
        Model sampleModel = this.genSampleModel();
        LinkedHashSet<Statement> results = new LinkedHashSet<>();
        QueryHandler.queryConstructStatementsAsSet(this.genSampleConstructQuery(), sampleModel, results);
        // Store the results as string to make it easier to compare
        List<String> strResults = new ArrayList<>();
        results.forEach(statement -> strResults.add(statement.toString()));
        // Generate the expected statements
        String firstStatement = "[" + JunitTestUtils.bimUri + inst + ", " + RDF.type + ", " + JunitTestUtils.bimUri + testConstructClass + "]";
        String secondStatement = "[" + JunitTestUtils.bimUri + secondInst + ", " + RDF.type + ", " + JunitTestUtils.bimUri + testConstructClass + "]";
        assertTrue(strResults.contains(firstStatement));
        assertTrue(strResults.contains(secondStatement));
    }

    private List<String> genInitQuery() {
        List<String> expected = new ArrayList<>();
        expected.add("PREFIX  rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>");
        expected.add("PREFIX  bot:  <https://w3id.org/bot#>");
        expected.add("PREFIX  bim:  <http://www.theworldavatar.com/kg/ontobim/>");
        expected.add("PREFIX  ifc:  <http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>");
        expected.add("PREFIX  rdfs: <http://www.w3.org/2000/01/rdf-schema#>");
        expected.add("PREFIX  express: <https://w3id.org/express#>");
        expected.add("PREFIX  list: <https://w3id.org/list#>");
        expected.add("PREFIX  om:   <http://www.ontology-of-units-of-measure.org/resource/om-2/>");
        expected.add("SELECT DISTINCT  *");
        expected.add("WHERE");
        return expected;
    }

    private Model genSampleModel() {
        Model sampleModel = ModelFactory.createDefaultModel();
        sampleModel.createResource(JunitTestUtils.bimUri + inst)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.bimUri + testClass));
        sampleModel.createResource(JunitTestUtils.bimUri + secondInst)
                .addProperty(RDF.type,
                        sampleModel.createResource(JunitTestUtils.bimUri + testClass));
        return sampleModel;
    }

    private String genSampleSelectQuery() {
        SelectBuilder builder = new SelectBuilder();
        builder.addPrefix("bim", JunitTestUtils.bimUri);
        builder.addVar("?storey").addWhere("?storey", RDF.type, "bim:" + testClass);
        return builder.buildString();
    }

    private String genSampleConstructQuery() {
        ConstructBuilder builder = new ConstructBuilder();
        builder.addPrefix("bim", JunitTestUtils.bimUri);
        builder.addConstruct("?storey", RDF.type, "bim:" + testConstructClass)
                .addWhere("?storey", RDF.type, "bim:" + testClass);
        return builder.buildString();
    }
}