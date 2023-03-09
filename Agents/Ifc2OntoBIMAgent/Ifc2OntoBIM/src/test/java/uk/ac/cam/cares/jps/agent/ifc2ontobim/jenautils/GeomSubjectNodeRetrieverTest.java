package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.LinkedHashSet;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class GeomSubjectNodeRetrieverTest {
    private static LinkedHashSet<Statement> testSet;
    private static final String bimUri = "http://www.theworldavatar.com/ontology/ontobim/ontoBIM#";
    private static final String inst = "CartesianPoint_50140";
    private static final String pointClass = "CartesianPoint";
    private static final String wrongClass = "WrongClass";

    @BeforeEach
    void genSampleStatement() {
        Model sampleModel = ModelFactory.createDefaultModel();
        // Generate the statements in the model
        sampleModel.createResource(bimUri + inst)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + pointClass));
        sampleModel.createResource(bimUri + wrongClass)
                .addProperty(RDF.type,
                        sampleModel.createResource(bimUri + wrongClass));
        // Extract the statements into a LinkedHashSet
        testSet = new LinkedHashSet<>();
        StmtIterator iter = sampleModel.listStatements();
        while (iter.hasNext()) {
            Statement stmt = iter.nextStatement();
            testSet.add(stmt);
        }
    }

    @Test
    void testIfcGeometrySubjectNodeRetrieverConstructor() {
        assertNotNull(new GeomSubjectNodeRetriever("cartesianpt", "bim:" + pointClass));
    }

    @Test
    void testRetrieveIriAsList() {
        GeomSubjectNodeRetriever retriever = new GeomSubjectNodeRetriever("cartesianpt", "bim:" + pointClass);
        List<RDFNode> iriList = retriever.retrieveIriAsList(testSet);
        assertTrue(iriList.get(0).toString().contains(pointClass));
        // Wrong class must not be extracted
        iriList.forEach(node -> assertFalse(node.toString().contains(wrongClass)));
    }
}