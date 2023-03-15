package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.vocabulary.VCARD;
import org.junit.jupiter.api.Test;


import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class NamespaceMapperTest {
    private static final String baseURI = "http://example.com/";
    private static final String janeURI = "JaneDoe";
    private static final String janeFullName = "Jane Doe";

    @Test
    void testRetrieveNamespace() {
        Model testModel = genSampleModel();
        Map<String, String> nsmapping = NamespaceMapper.retrieveNamespace(testModel);
        // Format the results
        StringBuilder strBuilder = new StringBuilder();
        for (var entry : nsmapping.entrySet()) {
            strBuilder.append(entry.getKey())
                    .append(":  <")
                    .append(entry.getValue())
                    .append(">\n");
        }
        String results = strBuilder.toString();

        List<String> expected = genExpectedResultList();
        // Add additional namespaces and prefixes
        expected.add("eg:");
        expected.add("vcard:");
        expected.add(baseURI);
        expected.add(VCARD.getURI());
        // The results must contain all these namespaces
        expected.forEach(line -> assertTrue(results.contains(line)));
    }

    @Test
    void testAddSubqueryBuilderNamespaces() {
        ConstructBuilder builder = new ConstructBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(builder);

        String namespaces = builder.buildString();
        List<String> expected = genExpectedResultList();
        expected.forEach(line -> assertTrue(namespaces.contains(line)));
    }

    private static Model genSampleModel() {
        Model testModel = ModelFactory.createDefaultModel();
        testModel.createResource(baseURI + janeURI)
                .addProperty(VCARD.FN, janeFullName);
        testModel.setNsPrefix("eg", baseURI);
        testModel.setNsPrefix("vcard", VCARD.getURI());
        return testModel;
    }

    private static List<String> genExpectedResultList() {
        List<String> results = new ArrayList<>();
        results.add("rdf:");
        results.add("bot:");
        results.add("bim:");
        results.add("ifc:");
        results.add("rdfs:");
        results.add("express:");
        results.add("list:");
        results.add("om:");
        results.add("ontobuildingstructure:");
        results.add("ontobms:");
        results.add("ontodevice:");
        results.add("ontoems:");
        results.add("ontolab:");
        results.add("saref:");
        results.add("<http://www.w3.org/1999/02/22-rdf-syntax-ns#>");
        results.add("<https://w3id.org/bot#>");
        results.add("<http://www.theworldavatar.com/kg/ontobim/>");
        results.add("<http://standards.buildingsmart.org/IFC/DEV/IFC2x3/TC1/OWL#>");
        results.add("<http://www.w3.org/2000/01/rdf-schema#>");
        results.add("<https://w3id.org/express#>");
        results.add("<https://w3id.org/list#>");
        results.add("<http://www.ontology-of-units-of-measure.org/resource/om-2/>");
        results.add("<http://www.theworldavatar.com/kg/ontobuildingstructure/>");
        results.add("<https://www.theworldavatar.com/kg/ontobms/>");
        results.add("<https://www.theworldavatar.com/kg/ontodevice/>");
        results.add("<https://www.theworldavatar.com/kg/ontoems/>");
        results.add("<https://www.theworldavatar.com/kg/ontolab/>");
        results.add("<https://saref.etsi.org/core/>");
        return results;
    }
}