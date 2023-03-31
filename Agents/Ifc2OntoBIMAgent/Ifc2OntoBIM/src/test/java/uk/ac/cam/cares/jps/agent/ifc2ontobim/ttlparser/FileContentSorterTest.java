package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

class FileContentSorterTest {

    private static List<String> standardTestContents;
    private static List<String> renamedTestContents;

    @BeforeAll
    static void genTestContents() {
        standardTestContents = new ArrayList<>();
        standardTestContents.add("bim:inst1 rdf:type bim:class1");
        standardTestContents.add("bim:inst2 rdf:type bim:class2");
        standardTestContents.add("bim:inst3 rdf:type bim:class3");
        standardTestContents.add("bim:inst1 bim:hasValue 'value1'");
        standardTestContents.add("bim:inst4 rdf:type bim:class4");
        standardTestContents.add("bim:inst2 bim:hasValue 'value2'");

        renamedTestContents = new ArrayList<>();
        renamedTestContents.add("bim:Building bim:hasWall bim:IfcSlab_2135");
        renamedTestContents.add("bim:Building bim:hasRoof bim:IfcSlab_521");
        renamedTestContents.add("bim:IfcSlab_2135 rdf:type bim:IfcSlab");
        renamedTestContents.add("bim:IfcSlab_521 rdf:type bim:IfcSlab");
    }

    @Test
    void testGroupTriples() {
        StringBuilder expected = new StringBuilder();
        Map<String, String> emptyMap = new HashMap<>();
        expected.append("\n\n")
                .append("bim:inst1 rdf:type bim:class1;\n")
                .append("\tbim:hasValue 'value1'.\n\n")
                .append("bim:inst2 rdf:type bim:class2;\n")
                .append("\tbim:hasValue 'value2'.\n\n")
                .append("bim:inst3 rdf:type bim:class3.\n\n")
                .append("bim:inst4 rdf:type bim:class4.");
        assertEquals(expected.toString(), FileContentSorter.groupTriples(standardTestContents, emptyMap).toString());
    }

    @Test
    void testGroupTriplesForRenaming() {
        Map<String, String> classMap = genTestMap();
        String results = FileContentSorter.groupTriples(renamedTestContents, classMap).toString();
        List<String> expected = new ArrayList<>();
        expected.add("bim:Building bim:hasWall bim:Wall_2135;");
        expected.add("bim:hasRoof bim:Roof_521.");
        expected.add("bim:Wall_2135 rdf:type bim:Wall.");
        expected.add("bim:Roof_521 rdf:type bim:Roof.");
        expected.forEach(line -> assertTrue(results.contains(line)));
    }

    private Map<String, String> genTestMap() {
        Map<String, String> classMap = new HashMap<>();
        classMap.put("IfcSlab_2135", "Wall");
        classMap.put("IfcSlab_521", "Roof");
        return classMap;
    }
}