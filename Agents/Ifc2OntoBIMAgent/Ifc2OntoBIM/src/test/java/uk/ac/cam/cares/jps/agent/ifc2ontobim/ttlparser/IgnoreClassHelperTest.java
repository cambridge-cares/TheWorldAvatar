package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class IgnoreClassHelperTest {
    private static Set<String> expected;

    @BeforeEach
    void genExpectedSet() {
        expected = new HashSet<>();
        expected.add("BooleanClippingResult");
        expected.add("PolygonalBoundedHalfSpace");
        expected.add("FacetedBrep");
        expected.add("ExtrudedAreaSolid");
        expected.add("Polyline");
        expected.add("GeometricRepresentationSubContext");
        expected.add("CartesianTransformationOperator");
        expected.add("LocalPlacement");
        expected.add("Direction");
        expected.add("CartesianPoint_");
    }

    @Test
    void genIgnoreSet() {
        Set<String> result = IgnoreClassHelper.genIgnoreSet();
        assertEquals(expected, result);
    }

    @Test
    void removeIgnoredClass() {
        // Set up
        String ignoreOne = "BooleanClippingResult";
        String ignoreTwo = "Direction";
        String ignoreThree = "CartesianPoint_";
        Set<String> result = IgnoreClassHelper.genIgnoreSet();

        // Test removal of ifc namespace
        IgnoreClassHelper.removeIgnoredClass("ifc:IfcBooleanClippingResult", result);
        expected.remove(ignoreOne);
        assertEquals(expected, result);
        // Test removal of bim namespace
        IgnoreClassHelper.removeIgnoredClass("bim:DirectionVector", result);
        expected.remove(ignoreTwo);
        assertEquals(expected, result);
        // Test removal of switch default
        IgnoreClassHelper.removeIgnoredClass("bim:CartesianPoint", result);
        expected.remove(ignoreThree);
        assertEquals(expected, result);

        assertFalse(result.contains(ignoreOne));
        assertFalse(result.contains(ignoreTwo));
        assertFalse(result.contains(ignoreThree));
    }
}