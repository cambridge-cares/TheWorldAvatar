package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

import java.util.HashSet;
import java.util.Set;

/**
 * Provide helper methods involving classes to be ignored when writing output.
 *
 * @author qhouyee
 */
public class IgnoreClassHelper {
    /**
     * Generates a Set containing elements to ignore when retrieving statements for intermediate files.
     *
     * @return a set of classes to be ignored when writing the ttl file.
     */
    public static Set<String> genIgnoreSet() {
        Set<String> ignoreGeom = new HashSet<>();
        ignoreGeom.add("BooleanClippingResult");
        ignoreGeom.add("PolygonalBoundedHalfSpace");
        ignoreGeom.add("FacetedBrep");
        ignoreGeom.add("ExtrudedAreaSolid");
        ignoreGeom.add("Polyline");
        ignoreGeom.add("GeometricRepresentationSubContext");
        ignoreGeom.add("CartesianTransformationOperator");
        ignoreGeom.add("LocalPlacement");
        ignoreGeom.add("Direction");
        ignoreGeom.add("CartesianPoint_");
        return ignoreGeom;
    }

    /**
     * Removes the input ignoreClass from the set.
     */
    public static void removeIgnoredClass(String ignoreClass, Set<String> ignoreGeom) {
        String removedClass = ignoreClass.substring(4);
        switch (removedClass) {
            case "IfcBooleanClippingResult":
            case "IfcPolygonalBoundedHalfSpace":
            case "IfcFacetedBrep":
            case "IfcExtrudedAreaSolid":
            case "IfcPolyline":
                removedClass = removedClass.substring(3);
                ignoreGeom.remove(removedClass);
                break;
            case "DirectionVector":
                ignoreGeom.remove("Direction");
                break;
            case "CartesianPoint":
                ignoreGeom.remove("CartesianPoint_");
                break;
            default:
                ignoreGeom.remove(removedClass);
                break;
        }
    }
}
