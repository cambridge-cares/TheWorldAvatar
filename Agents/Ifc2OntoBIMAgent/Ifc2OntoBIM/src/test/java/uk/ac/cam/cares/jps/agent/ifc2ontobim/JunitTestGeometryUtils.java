package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import java.util.ArrayList;
import java.util.List;

public class JunitTestGeometryUtils {
    public static List<String> genExpectedCommonPolylineStatements(String polylineInst, String startingVertexInst, String startingPointInst) {
        List<String> expected = new ArrayList<>();
        expected.add(polylineInst + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/Polyline");
        expected.add(polylineInst + ", http://www.theworldavatar.com/kg/ontobim/hasStartingVertex, " + startingVertexInst);
        expected.add(startingVertexInst + ", http://www.theworldavatar.com/kg/ontobim/hasRefPoint, " + startingPointInst);
        expected.add(startingVertexInst + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LineVertex");
        return expected;
    }

    public static List<String> genExpectedNextLineVertexStatements(String prevVertex, String currentVertex, String currentPoint) {
        List<String> expected = new ArrayList<>();
        expected.add(prevVertex + ", http://www.theworldavatar.com/kg/ontobim/hasNextVertex, " + currentVertex);
        expected.add(currentVertex + ", http://www.w3.org/1999/02/22-rdf-syntax-ns#type, http://www.theworldavatar.com/kg/ontobim/LineVertex");
        expected.add(currentVertex + ", http://www.theworldavatar.com/kg/ontobim/hasRefPoint, " + currentPoint);
        return expected;
    }
}
