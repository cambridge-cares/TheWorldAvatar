package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import java.util.ArrayList;
import java.util.List;

public class JunitTestGeometryUtils {

    public static List<String> genExpectedFacetedBrepStatements(String baseURI, String inst) {
        List<String> expected = new ArrayList<>();
        expected.add(inst + ", https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/FacetedBrep");
        expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasExteriorBoundary, " + baseURI + "ClosedShell_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(baseURI + "ClosedShell_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/ClosedShell");
        return expected;
    }

    public static List<String> genExpectedFacetedBrepFaceStatements(String baseURI, String faceBoundaryIri, boolean isNonInversedOrientation) {
        List<String> expected = new ArrayList<>();
        expected.add(baseURI + "ClosedShell_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasConnectedFaces, " + baseURI + "Face_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(baseURI + "Face_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/Face");
        expected.add(baseURI + "Face_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasBounds, " + baseURI + "FaceOuterBound_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(baseURI + "FaceOuterBound_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/FaceOuterBound");
        expected.add(baseURI + "FaceOuterBound_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasFaceBoundary, " + faceBoundaryIri);
        expected.add(baseURI + "FaceOuterBound_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/isLoopNonInversedOrientation, \"" + isNonInversedOrientation);
        return expected;
    }

    public static List<String> genExpectedHalfSpaceStatements(String baseURI, String inst, String positionIri, String surfacePosition, String boundaryIri, boolean agreementFlag) {
        List<String> expected = genExpectedHalfSpaceSolidStatements(baseURI, inst, "PolygonalBoundedHalfSpace", surfacePosition, agreementFlag);
        expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + positionIri);
        expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasPolygonalBoundary, " + boundaryIri);
        return expected;
    }

    public static List<String> genExpectedHalfSpaceSolidStatements(String baseURI, String inst, String bimClass, String surfacePosition, boolean agreementFlag) {
        List<String> expected = new ArrayList<>();
        expected.add(inst + ", https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/" + bimClass);
        expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasBaseSurface, " + baseURI + "SurfacePlane_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}");
        expected.add(baseURI + "SurfacePlane_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/SurfacePlane");
        expected.add(baseURI + "SurfacePlane_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasLocalPosition, " + surfacePosition);
        expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasAgreementFlag, \"" + agreementFlag);
        return expected;
    }

    public static List<String> genExpectedPolylineStatements(String polylineInst, String startingVertexInst, String startingPointInst, String bimClass) {
        List<String> expected = new ArrayList<>();
        expected.add(polylineInst + ", https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/" + bimClass);
        expected.add(polylineInst + ", https://www.theworldavatar.com/kg/ontobim/hasStartingVertex, " + startingVertexInst);
        expected.add(startingVertexInst + ", https://www.theworldavatar.com/kg/ontobim/hasRefPoint, " + startingPointInst);
        expected.add(startingVertexInst + ", https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/LineVertex");
        return expected;
    }

    public static List<String> genExpectedNextLineVertexStatements(String prevVertex, String currentVertex, String currentPoint) {
        List<String> expected = new ArrayList<>();
        expected.add(prevVertex + ", https://www.theworldavatar.com/kg/ontobim/hasNextVertex, " + currentVertex);
        expected.add(currentVertex + ", https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/LineVertex");
        expected.add(currentVertex + ", https://www.theworldavatar.com/kg/ontobim/hasRefPoint, " + currentPoint);
        return expected;
    }

    public static List<String> genExpectedPointStatements(String baseURI, Double xCoord, Double yCoord, Double zCoord, boolean requireType) {
        List<String> expected = new ArrayList<>();
        if (requireType) {
            expected.add(baseURI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/CartesianPoint");
        }
        genExpectedPointStatements(baseURI, xCoord, yCoord, zCoord);
        return expected;
    }

    public static List<String> genExpectedPointStatements(String baseURI, Double xCoord, Double yCoord, Double zCoord) {
        List<String> expected = new ArrayList<>();
        expected.add(baseURI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasXCoordinate, \"" + xCoord);
        expected.add(baseURI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasYCoordinate, \"" + yCoord);
        if (zCoord != null) {
            expected.add(baseURI + "CartesianPoint_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasZCoordinate, \"" + zCoord);
        }
        return expected;
    }

    public static List<String> genExpectedDirectionStatements(String baseURI, Double xDir, Double yDir, Double zDir, boolean requireType) {
        List<String> expected = new ArrayList<>();
        if (requireType) {
            expected.add(baseURI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/DirectionVector");
        }
        genExpectedDirectionStatements(baseURI, xDir, yDir, zDir);
        return expected;
    }

    public static List<String> genExpectedDirectionStatements(String baseURI, Double xDir, Double yDir, Double zDir) {
        List<String> expected = new ArrayList<>();
        expected.add(baseURI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasXDirectionRatio, \"" + xDir);
        expected.add(baseURI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasYDirectionRatio, \"" + yDir);
        if (zDir != null) {
            expected.add(baseURI + "DirectionVector_[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}, https://www.theworldavatar.com/kg/ontobim/hasZDirectionRatio, \"" + zDir);
        }
        return expected;
    }

    public static List<String> genExpectedBooleanClippingResultStatements(String inst, String booleanOperator, String firstOperandIri, String secOperandIri) {
        List<String> expected = new ArrayList<>();
        expected.add(inst + ", https://www.w3.org/1999/02/22-rdf-syntax-ns#type, https://www.theworldavatar.com/kg/ontobim/BooleanClippingResult");
        expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasBooleanOperator, " + booleanOperator);
        expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasFirstOperand, " + firstOperandIri);
        expected.add(inst + ", https://www.theworldavatar.com/kg/ontobim/hasSecondOperand, " + secOperandIri);
        return expected;
    }
}
