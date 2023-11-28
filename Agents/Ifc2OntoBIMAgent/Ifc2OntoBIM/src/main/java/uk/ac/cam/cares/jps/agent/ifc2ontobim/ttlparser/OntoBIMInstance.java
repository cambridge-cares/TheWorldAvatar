package uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser;

/**
 * Provides the ontoBIM class name to replace the IfcOwl instance name.
 *
 * @author qhouyee
 */
class OntoBIMInstance {
    /**
     * Retrieves the relevant ontoBIM class name for a specific ifc instance. If none is available, defaults to "NA".
     *
     * @param ifcClass The ifc instance name without any numerical identifier.
     */
    protected static String retrieveOntoBimName(String ifcClass) {
        String replacementName;
        switch (ifcClass) {
            // Zones
            case "IfcProject":
                replacementName = "Project";
                break;
            case "IfcSite":
                replacementName = "Site";
                break;
            case "IfcBuilding":
                replacementName = "Building";
                break;
            case "IfcBuildingStorey":
                replacementName = "Storey";
                break;
            case "IfcSpace":
                replacementName = "Space";
                break;
            // Elements
            case "IfcRoof":
                replacementName = "Roof";
                break;
            case "IfcWall":
            case "IfcWallStandardCase":
                replacementName = "Wall";
                break;
            case "IfcDoor":
                replacementName = "Door";
                break;
            case "IfcWindow":
                replacementName = "Window";
                break;
            case "IfcOpeningElement":
                replacementName = "GeometryVoid";
                break;
            case "IfcColumn":
                replacementName = "Column";
                break;
            case "IfcStair":
                replacementName = "Stair";
                break;
            case "IfcStairFlight":
                replacementName = "StairFlight";
                break;
            case "IfcRailing":
                replacementName = "Railing";
                break;
            // Geospatial Operators
            case "IfcGeometricRepresentationContext":
                replacementName = "GeometricRepresentationContext";
                break;
            case "IfcGeometricRepresentationSubContext":
                replacementName = "GeometricRepresentationSubContext";
                break;
            case "IfcShapeRepresentation":
                replacementName = "ModelRepresentation3D";
                break;
            case "IfcCartesianTransformationOperator3D":
                replacementName = "CartesianTransformationOperator";
                break;
            case "IfcLocalPlacement":
            case "IfcAxis2Placement3D":
                replacementName = "LocalPlacement";
                break;
            case "IfcDirection":
                replacementName = "DirectionVector";
                break;
            case "IfcCartesianPoint_List":
                replacementName = "LineVertex";
                break;
            case "IfcCartesianPoint":
                replacementName = "CartesianPoint";
                break;
            // Geometry
            case "IfcBooleanClippingResult":
                replacementName = "BooleanClippingResult";
                break;
            case "IfcPolygonalBoundedHalfSpace":
                replacementName = "PolygonalBoundedHalfSpace";
                break;
            case "IfcPlane":
                replacementName = "SurfacePlane";
                break;
            case "IfcPolyline":
                replacementName = "Polyline";
                break;
            case "IfcFacetedBrep":
                replacementName = "FacetedBrep";
                break;
            case "IfcClosedShell":
                replacementName = "ClosedShell";
                break;
            case "IfcFace":
                replacementName = "Face";
                break;
            case "IfcFaceOuterBound":
                replacementName = "FaceOuterBound";
                break;
            case "IfcPolyLoop":
                replacementName = "PolyLoop";
                break;
            case "IfcExtrudedAreaSolid":
                replacementName = "ExtrudedAreaSolid";
                break;
            case "IfcRectangleProfileDef":
                replacementName = "RectangleProfileDefinition";
                break;
            default:
                replacementName = "NA";
                break;
        }
        return replacementName;
    }
}
