package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.*;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.*;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.CommonQuery;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.OntoBimConstant;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.GeometryStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ModellingOperatorStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.QueryHandler;

import java.util.LinkedHashSet;

/**
 * Provides functions to generate the geometry' triples.
 *
 * @author qhouyee
 */
public class GeometryFacade {
    private GeometryStorage geomMappings;
    private final ModellingOperatorStorage operatorMappings;


    /**
     * Standard Constructor setting up the mappings.
     */
    public GeometryFacade() {
        this.operatorMappings = ModellingOperatorStorage.Singleton();
        this.geomMappings = GeometryStorage.Singleton();
        // Purge any old values
        this.geomMappings = GeometryStorage.resetSingleton();
    }

    /**
     * Creates the SPARQL SELECT query statements for FacetedBrep.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private String createFacetedBrepSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.GEOM_VAR)
                .addVar(CommonQuery.POLYLINE_VAR)
                .addVar(CommonQuery.BOOLEAN_VAR);
        selectBuilder.addWhere(CommonQuery.GEOM_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_FACETED_BREP)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_BREP_OUTER, CommonQuery.BREP_CLOSED_SHELL_VAR)
                .addWhere(CommonQuery.BREP_CLOSED_SHELL_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_CLOSED_SHELL)
                .addWhere(CommonQuery.BREP_CLOSED_SHELL_VAR, CommonQuery.IFC_BREP_FACES, CommonQuery.BREP_FACE_VAR)
                .addWhere(CommonQuery.BREP_FACE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_FACE)
                .addWhere(CommonQuery.BREP_FACE_VAR, CommonQuery.IFC_BREP_FACE_BOUNDS, CommonQuery.BREP_FACE_OUTER_BOUND_VAR)
                .addWhere(CommonQuery.BREP_FACE_OUTER_BOUND_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_FACE_OUTER_BOUND)
                .addWhere(CommonQuery.BREP_FACE_OUTER_BOUND_VAR, CommonQuery.IFC_BREP_FACE_BOUNDARY, CommonQuery.POLYLINE_VAR)
                .addWhere(CommonQuery.BREP_FACE_OUTER_BOUND_VAR, CommonQuery.IFC_BREP_FACE_ORIENTATION + CommonQuery.EXPRESS_HASBOOLEAN, CommonQuery.BOOLEAN_VAR);
        return selectBuilder.buildString();
    }

    /**
     * Retrieve the OntoBIM statements for FacetedBrep.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addFacetedBrepStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String query = createFacetedBrepSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.GEOM_VAR).toString();
            String faceBoundaryIri = QueryHandler.retrieveIri(soln, CommonQuery.POLYLINE_VAR);
            boolean isOrientation = Boolean.parseBoolean(QueryHandler.retrieveLiteral(soln, CommonQuery.BOOLEAN_VAR));
            FacetedBrep geometry;
            // If the geometry has already been created previously
            if (this.geomMappings.containsIri(iri)) {
                // Retrieve the new geometry IRI and append the current face details
                geometry = this.geomMappings.getFacetedBrep(iri);
                geometry.appendFace(faceBoundaryIri, isOrientation);
            } else {
                // Create a new geometry class
                geometry = new FacetedBrep(iri, faceBoundaryIri, isOrientation);
                // Add the object into the mappings for its IRI
                this.geomMappings.add(iri, geometry);
            }
        }
        // Construct all faceted brep' statements
        this.geomMappings.constructFacetedBrepStatements(statementSet);
    }

    /**
     * Creates the SPARQL SELECT query statements for ExtrudedAreaSolid.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private String createExtrudedAreaSolidSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        SelectBuilder subgroupBuilder = selectBuilder.clone();
        selectBuilder.addVar(CommonQuery.GEOM_VAR)
                .addVar(CommonQuery.CART_POINT_VAR)
                .addVar(CommonQuery.REF_DIR_VECTOR_VAR)
                .addVar(CommonQuery.AXIS_DIR_VECTOR_VAR)
                .addVar(CommonQuery.EXTRUDED_DIRECTION_VAR)
                .addVar(CommonQuery.DEPTH_VAR)
                .addVar(CommonQuery.PROFILE_DEF_CLASS_VAR)
                .addVar(CommonQuery.PROFILE_DEF_TYPE_VAR)
                .addVar(CommonQuery.PROFILE_DEF_CART_POINT_VAR)
                .addVar(CommonQuery.DIR_VECTOR_VAR)
                .addVar(CommonQuery.X_VALUE_VAR)
                .addVar(CommonQuery.Y_VALUE_VAR);
        selectBuilder.addWhere(CommonQuery.GEOM_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_EXTRUDED_AREA_SOLID)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_SWEPT_AREA_POSITION + "/" + CommonQuery.IFC_PLACEMENT_LOCATION, CommonQuery.CART_POINT_VAR)
                .addOptional(CommonQuery.GEOM_VAR, CommonQuery.IFC_SWEPT_AREA_POSITION + "/" + CommonQuery.IFC_REF_DIRECTION_3D, CommonQuery.REF_DIR_VECTOR_VAR)
                .addOptional(CommonQuery.GEOM_VAR, CommonQuery.IFC_SWEPT_AREA_POSITION + "/" + CommonQuery.IFC_AXIS_DIRECTION_3D, CommonQuery.AXIS_DIR_VECTOR_VAR)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_EXTRUDED_DIRECTION, CommonQuery.EXTRUDED_DIRECTION_VAR)
                .addWhere(CommonQuery.EXTRUDED_DIRECTION_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_DIRECTION)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_EXTRUDED_DEPTH + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.DEPTH_VAR)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_EXTRUDED_PROFILE_AREA, CommonQuery.PROFILE_DEF_VAR)
                .addWhere(CommonQuery.PROFILE_DEF_VAR, QueryHandler.RDF_TYPE, CommonQuery.PROFILE_DEF_CLASS_VAR);
        // NOTE for future:
        // Profile definition can be rectangle, circle, and other shapes. But our sample models only use rectangles
        // When more profile types are provided, please update the query below to add UNION and combine the potential profile queried
        subgroupBuilder.addWhere(CommonQuery.PROFILE_DEF_VAR, CommonQuery.IFC_PROFILE_TYPE, CommonQuery.PROFILE_DEF_TYPE_VAR)
                .addWhere(CommonQuery.PROFILE_DEF_VAR, CommonQuery.IFC_PROFILE_POSITION + "/" + CommonQuery.IFC_PLACEMENT_LOCATION, CommonQuery.PROFILE_DEF_CART_POINT_VAR)
                .addOptional(CommonQuery.PROFILE_DEF_VAR, CommonQuery.IFC_PROFILE_POSITION + "/" + CommonQuery.IFC_REF_DIRECTION_2D, CommonQuery.DIR_VECTOR_VAR)
                .addWhere(CommonQuery.PROFILE_DEF_VAR, CommonQuery.IFC_PROFILE_RECTANGLE_X_DIM + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.X_VALUE_VAR)
                .addWhere(CommonQuery.PROFILE_DEF_VAR, CommonQuery.IFC_PROFILE_RECTANGLE_Y_DIM + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.Y_VALUE_VAR);
        selectBuilder.addWhere(subgroupBuilder);
        return selectBuilder.buildString();
    }

    /**
     * Retrieve the OntoBIM statements for ExtrudedAreaSolid.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addExtrudedAreaSolidStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String query = createExtrudedAreaSolidSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.GEOM_VAR).toString();
            String extrusionDepth = QueryHandler.retrieveLiteral(soln, CommonQuery.DEPTH_VAR);
            // If the retrieved IRI is not null, retrieve the direction and update the IRI
            // Else, keep it as null for the placement object
            String extrudedDirIri = QueryHandler.retrieveIri(soln, CommonQuery.EXTRUDED_DIRECTION_VAR);
            if (extrudedDirIri != null) {
                DirectionVector extrudedDir = this.operatorMappings.getDirectionVector(extrudedDirIri);
                extrudedDirIri = extrudedDir.getIri();
            }
            // Generate the extruded position from the queried points and directions and construct its statements
            String cartPointIri = QueryHandler.retrieveIri(soln, CommonQuery.CART_POINT_VAR);
            if (cartPointIri != null) {
                CartesianPoint point = this.operatorMappings.getPoint(cartPointIri);
                cartPointIri = point.getIri();
            }
            String refDirIri = QueryHandler.retrieveIri(soln, CommonQuery.REF_DIR_VECTOR_VAR);
            if (refDirIri != null) {
                DirectionVector refDir = this.operatorMappings.getDirectionVector(refDirIri);
                refDirIri = refDir.getIri();
            }
            String axisDirIri = QueryHandler.retrieveIri(soln, CommonQuery.AXIS_DIR_VECTOR_VAR);
            if (axisDirIri != null) {
                DirectionVector axisDir = this.operatorMappings.getDirectionVector(axisDirIri);
                axisDirIri = axisDir.getIri();
            }
            LocalPlacement extrudedPosition = new LocalPlacement(cartPointIri, refDirIri, axisDirIri);
            extrudedPosition.constructStatements(statementSet);
            // Determine which profile should be generated based on the class
            String profileClass = QueryHandler.retrieveIri(soln, CommonQuery.PROFILE_DEF_CLASS_VAR);
            String profileIri = null; // defaults to null if there is no relevant profile
            if (profileClass.contains("IfcRectangleProfileDef")) {
                String profileType = QueryHandler.retrieveIri(soln, CommonQuery.PROFILE_DEF_TYPE_VAR);
                String xDimExtent = QueryHandler.retrieveLiteral(soln, CommonQuery.X_VALUE_VAR);
                String yDimExtent = QueryHandler.retrieveLiteral(soln, CommonQuery.Y_VALUE_VAR);
                // For the profile position
                String profilePointIri = QueryHandler.retrieveIri(soln, CommonQuery.PROFILE_DEF_CART_POINT_VAR);
                if (profilePointIri != null) {
                    CartesianPoint profilePoint = this.operatorMappings.getPoint(profilePointIri);
                    profilePointIri = profilePoint.getIri();
                }
                String profileDirIri = QueryHandler.retrieveIri(soln, CommonQuery.DIR_VECTOR_VAR);
                if (profileDirIri != null) {
                    DirectionVector profileDir = this.operatorMappings.getDirectionVector(profileDirIri);
                    profileDirIri = profileDir.getIri();
                }
                LocalPlacement profilePosition = new LocalPlacement(profilePointIri, profileDirIri, null);
                profilePosition.constructStatements(statementSet);
                // Generate a new profile and construct the statements
                RectangleProfileDefinition profile = new RectangleProfileDefinition(profileType, profilePosition.getIri(), xDimExtent, yDimExtent);
                profile.constructStatements(statementSet);
                profileIri = profile.getIri();
            }
            // Generate a new extruded area solid and construct the statements
            ExtrudedAreaSolid geometry = new ExtrudedAreaSolid(iri, extrudedPosition.getIri(), extrudedDirIri, extrusionDepth, profileIri);
            geometry.constructStatements(statementSet);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for PolygonalBoundedHalfSpace.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private String createHalfSpaceSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.GEOM_VAR)
                .addVar(CommonQuery.CART_POINT_VAR)
                .addVar(CommonQuery.REF_DIR_VECTOR_VAR)
                .addVar(CommonQuery.AXIS_DIR_VECTOR_VAR)
                .addVar(CommonQuery.SEC_CART_POINT_VAR)
                .addVar(CommonQuery.SEC_REF_DIRECTION_VAR)
                .addVar(CommonQuery.SEC_AXIS_DIRECTION_VAR)
                .addVar(CommonQuery.POLYLINE_VAR)
                .addVar(CommonQuery.BOOLEAN_VAR);
        selectBuilder.addWhere(CommonQuery.GEOM_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_POLYGONAL_HALF_SPACE)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_HALF_SPACE_FLAG + CommonQuery.EXPRESS_HASBOOLEAN, CommonQuery.BOOLEAN_VAR)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_HALF_SPACE_BOUNDARY, CommonQuery.POLYLINE_VAR)
                .addWhere(CommonQuery.POLYLINE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_POLYLINE)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_HALF_SPACE_POSITION + "/" + CommonQuery.IFC_PLACEMENT_LOCATION, CommonQuery.CART_POINT_VAR)
                .addOptional(CommonQuery.GEOM_VAR, CommonQuery.IFC_HALF_SPACE_POSITION + "/" + CommonQuery.IFC_REF_DIRECTION_3D, CommonQuery.REF_DIR_VECTOR_VAR)
                .addOptional(CommonQuery.GEOM_VAR, CommonQuery.IFC_HALF_SPACE_POSITION + "/" + CommonQuery.IFC_AXIS_DIRECTION_3D, CommonQuery.AXIS_DIR_VECTOR_VAR)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_HALF_SPACE_SURFACE, CommonQuery.SURFACE_VAR)
                .addWhere(CommonQuery.SURFACE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_SURFACE_PLANE)
                .addWhere(CommonQuery.SURFACE_VAR, CommonQuery.IFC_HALF_SPACE_SURFACE_POSITION + "/" + CommonQuery.IFC_PLACEMENT_LOCATION, CommonQuery.SEC_CART_POINT_VAR)
                .addOptional(CommonQuery.SURFACE_VAR, CommonQuery.IFC_HALF_SPACE_SURFACE_POSITION + "/" + CommonQuery.IFC_REF_DIRECTION_3D, CommonQuery.SEC_REF_DIRECTION_VAR)
                .addOptional(CommonQuery.SURFACE_VAR, CommonQuery.IFC_HALF_SPACE_SURFACE_POSITION + "/" + CommonQuery.IFC_AXIS_DIRECTION_3D, CommonQuery.SEC_AXIS_DIRECTION_VAR);
        return selectBuilder.buildString();
    }

    /**
     * Retrieve the OntoBIM statements for PolygonalBoundedHalfSpace.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addHalfSpaceStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String query = createHalfSpaceSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            // Generate the position from the queried points and directions and construct its statements
            String cartPointIri = QueryHandler.retrieveIri(soln, CommonQuery.CART_POINT_VAR);
            if (cartPointIri != null) {
                CartesianPoint point = this.operatorMappings.getPoint(cartPointIri);
                cartPointIri = point.getIri();
            }
            String refDirIri = QueryHandler.retrieveIri(soln, CommonQuery.REF_DIR_VECTOR_VAR);
            if (refDirIri != null) {
                DirectionVector refDir = this.operatorMappings.getDirectionVector(refDirIri);
                refDirIri = refDir.getIri();
            }
            String axisDirIri = QueryHandler.retrieveIri(soln, CommonQuery.AXIS_DIR_VECTOR_VAR);
            if (axisDirIri != null) {
                DirectionVector axisDir = this.operatorMappings.getDirectionVector(axisDirIri);
                axisDirIri = axisDir.getIri();
            }
            LocalPlacement position = new LocalPlacement(cartPointIri, refDirIri, axisDirIri);
            position.constructStatements(statementSet);
            // For surface plane
            String planePointIri = QueryHandler.retrieveIri(soln, CommonQuery.SEC_CART_POINT_VAR);
            if (planePointIri != null) {
                CartesianPoint point = this.operatorMappings.getPoint(planePointIri);
                planePointIri = point.getIri();
            }
            String planeRefDirIri = QueryHandler.retrieveIri(soln, CommonQuery.SEC_REF_DIRECTION_VAR);
            if (planeRefDirIri != null) {
                DirectionVector refDir = this.operatorMappings.getDirectionVector(planeRefDirIri);
                planeRefDirIri = refDir.getIri();
            }
            String planeAxisDirIri = QueryHandler.retrieveIri(soln, CommonQuery.SEC_AXIS_DIRECTION_VAR);
            if (planeAxisDirIri != null) {
                DirectionVector axisDir = this.operatorMappings.getDirectionVector(planeAxisDirIri);
                planeAxisDirIri = axisDir.getIri();
            }
            LocalPlacement planePosition = new LocalPlacement(planePointIri, planeRefDirIri, planeAxisDirIri);
            planePosition.constructStatements(statementSet);
            // Generate the main geometry
            String iri = soln.get(CommonQuery.GEOM_VAR).toString();
            String boundaryIri = QueryHandler.retrieveIri(soln, CommonQuery.POLYLINE_VAR);
            boolean agreementFlag = Boolean.parseBoolean(QueryHandler.retrieveLiteral(soln, CommonQuery.BOOLEAN_VAR));
            PolygonalBoundedHalfSpace geometry = new PolygonalBoundedHalfSpace(iri, position.getIri(), planePosition.getIri(), boundaryIri, agreementFlag);
            geometry.constructStatements(statementSet);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for Polyline and PolyLoop.
     *
     * @param isPolyLoop A boolean indicating if this method should be executed as a PolyLoop or Polyline.
     * @return A string containing the SPARQL query to execute.
     */
    private String createPolylineSelectQuery(boolean isPolyLoop) {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.GEOM_VAR)
                .addVar(CommonQuery.FIRST_LINE_VERTEX_VAR)
                .addVar(CommonQuery.CART_POINT_VAR)
                .addVar(CommonQuery.LINE_VERTEX_VAR)
                .addVar(CommonQuery.NEXT_LINE_VERTEX_VAR);
        if (isPolyLoop) {
            selectBuilder.addWhere(CommonQuery.GEOM_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_POLYLOOP)
                    .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_POLYLOOP_POLYGON, CommonQuery.FIRST_LINE_VERTEX_VAR);
        } else {
            selectBuilder.addWhere(CommonQuery.GEOM_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_POLYLINE)
                    .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_POLYLINE_POINTS, CommonQuery.FIRST_LINE_VERTEX_VAR);
        }
        selectBuilder.addWhere(CommonQuery.FIRST_LINE_VERTEX_VAR, "(" + CommonQuery.LIST_HAS_NEXT + ")*", CommonQuery.LINE_VERTEX_VAR)
                .addWhere(CommonQuery.LINE_VERTEX_VAR, CommonQuery.LIST_HAS_CONTENT, CommonQuery.CART_POINT_VAR)
                .addOptional(CommonQuery.LINE_VERTEX_VAR, CommonQuery.LIST_HAS_NEXT, CommonQuery.NEXT_LINE_VERTEX_VAR);
        return selectBuilder.buildString();
    }

    /**
     * Retrieve the OntoBIM statements for PolyLoop.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     * @param isPolyLoop   A boolean indicating if this method should be executed as a PolyLoop or Polyline.
     */
    public void addPolylineStatements(Model owlModel, LinkedHashSet<Statement> statementSet, boolean isPolyLoop) {
        String query = createPolylineSelectQuery(isPolyLoop);
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.GEOM_VAR).toString();
            String startingVertex = QueryHandler.retrieveIri(soln, CommonQuery.FIRST_LINE_VERTEX_VAR);
            String currentVertex = QueryHandler.retrieveIri(soln, CommonQuery.LINE_VERTEX_VAR);
            // Next vertex is required to link the vertices as the queries returned are not in order
            String nextVertex = QueryHandler.retrieveIri(soln, CommonQuery.NEXT_LINE_VERTEX_VAR);
            // If the retrieved IRI is not null, retrieve the points and update the IRI
            String currentPointIri = QueryHandler.retrieveIri(soln, CommonQuery.CART_POINT_VAR);
            if (currentPointIri != null) {
                currentPointIri = this.operatorMappings.getPoint(currentPointIri).getIri();
            }
            Polyline geometry;
            // If the geometry has already been created previously
            if (this.geomMappings.containsIri(iri)) {
                // Retrieve the new geometry IRI and append the current vertex details
                geometry = this.geomMappings.getPolyline(iri);
                geometry.appendVertex(currentVertex, currentPointIri, nextVertex);
            } else {
                // Generate a new polyline or poly loop depending on the class and construct the statements
                geometry = isPolyLoop ? new Polyline(iri, startingVertex, currentVertex, currentPointIri, nextVertex, OntoBimConstant.POLYLOOP_CLASS) :
                        new Polyline(iri, startingVertex, currentVertex, currentPointIri, nextVertex);
                // Add the object into the mappings for its IRI
                this.geomMappings.add(iri, geometry);
            }
        }
        // Construct all poly lines' statements
        this.geomMappings.constructPolyLineStatements(statementSet);
        // Reset the geom mappings for poline and poly loop
        this.geomMappings = GeometryStorage.resetSingleton();
    }

    /**
     * Creates the SPARQL SELECT query statements for BooleanClippingResult.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private String createBooleanClippingResultSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.GEOM_VAR)
                .addVar(CommonQuery.OPERATOR_VAR)
                .addVar(CommonQuery.FIRST_OPERAND_VAR)
                .addVar(CommonQuery.SEC_OPERAND_VAR);
        selectBuilder.addWhere(CommonQuery.GEOM_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_CLIPPING_RESULT)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_CLIPPING_RESULT_OPERATOR, CommonQuery.OPERATOR_VAR)
                .addWhere(CommonQuery.GEOM_VAR, CommonQuery.IFC_CLIPPING_RESULT_FIRST_OPERAND, CommonQuery.FIRST_OPERAND_VAR)
                .addOptional(CommonQuery.GEOM_VAR, CommonQuery.IFC_CLIPPING_RESULT_SEC_OPERAND, CommonQuery.SEC_OPERAND_VAR);
        return selectBuilder.buildString();
    }

    /**
     * Retrieve the OntoBIM statements for BooleanClippingResult.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addBooleanClippingResultStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String query = createBooleanClippingResultSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.GEOM_VAR).toString();
            String operator = QueryHandler.retrieveIri(soln, CommonQuery.OPERATOR_VAR);
            String firstGeomIri = QueryHandler.retrieveIri(soln, CommonQuery.FIRST_OPERAND_VAR);
            String secGeomIri = QueryHandler.retrieveIri(soln, CommonQuery.SEC_OPERAND_VAR);
            BooleanClippingResult geometry = new BooleanClippingResult(iri, operator, firstGeomIri, secGeomIri);
            geometry.constructStatements(statementSet);
        }
    }
}
