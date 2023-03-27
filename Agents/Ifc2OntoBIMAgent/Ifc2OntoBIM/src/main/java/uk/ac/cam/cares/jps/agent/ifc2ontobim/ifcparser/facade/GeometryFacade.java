package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ExtrudedAreaSolid;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.RectangleProfileDefinition;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.*;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.CommonQuery;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ModellingOperatorStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.QueryHandler;

import java.util.LinkedHashSet;

/**
 * Provides functions to generate the geometry' triples.
 *
 * @author qhouyee
 */
public class GeometryFacade {
    private final ModellingOperatorStorage mappings;


    /**
     * Standard Constructor setting up the mappings.
     */
    public GeometryFacade() {
        this.mappings = ModellingOperatorStorage.Singleton();
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
                DirectionVector extrudedDir = this.mappings.getDirectionVector(extrudedDirIri);
                extrudedDirIri = extrudedDir.getIri();
            }
            // Generate the extruded position from the queried points and directions and construct its statements
            String cartPointIri = QueryHandler.retrieveIri(soln, CommonQuery.CART_POINT_VAR);
            if (cartPointIri != null) {
                CartesianPoint point = this.mappings.getPoint(cartPointIri);
                cartPointIri = point.getIri();
            }
            String refDirIri = QueryHandler.retrieveIri(soln, CommonQuery.REF_DIR_VECTOR_VAR);
            if (refDirIri != null) {
                DirectionVector refDir = this.mappings.getDirectionVector(refDirIri);
                refDirIri = refDir.getIri();
            }
            String axisDirIri = QueryHandler.retrieveIri(soln, CommonQuery.AXIS_DIR_VECTOR_VAR);
            if (axisDirIri != null) {
                DirectionVector axisDir = this.mappings.getDirectionVector(axisDirIri);
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
                    CartesianPoint profilePoint = this.mappings.getPoint(profilePointIri);
                    profilePointIri = profilePoint.getIri();
                }
                String profileDirIri = QueryHandler.retrieveIri(soln, CommonQuery.DIR_VECTOR_VAR);
                if (profileDirIri != null) {
                    DirectionVector profileDir = this.mappings.getDirectionVector(profileDirIri);
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
}
