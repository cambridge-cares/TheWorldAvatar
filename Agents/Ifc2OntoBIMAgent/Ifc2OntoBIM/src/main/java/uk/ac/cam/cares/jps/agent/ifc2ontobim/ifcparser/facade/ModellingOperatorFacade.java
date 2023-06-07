package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.*;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.CommonQuery;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ModellingOperatorStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.QueryHandler;

import java.util.LinkedHashSet;

/**
 * Provides functions to generate the modelling operators' triples.
 *
 * @author qhouyee
 */
public class ModellingOperatorFacade {
    private final ModellingOperatorStorage mappings;


    /**
     * Standard Constructor setting up the mappings.
     *
     * @param owlModel The IfcOwl model containing the triples to query from.
     */
    public ModellingOperatorFacade(Model owlModel) {
        // Create the mappings
        this.mappings = ModellingOperatorStorage.Singleton();
        // Query and store all points and directions in the mappings
        execPointQuery(owlModel);
        execDirectionQuery(owlModel);
        // Replace all duplicates
        mappings.replaceDuplicates();
    }


    /**
     * Creates the SPARQL SELECT query statements for CartesianPoint.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private String createCartesianPointSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.CART_POINT_VAR)
                .addVar(CommonQuery.X_VALUE_VAR)
                .addVar(CommonQuery.Y_VALUE_VAR)
                .addVar(CommonQuery.Z_VALUE_VAR);
        selectBuilder.addWhere(CommonQuery.CART_POINT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_CART_POINT)
                .addWhere(CommonQuery.CART_POINT_VAR, CommonQuery.IFC_COORDINATES, CommonQuery.X_LIST_VAR)
                .addWhere(CommonQuery.X_LIST_VAR, CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.X_VALUE_VAR)
                .addWhere(CommonQuery.X_LIST_VAR, CommonQuery.LIST_HAS_NEXT, CommonQuery.Y_LIST_VAR)
                .addWhere(CommonQuery.Y_LIST_VAR, CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.Y_VALUE_VAR)
                // Z-coordinates are optional in Ifc
                .addOptional(CommonQuery.Y_LIST_VAR, CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.Z_VALUE_VAR);
        return selectBuilder.buildString();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for CartesianPoint.
     *
     * @param owlModel The IfcOwl model containing the triples to query from.
     */
    private void execPointQuery(Model owlModel) {
        String query = createCartesianPointSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.CART_POINT_VAR).toString();
            String xCoord = QueryHandler.retrieveLiteral(soln, CommonQuery.X_VALUE_VAR);
            String yCoord = QueryHandler.retrieveLiteral(soln, CommonQuery.Y_VALUE_VAR);
            String zCoord = QueryHandler.retrieveLiteral(soln, CommonQuery.Z_VALUE_VAR);
            CartesianPoint point = new CartesianPoint(xCoord, yCoord, zCoord);
            this.mappings.add(iri, point);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for DirectionVector.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private String createDirectionVectorSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.DIR_VECTOR_VAR)
                .addVar(CommonQuery.X_VALUE_VAR)
                .addVar(CommonQuery.Y_VALUE_VAR)
                .addVar(CommonQuery.Z_VALUE_VAR);
        selectBuilder.addWhere(CommonQuery.DIR_VECTOR_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_DIRECTION)
                .addWhere(CommonQuery.DIR_VECTOR_VAR, CommonQuery.IFC_DIR_RATIOS, CommonQuery.X_LIST_VAR)
                .addWhere(CommonQuery.X_LIST_VAR, CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.X_VALUE_VAR)
                .addWhere(CommonQuery.X_LIST_VAR, CommonQuery.LIST_HAS_NEXT, CommonQuery.Y_LIST_VAR)
                .addWhere(CommonQuery.Y_LIST_VAR, CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.Y_VALUE_VAR)
                // Z-coordinates are optional in Ifc
                .addOptional(CommonQuery.Y_LIST_VAR, CommonQuery.LIST_HAS_NEXT + "/" + CommonQuery.LIST_HAS_CONTENT + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.Z_VALUE_VAR);
        return selectBuilder.buildString();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for DirectionVector.
     *
     * @param owlModel The IfcOwl model containing the triples to query from.
     */
    private void execDirectionQuery(Model owlModel) {
        String query = createDirectionVectorSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.DIR_VECTOR_VAR).toString();
            String xDirRatio = QueryHandler.retrieveLiteral(soln, CommonQuery.X_VALUE_VAR);
            String yDirRatio = QueryHandler.retrieveLiteral(soln, CommonQuery.Y_VALUE_VAR);
            String zDirRatio = QueryHandler.retrieveLiteral(soln, CommonQuery.Z_VALUE_VAR);
            DirectionVector direction = new DirectionVector(xDirRatio, yDirRatio, zDirRatio);
            this.mappings.add(iri, direction);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for LocalPlacement.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private String createLocalPlacementSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        SelectBuilder unionBuilder = selectBuilder.clone();
        SelectBuilder subgroupBuilder = selectBuilder.clone();
        selectBuilder.addVar(CommonQuery.PLACEMENT_VAR)
                .addVar(CommonQuery.CART_POINT_VAR)
                .addVar(CommonQuery.REF_DIR_VECTOR_VAR)
                .addVar(CommonQuery.AXIS_DIR_VECTOR_VAR)
                .addVar(CommonQuery.REL_PLACEMENT_VAR);
        selectBuilder.addWhere(CommonQuery.PLACEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCLOCALPLACEMENT)
                // All Ifc elements have a relative position to a parent element's placement except for IfcSite
                .addOptional(CommonQuery.PLACEMENT_VAR, CommonQuery.IFC_REL_PLACEMENT, CommonQuery.REL_PLACEMENT_VAR)
                .addWhere(CommonQuery.PLACEMENT_VAR, CommonQuery.IFC_PLACEMENT_POSITION, CommonQuery.MODEL_PLACEMENT_VAR)
                .addWhere(CommonQuery.MODEL_PLACEMENT_VAR, CommonQuery.IFC_PLACEMENT_LOCATION, CommonQuery.CART_POINT_VAR)
                .addWhere(CommonQuery.CART_POINT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_CART_POINT);
        // Adds a subgroup with Union pattern as the placement transformer may be either 2D or 3D with different properties
        subgroupBuilder.addWhere(CommonQuery.MODEL_PLACEMENT_VAR, CommonQuery.IFC_AXIS_DIRECTION_3D, CommonQuery.AXIS_DIR_VECTOR_VAR)
                .addWhere(CommonQuery.AXIS_DIR_VECTOR_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_DIRECTION)
                .addWhere(CommonQuery.MODEL_PLACEMENT_VAR, CommonQuery.IFC_REF_DIRECTION_3D, CommonQuery.REF_DIR_VECTOR_VAR)
                .addWhere(CommonQuery.REF_DIR_VECTOR_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_DIRECTION);
        unionBuilder.addWhere(CommonQuery.MODEL_PLACEMENT_VAR, CommonQuery.IFC_REF_DIRECTION_2D, CommonQuery.REF_DIR_VECTOR_VAR)
                .addWhere(CommonQuery.REF_DIR_VECTOR_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_DIRECTION);
        subgroupBuilder.addUnion(unionBuilder);
        selectBuilder.addOptional(subgroupBuilder);
        return selectBuilder.buildString();
    }

    /**
     * Retrieve the OntoBIM statements for LocalPlacement.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addLocalPlacementStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String query = createLocalPlacementSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.PLACEMENT_VAR).toString();
            String cartPointIri = QueryHandler.retrieveIri(soln, CommonQuery.CART_POINT_VAR);
            // If the retrieved IRI is not null, retrieve the point and update the IRI
            // Else, keep it as null for the placement object
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
            String relPlacementIri = QueryHandler.retrieveIri(soln, CommonQuery.REL_PLACEMENT_VAR);
            LocalPlacement placement = new LocalPlacement(iri, cartPointIri, refDirIri, axisDirIri, relPlacementIri);
            placement.constructStatements(statementSet);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for CartesianTransformationOperator.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private String createCartesianTransformationOperatorSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.CART_TRANSFORMER_VAR)
                .addVar(CommonQuery.CART_POINT_VAR)
                .addVar(CommonQuery.SCALE_VAR)
                .addVar(CommonQuery.X_AXIS_DIR_VECTOR_VAR)
                .addVar(CommonQuery.Y_AXIS_DIR_VECTOR_VAR);
        selectBuilder.addWhere(CommonQuery.CART_TRANSFORMER_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_CART_TRANSFORMATION_OPERATOR)
                .addWhere(CommonQuery.CART_TRANSFORMER_VAR, CommonQuery.IFC_ORIGIN_TRANSFORMATION, CommonQuery.CART_POINT_VAR)
                .addWhere(CommonQuery.CART_POINT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_CART_POINT)
                .addOptional(CommonQuery.CART_TRANSFORMER_VAR, CommonQuery.IFC_SCALE_TRANSFORMATION + CommonQuery.EXPRESS_HASDOUBLE, CommonQuery.SCALE_VAR)
                .addOptional(CommonQuery.CART_TRANSFORMER_VAR, CommonQuery.IFC_X_AXIS_TRANSFORMATION, CommonQuery.X_AXIS_DIR_VECTOR_VAR)
                .addOptional(CommonQuery.CART_TRANSFORMER_VAR, CommonQuery.IFC_Y_AXIS_TRANSFORMATION, CommonQuery.Y_AXIS_DIR_VECTOR_VAR);
        return selectBuilder.buildString();
    }

    /**
     * Executes the OntoBIM statements for CartesianTransformationOperator.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addCartesianTransformationOperatorStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String query = createCartesianTransformationOperatorSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.CART_TRANSFORMER_VAR).toString();
            String scaleFactor = QueryHandler.retrieveLiteral(soln, CommonQuery.SCALE_VAR);
            String originIri = QueryHandler.retrieveIri(soln, CommonQuery.CART_POINT_VAR);
            // If the retrieved IRI is not null, retrieve the point and update the IRI
            // Else, keep it as null for the placement object
            if (originIri != null) {
                CartesianPoint point = this.mappings.getPoint(originIri);
                originIri = point.getIri();
            }
            String xDirIri = QueryHandler.retrieveIri(soln, CommonQuery.X_AXIS_DIR_VECTOR_VAR);
            if (xDirIri != null) {
                DirectionVector xAxisDirection = this.mappings.getDirectionVector(xDirIri);
                xDirIri = xAxisDirection.getIri();
            }
            String yDirIri = QueryHandler.retrieveIri(soln, CommonQuery.Y_AXIS_DIR_VECTOR_VAR);
            if (yDirIri != null) {
                DirectionVector yAxisDirection = this.mappings.getDirectionVector(yDirIri);
                yDirIri = yAxisDirection.getIri();
            }
            CartesianTransformationOperator operator = new CartesianTransformationOperator(iri, originIri, scaleFactor, xDirIri, yDirIri);
            operator.constructStatements(statementSet);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for GeometricRepresentationSubContext.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createGeometricRepresentationSubContextSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.REP_SUBCONTEXT_VAR)
                .addVar(CommonQuery.REP_CONTEXT_VAR)
                .addVar(CommonQuery.CONTEXT_TYPE_VAR)
                .addVar(CommonQuery.CONTEXT_IDENTIFIER_VAR)
                .addVar(CommonQuery.CONTEXT_VIEW_VAR);
        selectBuilder.addWhere(CommonQuery.REP_SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCGEOM_REP_SUBCONTEXT)
                .addWhere(CommonQuery.REP_SUBCONTEXT_VAR, CommonQuery.IFC_PARENT_CONTEXT, CommonQuery.REP_CONTEXT_VAR)
                .addWhere(CommonQuery.REP_CONTEXT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCGEOM_REP_CONTEXT)
                .addOptional(CommonQuery.REP_SUBCONTEXT_VAR, CommonQuery.IFC_CONTEXT_TYPE + CommonQuery.EXPRESS_HASSTRING, CommonQuery.CONTEXT_TYPE_VAR)
                .addOptional(CommonQuery.REP_SUBCONTEXT_VAR, CommonQuery.IFC_CONTEXT_IDENTIFIER + CommonQuery.EXPRESS_HASSTRING, CommonQuery.CONTEXT_IDENTIFIER_VAR)
                .addOptional(CommonQuery.REP_SUBCONTEXT_VAR, CommonQuery.IFC_TARGET_VIEW, CommonQuery.CONTEXT_VIEW_VAR);
        return selectBuilder.buildString();
    }

    /**
     * Executes the OntoBIM statements for GeometricRepresentationSubContext.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addGeometricRepresentationSubContextStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String query = createGeometricRepresentationSubContextSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.REP_SUBCONTEXT_VAR).toString();
            String parentContextIri = QueryHandler.retrieveIri(soln, CommonQuery.REP_CONTEXT_VAR);
            String type = QueryHandler.retrieveLiteral(soln, CommonQuery.CONTEXT_TYPE_VAR);
            String identifier = QueryHandler.retrieveLiteral(soln, CommonQuery.CONTEXT_IDENTIFIER_VAR);
            String view = QueryHandler.retrieveIri(soln, CommonQuery.CONTEXT_VIEW_VAR);
            GeometricRepresentationSubContext subcontext = new GeometricRepresentationSubContext(iri, parentContextIri, type, identifier, view);
            subcontext.constructStatements(statementSet);
        }
    }
}
