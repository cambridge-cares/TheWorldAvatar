package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.CartesianPoint;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.DirectionVector;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.model.LocalPlacement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.CommonQuery;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ModellingOperatorStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.QueryHandler;

/**
 * Provides functions to generate the modelling operators' triples.
 *
 * @author qhouyee
 */
public class ModellingOperatorFacade {
    private static ModellingOperatorStorage mappings;


    /**
     * Generate the modelling operators' triples.
     *
     * @param owlModel The IfcOwl model containing the triples to query from.
     */
    public static void retrieveOperatorInstances(Model owlModel) {
        mappings = ModellingOperatorStorage.Singleton();
        execPointQuery(owlModel);
        execDirectionQuery(owlModel);
        mappings.replaceDuplicates();
        execLocalPlacementQuery(owlModel);
    }

    /**
     * Creates the SPARQL SELECT query statements for CartesianPoint.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createCartesianPointSelectQuery() {
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
    private static void execPointQuery(Model owlModel) {
        String query = createCartesianPointSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.CART_POINT_VAR).toString();
            String xCoord = QueryHandler.retrieveLiteral(soln, CommonQuery.X_VALUE_VAR);
            String yCoord = QueryHandler.retrieveLiteral(soln, CommonQuery.Y_VALUE_VAR);
            String zCoord = QueryHandler.retrieveLiteral(soln, CommonQuery.Z_VALUE_VAR);
            CartesianPoint point = new CartesianPoint(iri, xCoord, yCoord, zCoord);
            mappings.add(iri, point);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for DirectionVector.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createDirectionVectorSelectQuery() {
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
    private static void execDirectionQuery(Model owlModel) {
        String query = createDirectionVectorSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.DIR_VECTOR_VAR).toString();
            String xDirRatio = QueryHandler.retrieveLiteral(soln, CommonQuery.X_VALUE_VAR);
            String yDirRatio = QueryHandler.retrieveLiteral(soln, CommonQuery.Y_VALUE_VAR);
            String zDirRatio = QueryHandler.retrieveLiteral(soln, CommonQuery.Z_VALUE_VAR);
            DirectionVector direction = new DirectionVector(iri, xDirRatio, yDirRatio, zDirRatio);
            mappings.add(iri, direction);
        }
    }

    /**
     * Creates the SPARQL SELECT query statements for LocalPlacement.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createLocalPlacementSelectQuery() {
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
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for LocalPlacement.
     *
     * @param owlModel The IfcOwl model containing the triples to query from.
     */
    private static void execLocalPlacementQuery(Model owlModel) {
        String query = createLocalPlacementSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.PLACEMENT_VAR).toString();
            String cartPointIri = QueryHandler.retrieveIri(soln, CommonQuery.CART_POINT_VAR);
            // If the retrieved IRI is not null, retrieve the point and update the IRI
            // Else, keep it as null for the placement object
            if (cartPointIri!= null){
                CartesianPoint point = mappings.getPoint(cartPointIri);
                cartPointIri = point.getIri();
            }
            String refDirIri = QueryHandler.retrieveIri(soln, CommonQuery.REF_DIR_VECTOR_VAR);
            if (refDirIri!= null){
                DirectionVector refDir = mappings.getDirectionVector(refDirIri);
                refDirIri = refDir.getIri();
            }
            String axisDirIri = QueryHandler.retrieveIri(soln, CommonQuery.AXIS_DIR_VECTOR_VAR);
            if (axisDirIri!= null){
                DirectionVector axisDir = mappings.getDirectionVector(axisDirIri);
                axisDirIri = axisDir.getIri();
            }
            String relPlacementIri = QueryHandler.retrieveIri(soln, CommonQuery.REL_PLACEMENT_VAR);
            LocalPlacement placement = new LocalPlacement(iri, cartPointIri, refDirIri, axisDirIri, relPlacementIri);
            mappings.add(placement);
        }
    }
}
