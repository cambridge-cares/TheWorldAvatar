package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.GeometricVoid;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ModelRepresentation3D;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.CommonQuery;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ElementStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.SpatialZoneStorage;

import java.util.*;

/**
 * Provides the functions to initialise a SPARQL query builder, execute all SPARQL query types, and provides common properties.
 *
 * @author qhouyee
 */
public class QueryHandler {
    public static final String RDF_TYPE = "rdf:type";
    public static final String RDFS_LABEL = "rdfs:label";

    /**
     * Initialise a SPARQL SELECT query builder.
     *
     * @return The SELECT builder.
     */
    public static SelectBuilder initSelectQueryBuilder() {
        SelectBuilder selectBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(selectBuilder);
        selectBuilder.setDistinct(true);
        return selectBuilder;
    }

    /**
     * Executes the SPARQL SELECT query on the new Model.
     *
     * @param queryString A string containing the required SELECT statements.
     * @param queryModel  The query model to retrieve statements from.
     * @return The query results for further processing.
     */
    public static ResultSet execSelectQuery(String queryString, Model queryModel) {
        Query query = QueryFactory.create(queryString);
        try (QueryExecution qExec = QueryExecutionFactory.create(query, queryModel)) {
            return ResultSetFactory.copyResults(qExec.execSelect());
        }
    }

    /**
     * Executes a Sparql Construct query and add the statements returned into a Linked Hash Set.
     *
     * @param queryString  A string containing the SPARQL query.
     * @param queryModel   The query model to retrieve statements from.
     * @param statementSet An ordered set to store the statements from the query results.
     */
    public static void queryConstructStatementsAsSet(String queryString, Model queryModel, LinkedHashSet<Statement> statementSet) {
        Query query = QueryFactory.create(queryString);
        try (QueryExecution qExec = QueryExecutionFactory.create(query, queryModel)) {
            Model results = qExec.execConstruct();
            results.listStatements().forEach(statementSet::add);
        }
    }

    /**
     * Retrieves the IRI as a string from a row of Jena Result Set if it exists.
     *
     * @param soln     The row of Jena result set to retrieve information from.
     * @param variable The variable name of interest.
     * @return The IRI as a string.
     */
    public static String retrieveIri(QuerySolution soln, String variable) {
        if (soln.contains(variable)) {
            return soln.get(variable).toString();
        } else {
            return null;
        }
    }

    /**
     * Retrieves literal as a string from a row of Jena Result Set if it exists.
     *
     * @param soln     The row of Jena result set to retrieve information from.
     * @param variable The variable name of interest.
     * @return The value of interest as a string.
     */
    public static String retrieveLiteral(QuerySolution soln, String variable) {
        if (soln.contains(variable)) {
            // Retrieve only the literal value with getString(), do not retrieve the namespace
            return soln.getLiteral(variable).getString();
        } else {
            return null;
        }
    }

    /**
     * Retrieve the IRI of the zone hosting this element, usually in the Storey or Room concept.
     *
     * @param soln         The row of the query response to retrieve this zone IRI.
     * @param zoneMappings The mappings of the zone's IfcOwl IRI to their class object instantiated.
     */
    public static String retrieveHostZone(QuerySolution soln, SpatialZoneStorage zoneMappings) {
        String zoneIri = retrieveIri(soln, CommonQuery.PARENT_ZONE_VAR);
        if (zoneIri.contains(CommonQuery.IFCSTOREY_CLASS)) {
            return zoneMappings.getStorey(zoneIri).getBotStoreyIRI();
        }
        return zoneMappings.getRoom(zoneIri).getBimRoomIRI();
    }

    /**
     * Retrieve the geometry model representation of the element as a ModelRepresentation3D class.
     *
     * @param soln The row of the query response.
     */
    public static ModelRepresentation3D retrieveModelRepresentation3D(QuerySolution soln) {
        String shapeRepIri = soln.get(CommonQuery.INST_SHAPE_REP_VAR).toString();
        String subContextIri = soln.get(CommonQuery.REP_SUBCONTEXT_VAR).toString();
        String geomIri = retrieveIri(soln, CommonQuery.GEOM_VAR);
        String shapeRepType = retrieveLiteral(soln, CommonQuery.INST_SHAPE_REP_TYPE_VAR);
        String sourcePlacementIri = retrieveIri(soln, CommonQuery.GEOM_AXIS_PLACEMENT_VAR);
        String cartesianTransformerIri = retrieveIri(soln, CommonQuery.CART_TRANSFORMER_VAR);
        return new ModelRepresentation3D(shapeRepIri, subContextIri, geomIri,
                shapeRepType, sourcePlacementIri, cartesianTransformerIri);
    }

    /**
     * An overloaded method to retrieve the geometry model representation of the element or geometric void
     * depending on the input variables that is called.
     *
     * @param soln The row of the query response.
     */
    public static ModelRepresentation3D retrieveModelRepresentation3D(QuerySolution soln, String shapeRepVar,
                                                                      String subContextVar, String geomVar, String shapeRepTypeVar) {
        String shapeRepIri = retrieveIri(soln, shapeRepVar);
        String subContextIri = retrieveIri(soln, subContextVar);
        String geomIri = retrieveIri(soln, geomVar);
        String shapeRepType = retrieveLiteral(soln, shapeRepTypeVar);
        return new ModelRepresentation3D(shapeRepIri, subContextIri, geomIri,
                shapeRepType, null, null);
    }

    /**
     * Retrieve the second geometry model representation of the element as a ModelRepresentation3D class.
     * Adds the void model representation 3D into the mappings to construct its statements.
     *
     * @param soln              The row of the query response.
     * @param elementIRI        The IRI of the element containing the void.
     * @param statementSet      A list containing the new OntoBIM triples.
     * @param modelRepMappings  The mappings for the model representation and their void IRI.
     */
    public static void addVoidGeometryStatements(QuerySolution soln, String elementIRI, LinkedHashSet<Statement> statementSet, ElementStorage modelRepMappings) {
        if (soln.contains(CommonQuery.VOID_SHAPE_REP_VAR)) {
            // Retrieve the common void parameters
            String voidPlacementIRI = retrieveIri(soln, CommonQuery.VOID_PLACEMENT_VAR);
            String voidType = retrieveLiteral(soln, CommonQuery.VOID_TYPE_VAR);
            // Retrieve the Model Representation 3D parameters for the void
            ModelRepresentation3D voidRep = retrieveModelRepresentation3D(soln, CommonQuery.VOID_SHAPE_REP_VAR,
                    CommonQuery.VOID_SUB_CONTEXT_VAR, CommonQuery.VOID_GEOM_VAR, CommonQuery.VOID_REP_TYPE_VAR);
            // Add the void's shape representation IRI to the mappings to
            // distinguish their model representation if there are multiple voids
            // as well as prevent us from creating a duplicate geometric void
            String voidShapeRepIRI = retrieveIri(soln, CommonQuery.VOID_SHAPE_REP_VAR);
            modelRepMappings.add(voidShapeRepIRI, voidRep);
            // Create the geometric void instance and add its statements
            GeometricVoid voidInst =  new GeometricVoid(elementIRI, voidRep.getBimIri(), voidType, voidPlacementIRI);
            voidInst.addGeometricVoidStatements(statementSet);
        }
    }
}
