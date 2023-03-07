package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Door;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ModelRepresentation3D;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

import java.util.LinkedHashSet;

/**
 * Provides functions to generate the triples for building structure elements.
 *
 * @author qhouyee
 */
public class BuildingStructureFacade {
    private static SpatialZoneStorage zoneMappings;

    /**
     * Generate zone triples
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public static void genZoneTriples(Model owlModel, LinkedHashSet<Statement> statementSet) {
        zoneMappings = SpatialZoneStorage.Singleton();
        execDoorQuery(owlModel, statementSet);
    }

    /**
     * Creates the SPARQL SELECT query statements for IfcDoor.
     *
     * @return A string containing the SPARQL query to execute.
     */
    private static String createDoorSelectQuery() {
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addVar(CommonQuery.ZONE_VAR)
                .addVar(CommonQuery.PARENT_ZONE_VAR);
        selectBuilder.addWhere(CommonQuery.ZONE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCDOOR)
                .addWhere(CommonQuery.RELAGGR_VAR, QueryHandler.RDF_TYPE, CommonQuery.REL_SPATIAL_ZONE_ELEMENT)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_REL_ZONE, CommonQuery.PARENT_ZONE_VAR)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_REL_ELEMENT, CommonQuery.ZONE_VAR);
        CommonQuery.addBaseQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        return selectBuilder.buildString();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcDoor.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    private static void execDoorQuery(Model owlModel, LinkedHashSet<Statement> statementSet) {
        String query = createDoorSelectQuery();
        ResultSet results = QueryHandler.execSelectQuery(query, owlModel);
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.ZONE_VAR).toString();
            String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
            String uid = QueryHandler.retrieveLiteral(soln, CommonQuery.UID_VAR);
            String placement = QueryHandler.retrieveIri(soln, CommonQuery.PLACEMENT_VAR);
            String hostZone = retrieveHostZone(soln);
            String geomType = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_TYPE_VAR);
            ModelRepresentation3D geomModel = retrieveModelRepresentation3D(soln);
            Door door = new Door(iri, name, uid, placement, hostZone, geomModel.getBimIri());
            door.constructStatements(statementSet);
            geomModel.addModelRepresentation3DStatements(statementSet, geomType);
        }
    }

    /**
     * Retrieve the IRI of the zone hosting this element, usually in the IfcBuildingStorey or IfcSpace.
     *
     * @param soln The row of the query response to retrieve this zone IRI.
     */
    private static String retrieveHostZone(QuerySolution soln) {
        String zoneIri = QueryHandler.retrieveIri(soln, CommonQuery.PARENT_ZONE_VAR);
        if (zoneIri.contains(CommonQuery.IFCSTOREY_CLASS)) {
            return zoneMappings.getStorey(zoneIri).getBotStoreyIRI();
        }
        return zoneMappings.getRoom(zoneIri).getBimRoomIRI();
    }

    /**
     * Retrieve the geometry model representation of the element as a ModelRepresentation3D class.
     *
     * @param soln The row of the query response to retrieve this zone IRI.
     */
    private static ModelRepresentation3D retrieveModelRepresentation3D(QuerySolution soln) {
        String shapeRepIri = soln.get(CommonQuery.INST_SHAPE_REP_VAR).toString();
        String subContextIri = soln.get(CommonQuery.REP_SUBCONTEXT_VAR).toString();
        String geomIri = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_VAR);
        String shapeRepType = QueryHandler.retrieveLiteral(soln, CommonQuery.INST_SHAPE_REP_TYPE_VAR);
        String sourcePlacementIri = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_AXIS_PLACEMENT_VAR);
        String cartesianTransformerIri = QueryHandler.retrieveIri(soln, CommonQuery.CART_TRANSFORMER_VAR);
        return new ModelRepresentation3D(shapeRepIri, subContextIri, geomIri,
                shapeRepType, sourcePlacementIri, cartesianTransformerIri);
    }
}
