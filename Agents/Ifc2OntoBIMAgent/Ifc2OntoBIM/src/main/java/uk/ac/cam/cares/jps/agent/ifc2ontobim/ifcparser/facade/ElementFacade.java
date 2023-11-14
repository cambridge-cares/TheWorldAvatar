package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.Element;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ModelRepresentation3D;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.CommonQuery;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ElementStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.SpatialZoneStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.QueryHandler;

import java.util.LinkedHashSet;

/**
 * Provides functions to generate the triples for non-building structure elements.
 *
 * @author qhouyee
 */
public class ElementFacade {
    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for the all other elements.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public static void addElementStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        SpatialZoneStorage zoneMappings = SpatialZoneStorage.Singleton();
        ElementStorage modelRepMappings = ElementStorage.Singleton();
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        // Create a union group for the possible element classes remaining
        SelectBuilder buildingElementProxyClassUnionBuilder = selectBuilder.clone();
        buildingElementProxyClassUnionBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_BUILDING_ELEMENT_PROXY);
        SelectBuilder flowSegmentClassUnionBuilder = selectBuilder.clone();
        flowSegmentClassUnionBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_FLOW_SEGMENT);
        SelectBuilder flowTerminalClassUnionBuilder = selectBuilder.clone();
        flowTerminalClassUnionBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_FLOW_TERMINAL);
        SelectBuilder furnishingElementClassUnionBuilder = selectBuilder.clone();
        furnishingElementClassUnionBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_FURNISHING_ELEMENT);
        buildingElementProxyClassUnionBuilder.addUnion(flowSegmentClassUnionBuilder);
        buildingElementProxyClassUnionBuilder.addUnion(flowTerminalClassUnionBuilder);
        buildingElementProxyClassUnionBuilder.addUnion(furnishingElementClassUnionBuilder);
        selectBuilder.addWhere(buildingElementProxyClassUnionBuilder);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        // Query from the model
        ResultSet results = QueryHandler.execSelectQuery(selectBuilder.buildString(), owlModel);
        // Process query results
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String iri = soln.get(CommonQuery.ELEMENT_VAR).toString();
            String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
            String uid = QueryHandler.retrieveLiteral(soln, CommonQuery.UID_VAR);
            String placement = QueryHandler.retrieveIri(soln, CommonQuery.PLACEMENT_VAR);
            String hostZone = QueryHandler.retrieveHostZone(soln, zoneMappings);
            ModelRepresentation3D geomModel;
            // If the element object has already been created previously
            if (modelRepMappings.containsModelRepIri(iri)) {
                // Retrieve the new geometry IRI and append it to the existing Model Representation 3D of this element
                String geomIri = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_VAR);
                geomModel = modelRepMappings.getModelRep(iri);
                geomModel.appendGeometry(geomIri);
            } else {
                // If it is not yet created, first generate a new Model Representation 3D object
                geomModel = QueryHandler.retrieveModelRepresentation3D(soln);
                // Add the object into the mappings for its IRI
                modelRepMappings.add(iri, geomModel);
                // Construct the element's instance and its statements
                Element element = new Element(name, uid, placement, hostZone, geomModel.getBimIri());
                element.constructStatements(statementSet);
            }
        }
        // Construct all the Model Representation 3D statements
        modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        modelRepMappings.clear();
    }
}
