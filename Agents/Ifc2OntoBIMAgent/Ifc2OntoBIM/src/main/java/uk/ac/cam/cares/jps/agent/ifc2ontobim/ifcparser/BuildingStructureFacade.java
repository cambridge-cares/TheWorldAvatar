package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.Ceiling;
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
    private final SpatialZoneStorage zoneMappings;

    /**
     * Standard Constructor retrieving the spatial zone singleton.
     */
    public BuildingStructureFacade(){
        this.zoneMappings = SpatialZoneStorage.Singleton();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcCovering.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addCeilingStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_CEILING);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        // Query statements to restrict it to specifically ceiling
        selectBuilder.addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_REL_TYPE_DEFINITION)
                .addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, CommonQuery.IFC_RELATED_OBJECT, CommonQuery.ELEMENT_VAR)
                .addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, CommonQuery.IFC_RELATING_TYPE, CommonQuery.ELEMENT_TYPE_VAR)
                .addWhere(CommonQuery.ELEMENT_TYPE_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_COVERING_TYPE)
                .addWhere(CommonQuery.ELEMENT_TYPE_VAR, CommonQuery.IFC_PREDEFINED_TYPE_COVERING, CommonQuery.IFC_CEILING_ENUM);
        // Query from the model
        ResultSet results = QueryHandler.execSelectQuery(selectBuilder.buildString(), owlModel);
        // Create new model representation mappings
        ElementModelRepresentationStorage modelRepMappings = new ElementModelRepresentationStorage();
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
            if (modelRepMappings.containsIri(iri)){
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
                Ceiling ceiling = new Ceiling(iri, name, uid, placement, hostZone, geomModel.getBimIri());
                ceiling.constructStatements(statementSet);
            }
        }
        // Construct all the Model Representation 3D statements
        modelRepMappings.constructModelRepStatements(statementSet);
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for IfcDoor.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addDoorStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCDOOR);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        // Query from the model
        ResultSet results = QueryHandler.execSelectQuery(selectBuilder.buildString(), owlModel);
        // Create new model representation mappings
        ElementModelRepresentationStorage modelRepMappings = new ElementModelRepresentationStorage();
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
            if (modelRepMappings.containsIri(iri)){
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
                Door door = new Door(iri, name, uid, placement, hostZone, geomModel.getBimIri());
                door.constructStatements(statementSet);
            }
        }
        // Construct all the Model Representation 3D statements
        modelRepMappings.constructModelRepStatements(statementSet);
    }
}
