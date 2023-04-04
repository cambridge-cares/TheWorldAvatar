package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.facade;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.*;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ModelRepresentation3D;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.CommonQuery;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.ElementStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser.storage.SpatialZoneStorage;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.utils.QueryHandler;

import java.util.LinkedHashSet;

/**
 * Provides functions to generate the triples for building structure elements.
 * If you wish to know the basic query template, please look at columns.
 *
 * @author qhouyee
 */
public class BuildingStructureFacade {
    private final SpatialZoneStorage zoneMappings;
    private final ElementStorage modelRepMappings;


    /**
     * Standard Constructor retrieving the spatial zone singleton.
     */
    public BuildingStructureFacade() {
        this.zoneMappings = SpatialZoneStorage.Singleton();
        this.modelRepMappings = ElementStorage.Singleton();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for ceilings.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addCeilingStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_CEILING);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
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
            if (this.modelRepMappings.containsModelRepIri(iri)) {
                // Retrieve the new geometry IRI and append it to the existing Model Representation 3D of this element
                String geomIri = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_VAR);
                geomModel = this.modelRepMappings.getModelRep(iri);
                geomModel.appendGeometry(geomIri);
            } else {
                // If it is not yet created, first generate a new Model Representation 3D object
                geomModel = QueryHandler.retrieveModelRepresentation3D(soln);
                // Add the object into the mappings for its IRI
                this.modelRepMappings.add(iri, geomModel);
                // Construct the element's instance and its statements
                Ceiling ceiling = new Ceiling( name, uid, placement, hostZone, geomModel.getBimIri());
                ceiling.constructStatements(statementSet);
            }
        }
        // Construct all the Model Representation 3D statements
        this.modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        this.modelRepMappings.clear();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for columns.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addColumnStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_COLUMN);
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
            if (this.modelRepMappings.containsModelRepIri(iri)) {
                // Retrieve the new geometry IRI and append it to the existing Model Representation 3D of this element
                String geomIri = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_VAR);
                geomModel = this.modelRepMappings.getModelRep(iri);
                geomModel.appendGeometry(geomIri);
            } else {
                // If it is not yet created, first generate a new Model Representation 3D object
                geomModel = QueryHandler.retrieveModelRepresentation3D(soln);
                // Add the object into the mappings for its IRI
                this.modelRepMappings.add(iri, geomModel);
                // Construct the element's instance and its statements
                Column column = new Column(name, uid, placement, hostZone, geomModel.getBimIri());
                column.constructStatements(statementSet);
            }
        }
        // Construct all the Model Representation 3D statements
        this.modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        this.modelRepMappings.clear();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for doors.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addDoorStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_DOOR);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        // Add class-specific properties
        // Add query for assembly element
        selectBuilder.addVar(CommonQuery.ASSEMBLY_VAR)
                .addWhere(CommonQuery.REL_FILLS_ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_REL_FILLS_ELEMENT)
                .addWhere(CommonQuery.REL_FILLS_ELEMENT_VAR, CommonQuery.IFC_REL_FILLS_SUB_ELEMENT, CommonQuery.ELEMENT_VAR)
                .addWhere(CommonQuery.REL_FILLS_ELEMENT_VAR, CommonQuery.IFC_REL_FILLS_OPENING, CommonQuery.OPENING_ELEMENT_VAR)
                .addWhere(CommonQuery.OPENING_ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_OPENING_ELEMENT)
                .addWhere(CommonQuery.REL_VOID_ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_REL_VOIDS_ELEMENT)
                .addWhere(CommonQuery.REL_VOID_ELEMENT_VAR, CommonQuery.IFC_REL_VOIDS_OPENING, CommonQuery.OPENING_ELEMENT_VAR)
                .addWhere(CommonQuery.REL_VOID_ELEMENT_VAR, CommonQuery.IFC_REL_VOIDS_ASSEMBLY, CommonQuery.ASSEMBLY_VAR);
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
            String assemblyIri = QueryHandler.retrieveIri(soln, CommonQuery.ASSEMBLY_VAR);
            ModelRepresentation3D geomModel;
            // If the element object has already been created previously
            if (this.modelRepMappings.containsModelRepIri(iri)) {
                // Retrieve the new geometry IRI and append it to the existing Model Representation 3D of this element
                String geomIri = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_VAR);
                geomModel = this.modelRepMappings.getModelRep(iri);
                geomModel.appendGeometry(geomIri);
            } else {
                // If it is not yet created, first generate a new Model Representation 3D object
                geomModel = QueryHandler.retrieveModelRepresentation3D(soln);
                // Add the object into the mappings for its IRI
                this.modelRepMappings.add(iri, geomModel);
                // Retrieve the IRI of the wall hosting this window
                String assemblyBimIri = this.modelRepMappings.getElementIri(assemblyIri);
                // Construct the element's instance and its statements
                Door door = new Door(name, uid, placement, hostZone, assemblyBimIri, geomModel.getBimIri());
                door.constructStatements(statementSet);
            }
        }
        // Construct all the Model Representation 3D statements
        this.modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        this.modelRepMappings.clear();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for floors.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addFloorStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_SLAB);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        CommonQuery.addVoidRepresentationQueryComponents(selectBuilder);
        // Add class-specific properties
        // Restrict to certain IfcSlab types for floor
        selectBuilder.addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_REL_TYPE_DEFINITION)
                .addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, CommonQuery.IFC_RELATED_OBJECT, CommonQuery.ELEMENT_VAR)
                .addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, CommonQuery.IFC_RELATING_TYPE, CommonQuery.ELEMENT_TYPE_VAR)
                .addWhere(CommonQuery.ELEMENT_TYPE_VAR, CommonQuery.IFC_PREDEFINED_TYPE_SLAB, CommonQuery.SLAB_ENUM_VAR)
                .addWhereValueVar(CommonQuery.SLAB_ENUM_VAR, CommonQuery.IFC_SLAB_FLOOR_ENUM, CommonQuery.IFC_SLAB_BASE_SLAB_ENUM);
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
            // If the element object has already been created in a previous row
            if (this.modelRepMappings.containsModelRepIri(iri)) {
                // This is a duplicate solution which may either have more geometries as part of the element's Model Representation 3D
                // OR it has multiple geometric voids and should generate new ones
                // Note that geometric voids usually have one geometry and does not require further appending to the void's Model Representation 3D
                String geomIri = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_VAR);
                String voidShapeRepIRI = QueryHandler.retrieveIri(soln, CommonQuery.VOID_SHAPE_REP_VAR);
                // Only append the geometry IRI to the element's existing Model Representation 3D if there are differences
                if (geomIri != null) {
                    geomModel = this.modelRepMappings.getModelRep(iri);
                    geomModel.appendGeometry(geomIri);
                }
                // If this is a distinct void shape rep, generate another void geometry statement
                if (!this.modelRepMappings.containsModelRepIri(voidShapeRepIRI)) {
                    QueryHandler.addVoidGeometryStatements(soln, this.modelRepMappings.getElementIri(iri), statementSet, this.modelRepMappings);
                }
            } else {
                // If it is not yet created, first generate a new Model Representation 3D object
                geomModel = QueryHandler.retrieveModelRepresentation3D(soln);
                // Construct the element's instance and its statements
                Floor floor = new Floor(name, uid, placement, hostZone, geomModel.getBimIri());
                // Generate the void geometry statements and add void model representation into the mappings
                QueryHandler.addVoidGeometryStatements(soln, floor.getIfcRepIri(), statementSet, this.modelRepMappings);
                floor.constructStatements(statementSet);
                // Add the object into the mappings for its IRI
                this.modelRepMappings.add(iri, geomModel);
                this.modelRepMappings.add(iri, floor.getIfcRepIri());
            }
        }
        // Construct all the Model Representation 3D statements
        this.modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        this.modelRepMappings.clear();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for roofs.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addRoofStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        // Restrict to IfcSlab type of ROOF
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_SLAB)
                .addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_REL_TYPE_DEFINITION)
                .addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, CommonQuery.IFC_RELATED_OBJECT, CommonQuery.ELEMENT_VAR)
                .addWhere(CommonQuery.REL_TYPE_DEFINITION_VAR, CommonQuery.IFC_RELATING_TYPE, CommonQuery.ELEMENT_TYPE_VAR)
                .addWhere(CommonQuery.ELEMENT_TYPE_VAR, CommonQuery.IFC_PREDEFINED_TYPE_SLAB, CommonQuery.IFC_SLAB_ROOF_ENUM);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        CommonQuery.addVoidRepresentationQueryComponents(selectBuilder);
        // Add class-specific properties for querying their spatial location
        // Note that IfcRoof is a container class linking the host zone to the individual slab geometry representation
        selectBuilder.addVar(CommonQuery.PARENT_ZONE_VAR);
        selectBuilder.addWhere(CommonQuery.RELAGGR_VAR, QueryHandler.RDF_TYPE, CommonQuery.RELAGG)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_PARENT_ZONE_REL, CommonQuery.ROOF_SLAB_VAR)
                .addWhere(CommonQuery.ROOF_SLAB_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_ROOF)
                .addWhere(CommonQuery.RELAGGR_VAR, CommonQuery.IFC_CHILD_ZONE_REL, CommonQuery.ELEMENT_VAR)
                .addWhere(CommonQuery.REL_SPATIAL_STRUCTURE_VAR, QueryHandler.RDF_TYPE, CommonQuery.REL_SPATIAL_ZONE_ELEMENT)
                .addWhere(CommonQuery.REL_SPATIAL_STRUCTURE_VAR, CommonQuery.IFC_REL_ELEMENT, CommonQuery.ROOF_SLAB_VAR)
                .addWhere(CommonQuery.REL_SPATIAL_STRUCTURE_VAR, CommonQuery.IFC_REL_ZONE, CommonQuery.PARENT_ZONE_VAR);
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
            // If the element object has already been created in a previous row
            if (this.modelRepMappings.containsModelRepIri(iri)) {
                // This is a duplicate solution which may either have more geometries as part of the element's Model Representation 3D
                // OR it has multiple geometric voids and should generate new ones
                // Note that geometric voids usually have one geometry and does not require further appending to the void's Model Representation 3D
                String geomIri = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_VAR);
                String voidShapeRepIRI = QueryHandler.retrieveIri(soln, CommonQuery.VOID_SHAPE_REP_VAR);
                // Only append the geometry IRI to the element's existing Model Representation 3D if there are differences
                if (geomIri != null) {
                    geomModel = this.modelRepMappings.getModelRep(iri);
                    geomModel.appendGeometry(geomIri);
                }
                // If this is a distinct void shape rep, generate another void geometry statement
                if (!this.modelRepMappings.containsModelRepIri(voidShapeRepIRI)) {
                    QueryHandler.addVoidGeometryStatements(soln, this.modelRepMappings.getElementIri(iri), statementSet, this.modelRepMappings);
                }
            } else {
                // If it is not yet created, first generate a new Model Representation 3D object
                geomModel = QueryHandler.retrieveModelRepresentation3D(soln);
                // Construct the element's instance and its statements
                Roof roof = new Roof(name, uid, placement, hostZone, geomModel.getBimIri());
                // Generate the void geometry statements and add void model representation into the mappings
                QueryHandler.addVoidGeometryStatements(soln, roof.getIfcRepIri(), statementSet, this.modelRepMappings);
                roof.constructStatements(statementSet);
                // Add the object into the mappings for its IRI
                this.modelRepMappings.add(iri, geomModel);
                this.modelRepMappings.add(iri, roof.getIfcRepIri());
            }
        }
        // Construct all the Model Representation 3D statements
        this.modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        this.modelRepMappings.clear();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for stairs.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addStairStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_STAIR);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        // Split the queries and processing for each of the four assembly component
        // This split helps to simplify query returned
        SelectBuilder landingQuery = selectBuilder.clone();
        CommonQuery.addStairLandingQueryComponents(landingQuery);
        // Query from the model for landing
        ResultSet results = QueryHandler.execSelectQuery(landingQuery.buildString(), owlModel);
        // Process query results
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String subComponentIri = QueryHandler.retrieveIri(soln, CommonQuery.LANDING_VAR);
            String subComponentName = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_LANDING_NAME_VAR);
            String subComponentUid = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_LANDING_UID_VAR);
            String subComponentPlacement = QueryHandler.retrieveIri(soln, CommonQuery.STAIR_LANDING_PLACEMENT_VAR);
            ModelRepresentation3D geomModel = retrieveStairModelRepStatements(soln, subComponentIri, CommonQuery.STAIR_LANDING_SHAPE_REP_VAR,
                    CommonQuery.STAIR_LANDING_SUB_CONTEXT_VAR, CommonQuery.STAIR_LANDING_GEOM_VAR, CommonQuery.STAIR_LANDING_REP_TYPE_VAR);
            // If landing variable doesn't exist, create a new one
            if (!this.modelRepMappings.containsStairSubComponentIri(subComponentIri)) {
                StairLanding landing = new StairLanding(subComponentName, subComponentUid,
                        subComponentPlacement, retrieveStairIRI(soln, statementSet), geomModel.getBimIri());
                landing.constructStatements(statementSet);
                // Add the subcomponent to the mappings to keep track 
                this.modelRepMappings.add(subComponentIri);
            }
        }
        // Generate query for railing
        SelectBuilder railingQuery = selectBuilder.clone();
        CommonQuery.addStairRailingQueryComponents(railingQuery);
        // Query from the model
        results = QueryHandler.execSelectQuery(railingQuery.buildString(), owlModel);
        // Process query results
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String subComponentIri = QueryHandler.retrieveIri(soln, CommonQuery.RAILING_VAR);
            String subComponentName = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_RAILING_NAME_VAR);
            String subComponentUid = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_RAILING_UID_VAR);
            String subComponentPlacement = QueryHandler.retrieveIri(soln, CommonQuery.STAIR_RAILING_PLACEMENT_VAR);
            ModelRepresentation3D geomModel = retrieveStairModelRepStatements(soln, subComponentIri, CommonQuery.STAIR_RAILING_SHAPE_REP_VAR,
                    CommonQuery.STAIR_RAILING_SUB_CONTEXT_VAR, CommonQuery.STAIR_RAILING_GEOM_VAR, CommonQuery.STAIR_RAILING_REP_TYPE_VAR);
            // If railing variable doesn't exist, create a new one
            if (!this.modelRepMappings.containsStairSubComponentIri(subComponentIri)) {
                StairRailing railing = new StairRailing(subComponentName, subComponentUid,
                        subComponentPlacement, retrieveStairIRI(soln, statementSet), geomModel.getBimIri());
                railing.constructStatements(statementSet);
                // Add the subcomponent to the mappings to keep track 
                this.modelRepMappings.add(subComponentIri);
            }
        }
        // Generate query for structural components
        SelectBuilder structuralComponentQuery = selectBuilder.clone();
        CommonQuery.addStairStructuralComponentQueryComponents(structuralComponentQuery);
        // Query from the model
        results = QueryHandler.execSelectQuery(structuralComponentQuery.buildString(), owlModel);
        // Process query results
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String subComponentIri = QueryHandler.retrieveIri(soln, CommonQuery.STAIR_STRUCTURAL_COMPONENT_VAR);
            String subComponentName = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_STRUCT_COMP_NAME_VAR);
            String subComponentUid = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_STRUCT_COMP_UID_VAR);
            String subComponentPlacement = QueryHandler.retrieveIri(soln, CommonQuery.STAIR_STRUCT_COMP_PLACEMENT_VAR);
            ModelRepresentation3D geomModel = retrieveStairModelRepStatements(soln, subComponentIri, CommonQuery.STAIR_STRUCT_COMP_SHAPE_REP_VAR,
                    CommonQuery.STAIR_STRUCT_COMP_SUB_CONTEXT_VAR, CommonQuery.STAIR_STRUCT_COMP_GEOM_VAR, CommonQuery.STAIR_STRUCT_COMP_REP_TYPE_VAR);
            // If structural component variable doesn't exist, create a new one
            if (!this.modelRepMappings.containsStairSubComponentIri(subComponentIri)) {
                StairStructuralComponent structuralComponent = new StairStructuralComponent(subComponentName, subComponentUid,
                        subComponentPlacement, retrieveStairIRI(soln, statementSet), geomModel.getBimIri());
                structuralComponent.constructStatements(statementSet);
                // Add the subcomponent to the mappings to keep track 
                this.modelRepMappings.add(subComponentIri);
            }
        }
        // Generate query for stair flights
        SelectBuilder flightQuery = selectBuilder.clone();
        CommonQuery.addStairFlightQueryComponents(flightQuery);
        // Query from the model
        results = QueryHandler.execSelectQuery(flightQuery.buildString(), owlModel);
        // Process query results
        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            String subComponentIri = QueryHandler.retrieveIri(soln, CommonQuery.STAIRFLIGHT_VAR);
            String subComponentName = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_FLIGHT_NAME_VAR);
            String subComponentUid = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_FLIGHT_UID_VAR);
            String subComponentPlacement = QueryHandler.retrieveIri(soln, CommonQuery.STAIR_FLIGHT_PLACEMENT_VAR);
            String stairRiserNo = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_RISER_NUM_VAR);
            String stairTreadNo = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_TREAD_NUM_VAR);
            String stairRiserHeight = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_RISER_HEIGHT_VAR);
            String stairTreadLength = QueryHandler.retrieveLiteral(soln, CommonQuery.STAIR_TREAD_LENGTH_VAR);
            ModelRepresentation3D geomModel = retrieveStairModelRepStatements(soln, subComponentIri, CommonQuery.STAIR_FLIGHT_SHAPE_REP_VAR,
                    CommonQuery.STAIR_FLIGHT_SUB_CONTEXT_VAR, CommonQuery.STAIR_FLIGHT_GEOM_VAR, CommonQuery.STAIR_FLIGHT_REP_TYPE_VAR);
            // If stair flight variable doesn't exist, create a new one
            if (!this.modelRepMappings.containsStairSubComponentIri(subComponentIri)) {
                StairFlight stairFlight = new StairFlight(subComponentName, subComponentUid,
                        subComponentPlacement, retrieveStairIRI(soln, statementSet), geomModel.getBimIri(), stairRiserNo, stairTreadNo, stairRiserHeight, stairTreadLength);
                stairFlight.constructStatements(statementSet);
                // Add the subcomponent to the mappings to keep track 
                this.modelRepMappings.add(subComponentIri);
            }
        }
        // Construct all the Model Representation 3D statements
        this.modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        this.modelRepMappings.clear();
    }

    /**
     * Retrieves or generate the stair assembly object from each row of the query results.
     *
     * @param soln         The row of Jena result set to retrieve information from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    private String retrieveStairIRI(QuerySolution soln, LinkedHashSet<Statement> statementSet) {
        String iri = soln.get(CommonQuery.ELEMENT_VAR).toString();
        String name = QueryHandler.retrieveLiteral(soln, CommonQuery.NAME_VAR);
        String uid = QueryHandler.retrieveLiteral(soln, CommonQuery.UID_VAR);
        String placement = QueryHandler.retrieveIri(soln, CommonQuery.PLACEMENT_VAR);
        String hostZone = QueryHandler.retrieveHostZone(soln, zoneMappings);
        // If the stair object has already been created previously
        String bimIRI;
        if (this.modelRepMappings.containsIri(iri)) {
            bimIRI = this.modelRepMappings.getElementIri(iri);
        } else {
            // If it is not yet created, generate a new stair object and construct its statements
            Stair stair = new Stair(name, uid, placement, hostZone);
            stair.constructStatements(statementSet);
            bimIRI = stair.getBIMIri();
            // Add the object to the mappings
            this.modelRepMappings.add(iri, bimIRI);
        }
        return bimIRI;
    }

    /**
     * Retrieves or generate the model representation of the stair's subcomponents from each row of the query results.
     *
     * @param soln            The row of Jena result set to retrieve information from.
     * @param subComponentIri The IRI of the subcomponent.
     * @param shapeRepVar     The variable of the subcomponent's shape representation.
     * @param subContextVar   The variable of the subcomponent's sub-context.
     * @param geomVar         The variable of the subcomponent's geometry.
     * @param repTypeVar      The variable of the subcomponent's shape representation type.
     */
    private ModelRepresentation3D retrieveStairModelRepStatements(QuerySolution soln, String subComponentIri, String shapeRepVar,
                                                                  String subContextVar, String geomVar, String repTypeVar) {
        ModelRepresentation3D geomModel;
        if (this.modelRepMappings.containsModelRepIri(subComponentIri)) {
            // Retrieve the new geometry IRI and append it to the existing Model Representation 3D of this element
            String geomIri = QueryHandler.retrieveIri(soln, geomVar);
            geomModel = this.modelRepMappings.getModelRep(subComponentIri);
            geomModel.appendGeometry(geomIri);
        } else {
            // If it is not yet created, first generate a new Model Representation 3D object
            geomModel = QueryHandler.retrieveModelRepresentation3D(soln, subContextVar, geomVar, repTypeVar);
            // Add the object into the mappings for its IRI
            this.modelRepMappings.add(subComponentIri, geomModel);
        }
        return geomModel;
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for walls.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addWallStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        SelectBuilder optionalBuilder = selectBuilder.clone();
        // Create a union group for both possible wall classes
        SelectBuilder wallClassUnionBuilder = selectBuilder.clone();
        SelectBuilder wallStandardClassUnionBuilder = selectBuilder.clone();
        wallClassUnionBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_WALL);
        wallStandardClassUnionBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_WALL_STANDARD_CASE);
        wallClassUnionBuilder.addUnion(wallStandardClassUnionBuilder);
        selectBuilder.addWhere(wallClassUnionBuilder);
        // Add common properties
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        // Add class-specific properties
        // Walls may sometimes consist of polyline as the main geometry with a different second geometric representation type
        selectBuilder.addVar(CommonQuery.INST_SHAPE_REP_SEC_VAR);
        selectBuilder.addVar(CommonQuery.REP_SEC_SUBCONTEXT_VAR);
        selectBuilder.addVar(CommonQuery.GEOM_SEC_VAR);
        selectBuilder.addVar(CommonQuery.INST_SHAPE_REP_TYPE_SEC_VAR);
        optionalBuilder.addWhere(CommonQuery.PRODUCT_DEFINITION_VAR, CommonQuery.IFC_PRODUCT_REPRESENTATIONS + "/" + CommonQuery.LIST_HAS_NEXT +
                        "/" + CommonQuery.LIST_HAS_CONTENT, CommonQuery.INST_SHAPE_REP_SEC_VAR)
                .addWhere(CommonQuery.INST_SHAPE_REP_SEC_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_SHAPE_REP)
                .addWhere(CommonQuery.INST_SHAPE_REP_SEC_VAR, CommonQuery.IFC_PRODUCT_REPRESENTATION_TYPE + CommonQuery.EXPRESS_HASSTRING, CommonQuery.INST_SHAPE_REP_TYPE_SEC_VAR)
                .addWhere(CommonQuery.INST_SHAPE_REP_SEC_VAR, CommonQuery.IFC_REP_CONTEXT, CommonQuery.REP_SEC_SUBCONTEXT_VAR)
                .addWhere(CommonQuery.REP_SEC_SUBCONTEXT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFCGEOM_REP_SUBCONTEXT)
                .addWhere(CommonQuery.INST_SHAPE_REP_SEC_VAR, CommonQuery.IFC_REP_ITEMS, CommonQuery.GEOM_SEC_VAR)
                .addWhere(CommonQuery.GEOM_SEC_VAR, QueryHandler.RDF_TYPE, CommonQuery.GEOM_TYPE_SEC_VAR);
        selectBuilder.addOptional(optionalBuilder);
        // Note that I have excluded the IfcRelConnectsPathElements that indicates if the wall are connected to another wall,
        // and at which end. This class seems only useful for visualising the IFC schema in IFC viewers. But for conversion,
        // it does not matter as the dimensions and positions given should suffice to generate the required geometry.
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
            if (this.modelRepMappings.containsModelRepIri(iri)) {
                // Retrieve the new geometry IRI and append it to the existing Model Representation 3D of this element
                String geomIri = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_VAR);
                geomModel = this.modelRepMappings.getModelRep(iri);
                geomModel.appendGeometry(geomIri);
            } else {
                // If it is not yet created, first generate a new Model Representation 3D object
                geomModel = QueryHandler.retrieveModelRepresentation3D(soln);
                // Add the object into the mappings for its IRI
                this.modelRepMappings.add(iri, geomModel);
                Wall wall;
                // If there is a second geometry representation for the wall
                if (soln.contains(CommonQuery.INST_SHAPE_REP_SEC_VAR)) {
                    // Generate a second geom model
                    ModelRepresentation3D secGeomModel = QueryHandler.retrieveModelRepresentation3D(soln, CommonQuery.REP_SEC_SUBCONTEXT_VAR,
                            CommonQuery.GEOM_SEC_VAR, CommonQuery.INST_SHAPE_REP_TYPE_SEC_VAR);
                    // Add the IRI with a suffix to add geom model into the mappings so that its statements can be constructed
                    // Note that when there is two different geometric representation, they only have one geometric representation each
                    this.modelRepMappings.add(iri + "second", secGeomModel);
                    // Construct the element's instance
                    wall = new Wall(name, uid, placement, hostZone, geomModel.getBimIri(), secGeomModel.getBimIri());
                } else {
                    // Construct the element's instance
                    wall = new Wall(name, uid, placement, hostZone, geomModel.getBimIri(), null);
                }
                // Always construct the statement regardless of which constructor initialised
                wall.constructStatements(statementSet);
                // Add the wall element's IRI to the mappings
                this.modelRepMappings.add(iri, wall.getElementIri());
            }
        }
        // Construct all the Model Representation 3D statements
        this.modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        this.modelRepMappings.clear();
    }

    /**
     * Executes the SPARQL SELECT query and convert the results into OntoBIM statements for windows.
     *
     * @param owlModel     The IfcOwl model containing the triples to query from.
     * @param statementSet A list containing the new OntoBIM triples.
     */
    public void addWindowStatements(Model owlModel, LinkedHashSet<Statement> statementSet) {
        // Set up query builder and its query statements
        SelectBuilder selectBuilder = QueryHandler.initSelectQueryBuilder();
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_WINDOW);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR, CommonQuery.UID_VAR, CommonQuery.NAME_VAR, CommonQuery.PLACEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        // Add class-specific properties
        // Add query for assembly element
        selectBuilder.addVar(CommonQuery.ASSEMBLY_VAR)
                .addWhere(CommonQuery.REL_FILLS_ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_REL_FILLS_ELEMENT)
                .addWhere(CommonQuery.REL_FILLS_ELEMENT_VAR, CommonQuery.IFC_REL_FILLS_SUB_ELEMENT, CommonQuery.ELEMENT_VAR)
                .addWhere(CommonQuery.REL_FILLS_ELEMENT_VAR, CommonQuery.IFC_REL_FILLS_OPENING, CommonQuery.OPENING_ELEMENT_VAR)
                .addWhere(CommonQuery.OPENING_ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_OPENING_ELEMENT)
                .addWhere(CommonQuery.REL_VOID_ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_REL_VOIDS_ELEMENT)
                .addWhere(CommonQuery.REL_VOID_ELEMENT_VAR, CommonQuery.IFC_REL_VOIDS_OPENING, CommonQuery.OPENING_ELEMENT_VAR)
                .addWhere(CommonQuery.REL_VOID_ELEMENT_VAR, CommonQuery.IFC_REL_VOIDS_ASSEMBLY, CommonQuery.ASSEMBLY_VAR);
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
            String assemblyIri = QueryHandler.retrieveIri(soln, CommonQuery.ASSEMBLY_VAR);
            ModelRepresentation3D geomModel;
            // If the element object has already been created previously
            if (this.modelRepMappings.containsModelRepIri(iri)) {
                // Retrieve the new geometry IRI and append it to the existing Model Representation 3D of this element
                String geomIri = QueryHandler.retrieveIri(soln, CommonQuery.GEOM_VAR);
                geomModel = this.modelRepMappings.getModelRep(iri);
                geomModel.appendGeometry(geomIri);
            } else {
                // If it is not yet created, first generate a new Model Representation 3D object
                geomModel = QueryHandler.retrieveModelRepresentation3D(soln);
                // Add the object into the mappings for its IRI
                this.modelRepMappings.add(iri, geomModel);
                // Construct the element's instance and its statements
                Window window = new Window(name, uid, placement, hostZone, this.modelRepMappings.getElementIri(assemblyIri), geomModel.getBimIri());
                window.constructStatements(statementSet);
            }
        }
        // Construct all the Model Representation 3D statements
        this.modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        this.modelRepMappings.clear();
    }
}
