package uk.ac.cam.cares.jps.agent.ifc2ontobim.ifcparser;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.Statement;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.element.buildingstructure.*;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ifc2x3.geom.ModelRepresentation3D;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement.IfcElementConstructBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.QueryHandler;

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
                Ceiling ceiling = new Ceiling(iri, name, uid, placement, hostZone, geomModel.getBimIri());
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
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR);
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
                Column column = new Column(iri, name, uid, placement, hostZone, geomModel.getBimIri());
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
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        // Add class-specific properties
        // Add query for assembly element
        selectBuilder.addVar(CommonQuery.ASSEMBLY_VAR)
                .addWhere(CommonQuery.REL_FILLS_ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_REL_FILLS_ELEMENT)
                .addWhere(CommonQuery.REL_FILLS_ELEMENT_VAR, CommonQuery.IFC_REL_FILLS_SUB_ELEMENT, IfcElementConstructBuilder.ELEMENT_VAR)
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
                // Retrieve the wall object hosting this window
                Wall assembly = this.modelRepMappings.getWall(assemblyIri);
                // Construct the element's instance and its statements
                Door door = new Door(iri, name, uid, placement, hostZone, assembly.getElementIri(), geomModel.getBimIri());
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
        selectBuilder.addWhere(CommonQuery.ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_FLOOR);
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR);
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
                Floor floor = new Floor(iri, name, uid, placement, hostZone, geomModel.getBimIri());
                floor.constructStatements(statementSet);
            }
        }
        // Construct all the Model Representation 3D statements
        this.modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        this.modelRepMappings.clear();
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
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR);
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
                    ModelRepresentation3D secGeomModel = QueryHandler.retrieveSecModelRepresentation3D(soln);
                    // Add the IRI with a suffix to add geom model into the mappings so that its statements can be constructed
                    // Note that when there is two different geometric representation, they only have one geometric representation each
                    this.modelRepMappings.add(iri + "second", secGeomModel);
                    // Construct the element's instance
                    wall = new Wall(iri, name, uid, placement, hostZone, geomModel.getBimIri(), secGeomModel.getBimIri());
                } else {
                    // Construct the element's instance
                    wall = new Wall(iri, name, uid, placement, hostZone, geomModel.getBimIri(), null);
                }
                // Always construct the statement regardless of which constructor initialised
                wall.constructStatements(statementSet);
                this.modelRepMappings.add(iri, wall);
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
        CommonQuery.addBaseQueryComponents(selectBuilder, CommonQuery.ELEMENT_VAR);
        CommonQuery.addElementHostZoneQueryComponents(selectBuilder);
        CommonQuery.addElementModelRepresentationQueryComponents(selectBuilder);
        // Add class-specific properties
        // Add query for assembly element
        selectBuilder.addVar(CommonQuery.ASSEMBLY_VAR)
                .addWhere(CommonQuery.REL_FILLS_ELEMENT_VAR, QueryHandler.RDF_TYPE, CommonQuery.IFC_REL_FILLS_ELEMENT)
                .addWhere(CommonQuery.REL_FILLS_ELEMENT_VAR, CommonQuery.IFC_REL_FILLS_SUB_ELEMENT, IfcElementConstructBuilder.ELEMENT_VAR)
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
                // Retrieve the wall object hosting this window
                Wall assembly = this.modelRepMappings.getWall(assemblyIri);
                // Construct the element's instance and its statements
                Window window = new Window(iri, name, uid, placement, hostZone, assembly.getElementIri(), geomModel.getBimIri());
                window.constructStatements(statementSet);
            }
        }
        // Construct all the Model Representation 3D statements
        this.modelRepMappings.constructModelRepStatements(statementSet);
        // Clear all existing mappings
        this.modelRepMappings.clear();
    }
}
