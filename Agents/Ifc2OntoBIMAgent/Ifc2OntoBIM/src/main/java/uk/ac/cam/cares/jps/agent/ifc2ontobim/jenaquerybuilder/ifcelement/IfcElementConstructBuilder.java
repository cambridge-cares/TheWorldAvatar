package uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement;

import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.arq.querybuilder.Converters;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base.IfcConstructBuilderTemplate;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.NamespaceMapper;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Provides query statements specific to all elements such as doors, walls, floors, furnitures, and devices.
 *
 * @author qhouyee
 */
public class IfcElementConstructBuilder extends IfcConstructBuilderTemplate {
    /**
     * Create the SPARQL query syntax for Construct queries of all Ifc elements.
     *
     * @param builder  Construct Builder object to add Construct query statements.
     * @param ifcClass The element's class name in the IfcOwl ontology.
     * @param bimClass The object's class name in the OntoBIM ontology.
     * @return The SPARQL query string for IFC elements
     */
    public String createSparqlQuery(ConstructBuilder builder, String ifcClass, String bimClass) {
        // Replace ifcClass with their actual classes if identifiers are used
        ifcClass= (ifcClass.equals("ifc:IfcSlabF")||ifcClass.equals("ifc:IfcSlabR")) ? "ifc:IfcSlab" : ifcClass;
        this.createTemplateSparqlQuery(builder, ifcClass, bimClass);
        this.switchFunctionDependingOnInput(builder, ifcClass, bimClass);
        return builder.buildString();
    }

    /**
     * A utility method to call other functions that generate additional SPARQL query statements based on the inputs.
     */
    @Override
    protected void switchFunctionDependingOnInput(ConstructBuilder builder, String ifcClass, String bimClass) {
        switch (ifcClass) {
            case "ifc:IfcSlab":
                IfcOpeningQuery.addVoidRepresentationQueryComponents(builder);
                IfcSlabQuery.addSlabQueryComponents(builder, bimClass);
                break;
            case "ifc:IfcRoof":
                IfcOpeningQuery.addVoidRepresentationQueryComponents(builder);
                break;
            case "ifc:IfcDoor":
            case "ifc:IfcWindow":
                IfcOpeningQuery.addHostElementQueryComponents(builder);
                break;
            case "ifc:IfcWall":
            case "ifc:IfcWallStandardCase":
                IfcWallQuery.addSecondShapeRepresentationQueryComponents(builder);
                break;
            case "ifc:IfcStair":
                IfcStairQuery.addSubElementsQueryComponents(builder);
                break;
            case "ifc:IfcCovering":
                IfcCoveringQuery.addCoveringQueryComponents(builder,bimClass);
                break;
        }

        // Spatial location are usually the same for most elements except for the following classes
        if (!(bimClass.equals("bim:Roof")&&ifcClass.equals("ifc:IfcSlab"))) {
            this.addSpatialLocationQueryComponents(builder);
        }
        // Geometric representation are usually the same for most elements except for the following classes
        if (!bimClass.equals("bim:Stair")) {
            this.addGeometricRepresentationQueryComponents(builder);
        }
    }

    /**
     * Add the statements for relating each element to the zone they belong in.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addSpatialLocationQueryComponents(ConstructBuilder builder) {
        builder.addConstruct("?zone", "bot:containsElement", "?element");
        builder.addWhere("?spatialStructureRelationship", "rdf:type", "ifc:IfcRelContainedInSpatialStructure")
                .addWhere("?spatialStructureRelationship", "ifc:relatedElements_IfcRelContainedInSpatialStructure", "?element")
                .addWhere("?spatialStructureRelationship", "ifc:relatingStructure_IfcRelContainedInSpatialStructure", "?zone");
    }

    /**
     * Add the statements for querying the internal reference system into the builder. This is equivalent to
     * the IfcGeometricRepresentationContext class in Ifc schema.
     *
     * @param builder Construct Builder object to add Construct query statements.
     */
    private void addGeometricRepresentationQueryComponents(ConstructBuilder builder) {
        builder.addConstruct("?element", "bim:hasGeometricRepresentation", "?instshaperep")
                .addConstruct("?instshaperep", "rdf:type", "bim:ModelRepresentation3D")
                .addConstruct("?instshaperep", "bim:hasRepresentationType", "?shapereptype")
                .addConstruct("?instshaperep", "bim:hasSubContext", "?subcontext") // Sub-context
                .addConstruct("?subcontext", "rdf:type", "bim:GeometricRepresentationSubContext")
                .addConstruct("?instshaperep", "bim:hasRepresentationItem", "?geometry") // Geometry
                .addConstruct("?geometry", "rdf:type", "?geomtype")
                .addConstruct("?instshaperep", "bim:hasTargetPlacement", "?cartesiantransformer") // Optional transformer operator
                .addConstruct("?cartesiantransformer", "rdf:type", "bim:CartesianTransformationOperator")
                .addConstruct("?instshaperep", "bim:hasSourcePlacement", "?geomaxisplacement") // Optional source placement
                .addConstruct("?geomaxisplacement", "rdf:type", "bim:LocalPlacement");
        // Common query syntax related to geometric representation triples
        builder.addWhere("?element", "ifc:representation_IfcProduct", "?productDefinitionShape")
                .addWhere("?productDefinitionShape", "rdf:type", "ifc:IfcProductDefinitionShape");
        // Sub-query syntax
        this.addGeometricRepresentationUnionSubQuery(builder);
    }


    /**
     * Add a sub-group query comprising two query variations with a UNION pattern. Both queries are enclosed in a group {}
     * Geometric Representation can have one of two query structure in the IFC schema. But there doesn't seem to be
     * a pattern when one occurs or not. As one of the UNION query will return empty values, it would be better to
     * structure the query as such for this case.
     *
     * @param builder Construct Builder object to add the sub-query statements.
     */
    private void addGeometricRepresentationUnionSubQuery(ConstructBuilder builder) {
        // Set up the empty builders and add the necessary prefixes
        SelectBuilder subgroupBuilder = new SelectBuilder();
        NamespaceMapper.addSubqueryBuilderNamespaces(subgroupBuilder);
        SelectBuilder unionBuilder = subgroupBuilder.clone();
        // Query for the individual shape representation instances that are directly linked to specific geometries
        subgroupBuilder.addWhere("?productDefinitionShape", "ifc:representations_IfcProductRepresentation/list:hasContents", "?instshaperep")
                .addWhere("?instshaperep", "rdf:type", "ifc:IfcShapeRepresentation")
                .addWhere("?instshaperep", "ifc:representationType_IfcRepresentation/express:hasString", "?shapereptype")
                .addWhere("?instshaperep", "ifc:contextOfItems_IfcRepresentation", "?subcontext")
                .addWhere("?subcontext", "rdf:type", "ifc:IfcGeometricRepresentationSubContext")
                .addWhere("?instshaperep", "ifc:items_IfcRepresentation", "?geometry")
                .addWhere("?geometry", "rdf:type", "?geomtype");

        // ADD Optional for infinite list has next
        try {
            subgroupBuilder.addFilter("!regex(str(?geomtype) ,'IfcMappedItem')");
        } catch (ParseException e) {
            throw new JPSRuntimeException(e);
        }

        // Query for the family representation instance when there is no geometry available for individual instances
        // In these cases, individual instances are linked as a mapped item and are nested from the individual instances
        unionBuilder.addWhere("?productDefinitionShape", "ifc:representations_IfcProductRepresentation/list:hasContents", "?shaperep")
                .addWhere("?shaperep", "rdf:type", "ifc:IfcShapeRepresentation")
                .addWhere("?shaperep", "ifc:representationType_IfcRepresentation/express:hasString", Converters.makeLiteral("MappedRepresentation"))
                .addWhere("?shaperep", "ifc:items_IfcRepresentation", "?mappeditem") // individual instances with no geometries
                .addWhere("?mappeditem", "rdf:type", "ifc:IfcMappedItem")
                .addWhere("?mappeditem", "ifc:mappingSource_IfcMappedItem", "?representationmap")
                .addWhere("?mappeditem", "ifc:mappingTarget_IfcMappedItem", "?cartesiantransformer") // Transforms origin location to new location
                .addWhere("?cartesiantransformer", "rdf:type", "ifc:IfcCartesianTransformationOperator3D")
                .addWhere("?representationmap", "rdf:type", "ifc:IfcRepresentationMap")
                // Source placement structure is directly queried here as there is no IfcLocalPlacement instance in this specific case
                .addWhere("?representationmap", "ifc:mappingOrigin_IfcRepresentationMap","?geomaxisplacement")
                // Start of specific instance shape rep
                .addWhere("?representationmap", "ifc:mappedRepresentation_IfcRepresentationMap", "?instshaperep")
                .addWhere("?instshaperep", "rdf:type", "ifc:IfcShapeRepresentation")
                .addWhere("?instshaperep", "ifc:representationType_IfcRepresentation/express:hasString", "?shapereptype")
                .addWhere("?instshaperep", "ifc:contextOfItems_IfcRepresentation", "?subcontext") // Sub-context
                .addWhere("?subcontext", "rdf:type", "ifc:IfcGeometricRepresentationSubContext")
                .addWhere("?instshaperep", "ifc:items_IfcRepresentation", "?geometry") // Geometries
                .addWhere("?geometry", "rdf:type", "?geomtype");
        subgroupBuilder.addUnion(unionBuilder);
        builder.addWhere(subgroupBuilder);
    }
}
