package uk.ac.cam.cares.jps.agent.ifc2ontobim;

import org.apache.jena.rdf.model.*;
import org.apache.jena.arq.querybuilder.ConstructBuilder;
import org.apache.jena.riot.RDFParser;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaclassifier.*;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcelement.IfcElementConstructBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base.IfcProjectConstructBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.ifcgeometry.GeomConstructBuilderMediator;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenaquerybuilder.base.IfcSpatialZonesConstructBuilder;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.jenautils.*;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.IgnoreClassHelper;
import uk.ac.cam.cares.jps.agent.ifc2ontobim.ttlparser.TTLWriter;

import java.nio.file.Path;
import java.util.*;


/**
 * A converter class that reads the converted IfcOwl triples, and convert them into the OntoBIM instances as a TTL file.
 *
 * @author qhouyee
 */
public class OntoBimConverter {
    private Model owlModel;
    private TTLWriter writer;
    private Set<String> ignoreGeom;
    private final Map<String, String> classMapping = new HashMap<>();
    private final List<Path> tempFilePaths = new ArrayList<>();
    private static final Logger LOGGER = LogManager.getLogger(Ifc2OntoBIMAgent.class);

    /**
     * Standard Constructor
     */
    public OntoBimConverter() {
    }

    /**
     * Read the ttl file output for IfcOwl, modify according to OntoBIM Tbox, and overwrite the ttl file
     */
    public void convertOntoBIM(String ttlFile) {
        // Load the existing IfcOwl triples into a model
        Path ttlFilePath = Path.of(ttlFile);
        this.owlModel = RDFParser.create()
                .source(ttlFilePath)
                .toModel();

        // Retrieve and add namespaces to builder
        ConstructBuilder builder = new ConstructBuilder();
        Map<String, String> nsMapping = NamespaceMapper.retrieveNamespace(this.owlModel);
        builder.addPrefixes(nsMapping);
        this.writer = new TTLWriter(nsMapping);

        // Create a new Set to ensure statements are kept in one object and not duplicated
        LinkedHashSet<Statement> statementSet = new LinkedHashSet<>();
        ignoreGeom = IgnoreClassHelper.genIgnoreSet();
        genProjectStatements(builder, statementSet);
        genSpatialZoneStatements(builder, statementSet);
        genElementsStatements(builder, statementSet);
        genCommonGeometryContentStatements(builder, statementSet);

        writer.writeTTLWithIntermediateFiles(this.tempFilePaths, ttlFilePath, classMapping);
    }

    /**
     * Query the generated TTL file for the project container and their relevant information. The results returned are
     * constructed as statements in the OntoBIM schema and stored as a set.
     *
     * @param builder      The construct builder to add new query statements.
     * @param statementSet Stores the relevant queried statements into this set.
     */
    private void genProjectStatements(ConstructBuilder builder, LinkedHashSet<Statement> statementSet) {
        // Clone the builder to ensure that query statements are not transferred across different zones
        ConstructBuilder tempBuilder = builder.clone();
        LOGGER.info("Preparing query for IfcProject container.");
        String query = new IfcProjectConstructBuilder().createSparqlQuery(tempBuilder);
        QueryHandler.queryConstructStatementsAsSet(query, this.owlModel, statementSet);
        LOGGER.info("Retrieved statements related to Ifc Project.");
    }

    /**
     * Query the generated TTL file for spatial zones and their relevant information.
     * The results returned are constructed as statements in the OntoBIM schema and stored as a set.
     * Note that the statements will be written to a temporary file to prevent heap overflow.
     *
     * @param builder      The construct builder to add new query statements.
     * @param statementSet Stores the relevant queried statements into this set.
     */
    private void genSpatialZoneStatements(ConstructBuilder builder, LinkedHashSet<Statement> statementSet) {
        Map<String, String> spatialZones = new HashMap<>();
        spatialZones.put("ifc:IfcSite", "bot:Site");
        spatialZones.put("ifc:IfcBuilding", "bot:Building");
        spatialZones.put("ifc:IfcBuildingStorey", "bot:Storey");
        spatialZones.put("ifc:IfcSpace", "bot:Space");

        for (String ifcZones : spatialZones.keySet()) {
            // Clone the builder to ensure that query statements are not transferred across different zones
            ConstructBuilder tempBuilder = builder.clone();
            LOGGER.info("Preparing query for the spatial zone: " + ifcZones);
            String query = new IfcSpatialZonesConstructBuilder().createSparqlQuery(tempBuilder, ifcZones, spatialZones.get(ifcZones));
            QueryHandler.queryConstructStatementsAsSet(query, this.owlModel, statementSet);
            LOGGER.info("Retrieved statements related to spatial zones for " + ifcZones);
        }
        CompoundAngleMeasureClassifier.addClassMapping(statementSet, classMapping);
        this.storeInTempFiles(statementSet);
        LOGGER.info("Stored statements for spatial zones in temporary file");
    }

    /**
     * Query the generated TTL file for Ifc elements and their relevant information.
     * The results returned are constructed as statements in the OntoBIM schema and stored as a set.
     * Note that the statements will be written to a temporary file to prevent heap overflow.
     *
     * @param builder      The construct builder to add new query statements.
     * @param statementSet Stores the relevant queried statements into this set.
     */
    private void genElementsStatements(ConstructBuilder builder, LinkedHashSet<Statement> statementSet) {
        Map<String, String> ifcElements = new HashMap<>();
        ifcElements.put("ifc:IfcDoor", "bim:Door");
        ifcElements.put("ifc:IfcWindow", "bim:Window");
        ifcElements.put("ifc:IfcColumn", "bim:Column");
        // Actual class is IfcSlab but added identifier to put into Map as separate keys
        ifcElements.put("ifc:IfcSlabF", "bim:Floor");
        ifcElements.put("ifc:IfcSlabR", "bim:Roof");
        ifcElements.put("ifc:IfcRoof", "bim:Roof");
        ifcElements.put("ifc:IfcWall", "bim:Wall");
        ifcElements.put("ifc:IfcWallStandardCase", "bim:Wall");
        ifcElements.put("ifc:IfcCovering", "bim:Ceiling"); // haven't tested
        ifcElements.put("ifc:IfcStair", "bim:Stair");
        ifcElements.put("ifc:IfcBuildingElementProxy", "bim:IfcBuildingElementProxy");
        ifcElements.put("ifc:IfcFlowTerminal", "bim:IfcFlowTerminal");
        ifcElements.put("ifc:IfcFurnishingElement", "bim:IfcFurnishingElement");

        for (String ifcElement : ifcElements.keySet()) {
            // Clone the builder to ensure that query statements are not transferred across different elements
            ConstructBuilder tempBuilder = builder.clone();
            LOGGER.info("Preparing query for the element: " + ifcElement);
            String query = new IfcElementConstructBuilder().createSparqlQuery(tempBuilder, ifcElement, ifcElements.get(ifcElement));
            QueryHandler.queryConstructStatementsAsSet(query, this.owlModel, statementSet);
            if (!statementSet.isEmpty()) {
                switch (ifcElement) {
                    case "ifc:IfcFurnishingElement":
                    case "ifc:IfcBuildingElementProxy":
                    case "ifc:IfcFlowTerminal":
                        GenericElementClassifier.addClassMapping(ifcElements.get(ifcElement), statementSet, classMapping);
                        break;
                    case "ifc:IfcSlabF":
                    case "ifc:IfcSlabR":
                        SlabClassifier.addClassMapping(ifcElements.get(ifcElement), statementSet, classMapping);
                        break;
                    case "ifc:IfcStair":
                        StairClassifier.addClassMapping(statementSet, classMapping);
                        break;
                    case "ifc:IfcCovering":
                        CoveringClassifier.addClassMapping(ifcElements.get(ifcElement), statementSet, classMapping);
                        break;
                }
                LOGGER.info("Retrieved statements related to elements of " + ifcElement);
                this.storeInTempFiles(statementSet);
                LOGGER.info("Stored statements for " + ifcElement + " in temporary file");
            }
        }
    }

    /**
     * This method performs two functions. It first retrieves the common geometry elements' IRIs in the statement list
     * containing spatial zones and element information. Second, it queries the IRI and their related content from
     * the IfcOwl model and add the subgraph constructed as a query result into the statement set.
     * Note that the statements will be written to a temporary file to prevent heap overflow.
     * <p>
     * Directly querying the common geometry contents from the IfcOwl model is problematic as it is less efficient.
     * In the Ifc schema, each instance of the same IfcClass and Family Type is linked to the same geometry resource.
     * Repeating the query will add duplicate results. Moreover, as IFC is verbose, the extension to the
     * SPARQL query syntax for including these common geometries will slow down the SPARQL query process.
     *
     * @param builder      The construct builder to add new query statements.
     * @param statementSet Stores the relevant queried statements into this set.
     */
    private void genCommonGeometryContentStatements(ConstructBuilder builder, LinkedHashSet<Statement> statementSet) {
        // Requires a Map that retain sequential information
        // The IfcBooleanClippingResult must run first as it also contains the other geometry contents
        Map<String, String> geomElements = new LinkedHashMap<>();
        geomElements.put("clipres", "ifc:IfcBooleanClippingResult");
        geomElements.put("boundedhalfspace", "ifc:IfcPolygonalBoundedHalfSpace");
        geomElements.put("brep", "ifc:IfcFacetedBrep");
        geomElements.put("areasolid", "ifc:IfcExtrudedAreaSolid");
        geomElements.put("polyline", "ifc:IfcPolyline");
        geomElements.put("subcontext", "bim:GeometricRepresentationSubContext");
        geomElements.put("localplacement", "bim:LocalPlacement");
        geomElements.put("cartesiantransformer", "bim:CartesianTransformationOperator");
        geomElements.put("directionvector", "bim:DirectionVector");
        geomElements.put("cartesianpt", "bim:CartesianPoint");

        for (String geomElement : geomElements.keySet()) {
            LOGGER.info("Preparing query for the common geometry element: " + geomElement);
            // Retrieves the subject IRI of the statements that belongs to a specific geometry element
            String geomClass = geomElements.get(geomElement);
            GeomSubjectNodeRetriever retriever = new GeomSubjectNodeRetriever(geomElement, geomClass);
            List<RDFNode> iriList = retriever.retrieveIriAsList(statementSet);

            ConstructBuilder tempBuilder = builder.clone(); // Clone the builder to ensure that query statements are not transferred across different elements
            String query = new GeomConstructBuilderMediator().createSparqlQuery(tempBuilder, iriList);
            // Store the results in a separate set before adding to the main set
            // Important to add back as new geometries and operators may be added
            LinkedHashSet<Statement> result = new LinkedHashSet<>();
            QueryHandler.queryConstructStatementsAsSet(query, this.owlModel, result);
            statementSet.addAll(result);
            LOGGER.info("Retrieved statements related to common geometry elements of " + geomElement);
            IgnoreClassHelper.removeIgnoredClass(geomClass, ignoreGeom);
            this.storeInTempFiles(result);
            LOGGER.info("Stored statements for " + geomElement + " in temporary file");
        }
    }

    /**
     * Store the current statements constructed into temporary files, stores their file path, and removes statements from the Set.
     * This help to prevent heap overflow, especially for larger, more complex IFC files.
     *
     * @param statementSet An ordered set holding the required statements.
     */
    private void storeInTempFiles(LinkedHashSet<Statement> statementSet) {
        Path tempFilePath;
        tempFilePath = writer.writeIntermediateTempFile(statementSet, ignoreGeom);
        this.tempFilePaths.add(tempFilePath);
    }
}
