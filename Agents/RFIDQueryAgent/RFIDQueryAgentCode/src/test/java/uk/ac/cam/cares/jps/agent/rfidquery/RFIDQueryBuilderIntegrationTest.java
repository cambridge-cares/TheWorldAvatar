package uk.ac.cam.cares.jps.agent.rfidquery;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import java.util.*;

import org.eclipse.rdf4j.sparqlbuilder.core.query.DeleteDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;

/**
 * This test class is to test the RFID Query Builder with a running KG.
 */


@Ignore("Requires both triple store endpoint set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")

@Testcontainers
public class RFIDQueryBuilderIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);

    @Rule
    //temp folder for temp client.properties file
    public TemporaryFolder folder = new TemporaryFolder();

    //RFIDQueryAgent
    private RFIDQueryBuilder builder;

    //endpoint
    String endpoint;

    // Set up a kb client that points to the location of the triple store
    RemoteStoreClient kbClient = new RemoteStoreClient();

    /**
     * Namespaces for ontologies
     */
	public static final String ONTODEVICE_NS = "https://www.theworldavatar.com/kg/ontodevice/";
	public static final String ONTOLAB_NS = "https://www.theworldavatar.com/kg/ontolab/";
    public static final String ONTOCAPE_CPS_BEHAVIOR_NS = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#";
    public static final String ONTOCAPE_PHASE_SYSTEM_NS = "http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#";
    public static final String ONTOCAPE_MATERIAL_NS = "http://www.theworldavatar.com/ontology/ontocape/material/material.owl#";
    public static final String ONTOCAPE_SYSTEM_NS = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#";
    public static final String ONTOCAPE_CPS_SUBSTANCE = "http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#";
    public static final String ONTOSPECIES_NS = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#";
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String SAREF_NS = "https://saref.etsi.org/core/";
    
	/**
     * Prefixes
     */ 
	private static final Prefix PREFIX_ONTODEVICE = SparqlBuilder.prefix("ontodevice", iri(ONTODEVICE_NS));
	private static final Prefix PREFIX_ONTOLAB = SparqlBuilder.prefix("ontolab", iri(ONTOLAB_NS));
	private static final Prefix PREFIX_ONTOCAPE_CPS_BEHAVIOR = SparqlBuilder.prefix("ontocape_cps_behavior", iri(ONTOCAPE_CPS_BEHAVIOR_NS));
    private static final Prefix PREFIX_ONTOCAPE_PHASE_SYSTEM = SparqlBuilder.prefix("ontocape_cps_phase_system", iri(ONTOCAPE_PHASE_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOCAPE_MATERIAL = SparqlBuilder.prefix("ontocape_material", iri(ONTOCAPE_MATERIAL_NS));
    private static final Prefix PREFIX_ONTOCAPE_SYSTEM = SparqlBuilder.prefix("ontocape_system", iri(ONTOCAPE_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOCAPE_CPS_SUBSTANCE = SparqlBuilder.prefix("ontocape_cps_substance", iri(ONTOCAPE_CPS_SUBSTANCE));
    private static final Prefix PREFIX_ONTOSPECIES = SparqlBuilder.prefix("ontospecies", iri(ONTOSPECIES_NS));
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));
    private static final Prefix PREFIX_SAREF = SparqlBuilder.prefix("saref", iri(SAREF_NS));
    
	/**
     * Object Properties
     */ 
	private static final Iri isAttachedTo = PREFIX_ONTODEVICE.iri("isAttachedTo");
	private static final Iri isFilledWith = PREFIX_ONTOLAB.iri("isFilledWith");
	private static final Iri refersToMaterial = PREFIX_ONTOCAPE_CPS_BEHAVIOR.iri("refersToMaterial");
	private static final Iri thermodynamicBehavior = PREFIX_ONTOCAPE_MATERIAL.iri("thermodynamicBehavior");
    private static final Iri isComposedOfSubsystem = PREFIX_ONTOCAPE_SYSTEM.iri("isComposedOfSubsystem");
    private static final Iri representsOccurenceOf = PREFIX_ONTOCAPE_PHASE_SYSTEM.iri("representsOccurenceOf");
    private static final Iri hasState = PREFIX_SAREF.iri("hasState");
    private static final Iri hasGHSHazardStatement = PREFIX_ONTOSPECIES.iri("hasGHSHazardStatements");
    private static final Iri intrinsicCharacteristics = PREFIX_ONTOCAPE_MATERIAL.iri("intrinsicCharacteristics");
    private static final Iri containsDirectly = PREFIX_ONTOCAPE_SYSTEM.iri("containsDirectly");
    private static final Iri hasMolecularFormula = PREFIX_ONTOSPECIES.iri("hasMolecularFormula");
    private static final Iri hasMolecularWeight = PREFIX_ONTOSPECIES.iri("hasMolecularWeight");
    private static final Iri unit = PREFIX_ONTOSPECIES.iri("unit");

    /**
     * Data Properties
     */
    private static final Iri label = PREFIX_RDFS.iri("label");
    private static final Iri comment = PREFIX_RDFS.iri("comment");
    private static final Iri value = PREFIX_ONTOSPECIES.iri("value");

    /**
     * Classes
     */
    private static final Iri multiPhaseSystem = PREFIX_ONTOCAPE_PHASE_SYSTEM.iri("MultiphaseSystem");
    private static final Iri singlePhase = PREFIX_ONTOCAPE_PHASE_SYSTEM.iri("SinglePhase");
    private static final Iri mixture = PREFIX_ONTOCAPE_CPS_SUBSTANCE.iri("Mixture");
    private static final Iri chemicalComponent = PREFIX_ONTOCAPE_CPS_SUBSTANCE.iri("ChemicalComponent");
    /**
     * Instances IRIs
     */
    private static final Iri state = iri("http://www.theworldavatar.com/kb/ontodevice/tag_01_status");
    private static final Iri tag = iri("http://www.theworldavatar.com/kb/ontodevice/tag_01");
    private static final Iri bottle = iri("http://www.theworldavatar.com/kb/ontolab/Bottle_01");
    private static final Iri chemicalAmount = iri("http://www.theworldavatar.com/kb/ontolab/ChemicalAmount_01");
    private static final Iri chemical = iri("http://www.theworldavatar.com/kb/ontolab/Chemical_01");
    private static final Iri phase = iri("http://www.theworldavatar.com/kb/ontolab/SolidPhase_01");
    private static final Iri phaseComponent = iri("http://www.theworldavatar.com/kb/ontolab/PhaseComponent_01");
    private static final Iri species = iri("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
    private static final Iri GHSHazardStatement1 = iri("http://www.theworldavatar.com/kg/ontospecies/GHSHazardStatements_61ecccfe-2f04-44ac-a209-d6dc23e2dbe2");
    private static final Iri GHSHazardStatement2 = iri("http://www.theworldavatar.com/kg/ontospecies/GHSHazardStatements_b02dc3ca-0fba-4c7e-bf30-636918b6ca49");
    private static final Iri substance = iri("http://www.theworldavatar.com/kb/ontolab/substance_01");
    private static final Iri molecularFormula = iri("http://www.theworldavatar.com/kb/ontolab/molecularFormula_01");
    private static final Iri molecularWeight = iri("http://www.theworldavatar.com/kb/ontolab/molecularWeight_01");
    private static final Iri molecularWeightUnit = iri("http://www.theworldavatar.com/kb/ontolab/molecularWeightUnit_01");

    @Before
    public void IntializeMockBlazeGraphAndBuilderAndAddMockTriples() throws IOException {
        // Start the containers
        try {
            // Start Blazegraph container
            blazegraph.start();

        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }
        
        // Set endpoint to the triple store. The host and port are read from the container
        endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        // Default namespace in blazegraph is "kb"
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql";


        kbClient.setUpdateEndpoint(endpoint);
        kbClient.setQueryEndpoint(endpoint);

        String propertiesFile = Paths.get(folder.getRoot().toString(), "all.properties").toString();
        //single mock property file to represent the properties files
        writePropertyFile(propertiesFile, Arrays.asList("sparql.query.endpoint="+endpoint, "sparql.update.endpoint="+endpoint));

        //create RFIDQueryBuilder
        builder = new RFIDQueryBuilder(propertiesFile, propertiesFile);

        //Initialise mock triples in triple store
        TriplePattern updatePattern = chemical.has(intrinsicCharacteristics, substance);
        TriplePattern updatePattern2 = tag.has(hasState, state);
        TriplePattern updatePattern3 = tag.has(isAttachedTo, bottle);
        TriplePattern updatePattern4 = bottle.has(isFilledWith, chemicalAmount);
        TriplePattern updatePattern5 = chemicalAmount.has(refersToMaterial, chemical);
        TriplePattern updatePattern6 = chemical.has(thermodynamicBehavior, phase);
        TriplePattern updatePattern7 = phase.has(isComposedOfSubsystem, phaseComponent);
        TriplePattern updatePattern8 = phaseComponent.has(representsOccurenceOf, species);
        TriplePattern updatePattern9 = species.has(label, "test");
        TriplePattern updatePattern10 = species.has(hasGHSHazardStatement, GHSHazardStatement1).andHas(hasMolecularFormula, molecularFormula);
        TriplePattern updatePattern11 = species.has(hasGHSHazardStatement, GHSHazardStatement2).andHas(hasMolecularWeight, molecularWeight);
        TriplePattern updatePattern12 = GHSHazardStatement1.has(label, "testing1");
        TriplePattern updatePattern13 = GHSHazardStatement1.has(comment, "some hazard warning 1");
        TriplePattern updatePattern14 = GHSHazardStatement2.has(label, "testing2");
        TriplePattern updatePattern15 = GHSHazardStatement2.has(comment, "some hazard warning 2");
        TriplePattern updatePattern16 = bottle.has(label, "chemical container 1");
        TriplePattern updatePattern17 = phase.isA(singlePhase);
        TriplePattern updatePattern18 = substance.isA(chemicalComponent);
        TriplePattern updatePattern19 = molecularFormula.has(value, "ClH2NaO5");
        TriplePattern updatePattern20 = molecularWeight.has(value, "140.45").andHas(unit, molecularWeightUnit);
        TriplePattern updatePattern21 = molecularWeightUnit.has(label, "g / mol");

        InsertDataQuery insert = Queries.INSERT_DATA(updatePattern, updatePattern2, updatePattern3, updatePattern4, updatePattern5, updatePattern6, updatePattern7, updatePattern8, updatePattern9, updatePattern10, updatePattern11, updatePattern12, updatePattern13, updatePattern14, updatePattern15, updatePattern16, updatePattern17, updatePattern18,updatePattern19, updatePattern20, updatePattern21);
        insert.prefix(PREFIX_ONTOCAPE_CPS_SUBSTANCE, PREFIX_ONTOCAPE_CPS_BEHAVIOR, PREFIX_ONTOCAPE_MATERIAL, PREFIX_ONTOCAPE_PHASE_SYSTEM, PREFIX_ONTOCAPE_SYSTEM, PREFIX_ONTODEVICE, PREFIX_RDFS, PREFIX_ONTOLAB, PREFIX_ONTOSPECIES, PREFIX_SAREF);
        kbClient.executeUpdate(insert.getQueryString());
    }
    // Cleaning up containers after each test, otherwise unused containers will first be killed when all tests finished
    @After
    public void stopContainers() {
        if (blazegraph.isRunning()) {
            blazegraph.stop();
        }
    }

    @Test
    public void queryTagSuccessAndFail() throws IOException {
        String IRI = builder.queryForTagWithStateIRI("http://www.theworldavatar.com/kb/ontodevice/tag_01_status");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontodevice/tag_01", IRI);

        //invalid State IRI
        try {
            IRI = builder.queryForTagWithStateIRI("http://www.theworldavatar.com/kb/ontodevice/tag_01_stat");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for tag IRI!", e.getMessage());
        }

        //remove hasState link between Tag and State
        TriplePattern Pattern = tag.has(hasState, state);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_SAREF);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForTagWithStateIRI("http://www.theworldavatar.com/kb/ontodevice/tag_01_status");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for tag IRI!", e.getMessage());
        }
    }

    @Test
    public void queryTaggedObjectSuccessAndFail() throws IOException {
        String IRI = builder.queryForTaggedObjectWithIsAttachedTo("http://www.theworldavatar.com/kb/ontodevice/tag_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontolab/Bottle_01", IRI);

        //invalid tag IRI
        try {
            IRI = builder.queryForTaggedObjectWithIsAttachedTo("http://www.theworldavatar.com/kb/ontodevice/tag_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for tagged object IRI!", e.getMessage());
        }

        //remove isAttachedTo between tag and bottle
        TriplePattern Pattern = tag.has(isAttachedTo, bottle);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTODEVICE);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForTaggedObjectWithIsAttachedTo("http://www.theworldavatar.com/kb/ontodevice/tag_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for tagged object IRI!", e.getMessage());
        }
    }

    @Test
    public void queryForChemicalComponentSuccessAndFail() throws IOException {
        String IRI = builder.queryForChemicalComponent("http://www.theworldavatar.com/kb/ontolab/Chemical_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontolab/substance_01", IRI);

        //remove intrinsicCharacteristics between chemical and substance
        TriplePattern pattern = chemical.has(intrinsicCharacteristics, substance);
        DeleteDataQuery delete = Queries.DELETE_DATA(pattern);
        delete.prefix(PREFIX_ONTOCAPE_MATERIAL);
        kbClient.executeUpdate(delete.getQueryString());
        IRI = builder.queryForChemicalComponent("http://www.theworldavatar.com/kb/ontolab/Chemical_01");
        Assert.assertEquals(null, IRI);

        //remove rdf:type OntoCAPE_CPS_Substance: ChemicalComponent from substance
        pattern = substance.isA(chemicalComponent);
        delete = Queries.DELETE_DATA(pattern);
        delete.prefix(PREFIX_ONTOCAPE_CPS_SUBSTANCE);
        kbClient.executeUpdate(delete.getQueryString());

        //add intrinsicCharacteristics between chemical and substance
        //add rdf:type OntoCAPE_CPS_Substance: Mixture to substance
        //add <substance> ontoCAPE_System:containsDirectly <component>
        pattern = chemical.has(intrinsicCharacteristics, substance);
        TriplePattern pattern2 = substance.isA(mixture);
        TriplePattern pattern3 = substance.has(containsDirectly,species);
        InsertDataQuery insert = Queries.INSERT_DATA(pattern, pattern2, pattern3);
        insert.prefix(PREFIX_ONTOCAPE_MATERIAL ,PREFIX_ONTOCAPE_CPS_SUBSTANCE, PREFIX_ONTOCAPE_SYSTEM);
        kbClient.executeUpdate(insert.getQueryString());

        IRI = builder.queryForChemicalComponent("http://www.theworldavatar.com/kb/ontolab/Chemical_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060", IRI);

    }

    @Test
    public void queryChemicalAmountSuccessAndFail() throws IOException {
        String IRI = builder.queryForChemicalAmountWithIsFilledWith("http://www.theworldavatar.com/kb/ontolab/Bottle_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontolab/ChemicalAmount_01", IRI);

        //invalid bottle IRI
        IRI = builder.queryForChemicalAmountWithIsFilledWith("http://www.theworldavatar.com/kb/ontolab/Bottle_02");
        Assert.assertEquals("This tagged object does not contain any chemicals", IRI);

        //remove isFilledWith link between Bottle and ChemicalAmount
        TriplePattern Pattern = bottle.has(isFilledWith, chemicalAmount);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOLAB);
        kbClient.executeUpdate(delete.getQueryString());

        IRI = builder.queryForChemicalAmountWithIsFilledWith("http://www.theworldavatar.com/kb/ontolab/Bottle_01");
        Assert.assertEquals("This tagged object does not contain any chemicals", IRI);
    }
    
    @Test
    public void queryMaterialSuccessAndFail() throws IOException {
        String IRI = builder.queryForChemicalWithRefersToMaterial("http://www.theworldavatar.com/kb/ontolab/ChemicalAmount_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontolab/Chemical_01", IRI);

        //invalid chemical amount IRI
        try {
            IRI = builder.queryForChemicalWithRefersToMaterial("http://www.theworldavatar.com/kb/ontolab/ChemicalAmount_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for chemical IRI!", e.getMessage());
        }

        //remove refersToMaterial link between ChemicalAmount and Chemical
        TriplePattern Pattern = chemicalAmount.has(refersToMaterial, chemical);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOCAPE_CPS_BEHAVIOR);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForChemicalWithRefersToMaterial("http://www.theworldavatar.com/kb/ontolab/ChemicalAmount_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for chemical IRI!", e.getMessage());
        }
    }

    @Test
    public void queryPhaseSuccessAndFail() throws IOException {
        String IRI = builder.queryForPhaseWithThermodynamicBehavior("http://www.theworldavatar.com/kb/ontolab/Chemical_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontolab/SolidPhase_01", IRI);

        //invalid chemical IRI
        try {
            IRI = builder.queryForPhaseWithThermodynamicBehavior("http://www.theworldavatar.com/kb/ontolab/Chemical_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for phase IRI!", e.getMessage());
        }

        //remove thermodynamicBehavior link between Material and Phase
        TriplePattern Pattern = chemical.has(thermodynamicBehavior, phase);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOCAPE_MATERIAL);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForPhaseWithThermodynamicBehavior("http://www.theworldavatar.com/kb/ontolab/Chemical_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for phase IRI!", e.getMessage());
        }
    }

    @Test
    public void queryPhaseComponentSuccessAndFail() throws IOException {
        String IRI = builder.queryForPhaseComponentWithIsComposedOfSubsystem("http://www.theworldavatar.com/kb/ontolab/SolidPhase_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontolab/PhaseComponent_01", IRI);

        //invalid phase IRI
        try {
            IRI = builder.queryForPhaseComponentWithIsComposedOfSubsystem("http://www.theworldavatar.com/kb/ontolab/SolidPhase_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for phase component IRI!", e.getMessage());
        }

        //remove isComposedOfSubsystem link between Phase and Phase Component
        TriplePattern Pattern = phase.has(isComposedOfSubsystem, phaseComponent);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOCAPE_SYSTEM);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForPhaseComponentWithIsComposedOfSubsystem("http://www.theworldavatar.com/kb/ontolab/SolidPhase_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for phase component IRI!", e.getMessage());
        }
    }

    @Test
    public void querySpeciesSuccessAndFail() throws IOException {
        String IRI = builder.queryForSpeciesWithRepresentsOccurenceOf("http://www.theworldavatar.com/kb/ontolab/PhaseComponent_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060", IRI);

        //invalid phase component IRI
        try {
            IRI = builder.queryForSpeciesWithRepresentsOccurenceOf("http://www.theworldavatar.com/kb/ontolab/PhaseComponent_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for species IRI!", e.getMessage());
        }

        //remove representsOccurenceOf link between Phase Component and Species
        TriplePattern Pattern = phaseComponent.has(representsOccurenceOf, species);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOCAPE_PHASE_SYSTEM);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForSpeciesWithRepresentsOccurenceOf("http://www.theworldavatar.com/kb/ontolab/PhaseComponent_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for species IRI!", e.getMessage());
        }
    }

    @Test
    public void querySpeciesLabelSuccessAndFail() throws IOException {
        String a = null;
        a = builder.queryForSpeciesLabel("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        Assert.assertEquals("test", a);

        //invalid species IRI
        try {
            a = builder.queryForSpeciesLabel("http://www.theworldavatar.com/kb/ontospecies/Species");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for label via rdfs:label!", e.getMessage());
        }

        //remove label link between Species and String values
        TriplePattern Pattern = species.has(label, "test");
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_RDFS);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            a = builder.queryForSpeciesLabel("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for label via rdfs:label!", e.getMessage());
        }
    }

    @Test
    public void queryTaggedObjectLabelSuccessAndFail() throws IOException {
        String a = null;
        a = builder.queryForTaggedObjectLabel("http://www.theworldavatar.com/kb/ontolab/Bottle_01");
        Assert.assertEquals("chemical container 1", a);

        //invalid tagged object IRI
        try {
            a = builder.queryForTaggedObjectLabel("http://www.theworldavatar.com/kb/ontolab/Bottle_");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for label via rdfs:label!", e.getMessage());
        }

        //remove label link between tagged object and String values
        TriplePattern Pattern = species.has(label, "chemical container 1");
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_RDFS);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            a = builder.queryForTaggedObjectLabel("http://www.theworldavatar.com/kb/ontolab/Bottle_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for label via rdfs:label!", e.getMessage());
        }
    }

    @Test
    public void queryForGHSHazardStatementsSuccessAndFail() throws IOException {
        JSONArray a = builder.queryForGHSHazardStatements("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        Assert.assertEquals(2, a.length());

        //invalid species IRI
        a = builder.queryForGHSHazardStatements("http://www.theworldavatar.com/kb/ontospecies/Species");
        Assert.assertEquals(null, a);

        //remove hasGHSHazardStatements link between Species and GHSHazardStatements
        TriplePattern Pattern = species.has(hasGHSHazardStatement, GHSHazardStatement1);
        TriplePattern Pattern2 = species.has(hasGHSHazardStatement, GHSHazardStatement2);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern, Pattern2);
        delete.prefix(PREFIX_ONTOSPECIES);
        kbClient.executeUpdate(delete.getQueryString());
        a = builder.queryForGHSHazardStatements("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060"); 
        Assert.assertEquals(null, a);
    }

    @Test
    public void queryForLabelAndCommentForGHSHazardStatementsSuccessAndFail() throws IOException {
        JSONArray a = builder.queryForGHSHazardStatements("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        Map<String, List<String>> map = builder.queryForLabelAndCommentForGHSHazardStatements(a);
        Assert.assertEquals(2, map.get("label").size());
        Assert.assertEquals(2, map.get("comment").size());

        if (map.get("label").get(0) == "testing1") {
            Assert.assertEquals("some hazard warning 1",map.get("comment").get(0));
        } else if (map.get("label").get(0) == "testing2") {
            Assert.assertEquals("some hazard warning 2",map.get("comment").get(0));
        }

        //invalid species IRI
        a = builder.queryForGHSHazardStatements("http://www.theworldavatar.com/kb/ontospecies/Species");
        map = builder.queryForLabelAndCommentForGHSHazardStatements(a);
        Assert.assertEquals(null, map);

        //remove rdfs:label and rdfs:comment link between GHSHazardStatements and labels and comments
        TriplePattern updatePattern1 = GHSHazardStatement1.has(label, "testing1");
        TriplePattern updatePattern2 = GHSHazardStatement1.has(comment, "some hazard warning 1");
        TriplePattern updatePattern3 = GHSHazardStatement2.has(label, "testing2");
        TriplePattern updatePattern4 = GHSHazardStatement2.has(comment, "some hazard warning 2");
        DeleteDataQuery delete = Queries.DELETE_DATA(updatePattern1, updatePattern2, updatePattern3, updatePattern4);
        delete.prefix(PREFIX_RDFS);
        kbClient.executeUpdate(delete.getQueryString());
        try {
            a = builder.queryForGHSHazardStatements("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
            map = builder.queryForLabelAndCommentForGHSHazardStatements(a);
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for GHS Hazard Statements labels and comments!", e.getMessage());
        }
    }

    @Test
    public void queryForMolecularFormulaSuccessAndFail() throws IOException {
        String a = null;
        a = builder.queryForMolecularFormula("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        Assert.assertEquals("ClH2NaO5", a);


        a = builder.queryForMolecularFormula("testing");
        Assert.assertEquals("Molecular Formula information not available", a);

        //remove label link between tagged object and String values
        TriplePattern Pattern = molecularFormula.has(value, "ClH2NaO5");
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOSPECIES);
        kbClient.executeUpdate(delete.getQueryString());
        a = builder.queryForMolecularFormula(species.toString());
        Assert.assertEquals("Molecular Formula information not available", a);
    }

    @Test
    public void queryForMolecularWeightValueSuccessAndFail() throws IOException {
        String a = null;
        a = builder.queryForMolecularWeightValue("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        Assert.assertEquals("140.45", a);

        //invalid IRI
        a = builder.queryForMolecularWeightValue("testing");
        Assert.assertEquals("Molecular Weight information not available", a);

        //remove label link between tagged object and String values
        TriplePattern Pattern = molecularWeight.has(value, "140.45");
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOSPECIES);
        kbClient.executeUpdate(delete.getQueryString());

        a = builder.queryForMolecularWeightValue(species.toString());
        Assert.assertEquals("Molecular Weight information not available", a);
    }

    @Test
    public void queryForMolecularWeightUnitSuccessAndFail() throws IOException {
        String a = null;
        a = builder.queryForMolecularWeightUnit("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        Assert.assertEquals("g / mol", a);

        //invalid IRI
        a = builder.queryForMolecularWeightUnit("testing");
        Assert.assertEquals("Molecular Weight unit information not available", a);

        //remove label link between tagged object and String values
        TriplePattern Pattern = molecularWeight.has(value, "140.45");
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOSPECIES);
        kbClient.executeUpdate(delete.getQueryString());

        a = builder.queryForMolecularWeightUnit(species.toString());
        Assert.assertEquals("Molecular Weight unit information not available", a);
    }


    private void writePropertyFile(String filepath, List<String> properties) throws IOException {
        // Overwrite potentially existing properties file
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }
    
}
    	
    
   
    
    
    

