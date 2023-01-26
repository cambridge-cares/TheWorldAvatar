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
import java.util.Arrays;
import java.util.List;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.core.query.DeleteDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;

/**
 * This test class is to test the RFID Query agent with a running KG and postgres database.
 */


@Ignore("Requires both triple store endpoint and postgreSQL database set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")
        

@Testcontainers
public class RFIDQueryBuilderIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
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
	public static final String ONTOLAB_NS = "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontolab/OntoLab.owl#";
    public static final String ONTOCAPE_CPS_BEHAVIOR_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#";
    public static final String ONTOCAPE_PHASE_SYSTEM_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#";
    public static final String ONTOCAPE_MATERIAL_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/material.owl#";
    public static final String ONTOCAPE_SYSTEM_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#";
    public static final String ONTOSPECIES_NS = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#";
    public static final String ONTOKIN_NS = "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#";
    public static final String SKOS_NS = "http://www.w3.org/2004/02/skos/core#";
    
	/**
     * Prefixes
     */ 
	private static final Prefix PREFIX_ONTODEVICE = SparqlBuilder.prefix("ontodevice", iri(ONTODEVICE_NS));
	private static final Prefix PREFIX_ONTOLAB = SparqlBuilder.prefix("ontolab", iri(ONTOLAB_NS));
	private static final Prefix PREFIX_ONTOCAPE_CPS_BEHAVIOR = SparqlBuilder.prefix("ontocape_cps_behavior", iri(ONTOCAPE_CPS_BEHAVIOR_NS));
    private static final Prefix PREFIX_ONTOCAPE_PHASE_SYSTEM = SparqlBuilder.prefix("ontocape_cps_phase_system", iri(ONTOCAPE_PHASE_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOCAPE_MATERIAL = SparqlBuilder.prefix("ontocape_material", iri(ONTOCAPE_MATERIAL_NS));
    private static final Prefix PREFIX_ONTOCAPE_SYSTEM = SparqlBuilder.prefix("ontocape_system", iri(ONTOCAPE_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOSPECIES = SparqlBuilder.prefix("ontospecies", iri(ONTOSPECIES_NS));
    private static final Prefix PREFIX_ONTOKIN = SparqlBuilder.prefix("ontokin", iri(ONTOKIN_NS));
    private static final Prefix PREFIX_SKOS = SparqlBuilder.prefix("skos", iri(SKOS_NS));
    
	/**
     * Relationships
     */ 
	private static final Iri hasQualitativeValue = PREFIX_ONTODEVICE.iri("hasQualitativeValue");
	private static final Iri isPropertyOf = PREFIX_ONTODEVICE.iri("isPropertyOf");
	private static final Iri isAttachedTo = PREFIX_ONTODEVICE.iri("isAttachedTo");
	private static final Iri isFilledWith = PREFIX_ONTOLAB.iri("isFilledWith");
	private static final Iri refersToMaterial = PREFIX_ONTOCAPE_CPS_BEHAVIOR.iri("refersToMaterial");
	private static final Iri thermodynamicBehavior = PREFIX_ONTOCAPE_MATERIAL.iri("thermodynamicBehavior");
    private static final Iri isComposedOfSubsystem = PREFIX_ONTOCAPE_SYSTEM.iri("isComposedOfSubsystem");
    private static final Iri representsOccurenceOf = PREFIX_ONTOCAPE_PHASE_SYSTEM.iri("representsOccurenceOf");
    private static final Iri hasMolecularFormula = PREFIX_ONTOSPECIES.iri("hasMolecularFormula");
    private static final Iri hasElementNumber = PREFIX_ONTOKIN.iri("hasElementNumber");
    private static final Iri hasNumberOfElement = PREFIX_ONTOKIN.iri("hasNumberOfElement");
    private static final Iri altLabel = PREFIX_SKOS.iri("altLabel");

    /**
     * Classes
     */ 
	private static final Iri MolecularFormula = PREFIX_ONTOSPECIES.iri("MolecularFormula");

    /**
     * Instances IRIs
     */
    private static final Iri quality = iri("http://www.theworldavatar.com/kb/ontodevice/tag_01_status");
    private static final Iri qualitativeValue = iri("https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0");
    private static final Iri tag = iri("http://www.theworldavatar.com/kb/ontodevice/tag_01");
    private static final Iri bottle = iri("http://www.theworldavatar.com/kb/ontolab/Bottle_01");
    private static final Iri chemical = iri("http://www.theworldavatar.com/kb/ontolab/Chemical_01");
    private static final Iri material = iri("http://www.theworldavatar.com/kb/ontolab/Material_01");
    private static final Iri phase = iri("http://www.theworldavatar.com/kb/ontolab/SolidPhase_01");
    private static final Iri phaseComponent = iri("http://www.theworldavatar.com/kb/ontolab/PhaseComponent_01");
    private static final Iri species = iri("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
    private static final Iri molecularFormula = iri("http://www.theworldavatar.com/kb/ontospecies/MolecularFormula_0a1489ff-c315-4607-93fc-b70b633bf060");
    private static final Iri elementNumber = iri("http://www.theworldavatar.com/kb/ontospecies/ElementNumber_0a1489ff-c315-4607-93fc-b70b633bf060_Br");

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
        TriplePattern updatePattern = quality.has(hasQualitativeValue, qualitativeValue);
        TriplePattern updatePattern2 = quality.has(isPropertyOf, tag);
        TriplePattern updatePattern3 = tag.has(isAttachedTo, bottle);
        TriplePattern updatePattern4 = bottle.has(isFilledWith, chemical);
        TriplePattern updatePattern5 = chemical.has(refersToMaterial, material);
        TriplePattern updatePattern6 = material.has(thermodynamicBehavior, phase);
        TriplePattern updatePattern7 = phase.has(isComposedOfSubsystem, phaseComponent);
        TriplePattern updatePattern8 = phaseComponent.has(representsOccurenceOf, species);
        TriplePattern updatePattern9 = species.has(hasMolecularFormula, molecularFormula);
        TriplePattern updatePattern10 = molecularFormula.isA(MolecularFormula);
        TriplePattern updatePattern11 = molecularFormula.has(hasElementNumber, elementNumber);
        TriplePattern updatePattern12 = elementNumber.has(hasNumberOfElement, "3");
        TriplePattern updatePattern13 = species.has(altLabel, "test");
        TriplePattern updatePattern14 = species.has(altLabel, "testing");
        InsertDataQuery insert = Queries.INSERT_DATA(updatePattern, updatePattern2, updatePattern3, updatePattern4, updatePattern5, updatePattern6, updatePattern7, updatePattern8, updatePattern9, updatePattern10, updatePattern11, updatePattern12, updatePattern13, updatePattern14);
        insert.prefix(PREFIX_ONTOCAPE_CPS_BEHAVIOR, PREFIX_ONTOCAPE_MATERIAL, PREFIX_ONTOCAPE_PHASE_SYSTEM, PREFIX_ONTOCAPE_SYSTEM, PREFIX_ONTODEVICE, PREFIX_ONTOKIN, PREFIX_ONTOLAB, PREFIX_ONTOSPECIES, PREFIX_SKOS);
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
    public void queryQualitySuccessAndFail() throws IOException {
        String IRI = builder.queryForQualityWithHasQualitativeValue("https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontodevice/tag_01_status", IRI);

        //provide invalid data IRI
        try {
            IRI = builder.queryForQualityWithHasQualitativeValue("https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for quality IRI!", e.getMessage());
        }

        //remove hasQualitativeValue link between Quality and QualitativeValue
        TriplePattern Pattern = quality.has(hasQualitativeValue, qualitativeValue);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTODEVICE);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForQualityWithHasQualitativeValue("https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for quality IRI!", e.getMessage());
        }
    }

    @Test
    public void queryTagSuccessAndFail() throws IOException {
        String IRI = builder.queryForTagWithQualityIRI("http://www.theworldavatar.com/kb/ontodevice/tag_01_status");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontodevice/tag_01", IRI);

        //invalid Quality IRI
        try {
            IRI = builder.queryForTagWithQualityIRI("http://www.theworldavatar.com/kb/ontodevice/tag_01_stat");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for tag IRI!", e.getMessage());
        }

        //remove isPropertyOf link between Tag and Quality
        TriplePattern Pattern = quality.has(isPropertyOf, tag);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTODEVICE);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForTagWithQualityIRI("http://www.theworldavatar.com/kb/ontodevice/tag_01_status");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for tag IRI!", e.getMessage());
        }
    }

    @Test
    public void queryBottleSuccessAndFail() throws IOException {
        String IRI = builder.queryForBottleWithIsAttachedTo("http://www.theworldavatar.com/kb/ontodevice/tag_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontolab/Bottle_01", IRI);

        //invalid tag IRI
        try {
            IRI = builder.queryForBottleWithIsAttachedTo("http://www.theworldavatar.com/kb/ontodevice/tag_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for bottle IRI!", e.getMessage());
        }

        //remove isPropertyOf link between Tag and Quality
        TriplePattern Pattern = tag.has(isAttachedTo, bottle);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTODEVICE);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForBottleWithIsAttachedTo("http://www.theworldavatar.com/kb/ontodevice/tag_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for tag IRI!", e.getMessage());
        }
    }

    @Test
    public void queryChemicalSuccessAndFail() throws IOException {
        String IRI = builder.queryForChemicalWithIsFilledWith("http://www.theworldavatar.com/kb/ontolab/Bottle_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontolab/Chemical_01", IRI);

        //invalid bottle IRI
        try {
            IRI = builder.queryForChemicalWithIsFilledWith("http://www.theworldavatar.com/kb/ontolab/Bottle_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for chemical IRI!", e.getMessage());
        }

        //remove isFilledWith link between Bottle and Chemical
        TriplePattern Pattern = bottle.has(isFilledWith, chemical);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOLAB);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForBottleWithIsAttachedTo("http://www.theworldavatar.com/kb/ontolab/Bottle_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for chemical IRI!", e.getMessage());
        }
    }
    
    @Test
    public void queryMaterialSuccessAndFail() throws IOException {
        String IRI = builder.queryForMaterialWithRefersToMaterial("http://www.theworldavatar.com/kb/ontolab/Chemical_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontolab/Material_01", IRI);

        //invalid chemical IRI
        try {
            IRI = builder.queryForMaterialWithRefersToMaterial("http://www.theworldavatar.com/kb/ontolab/Chemical_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for material IRI!", e.getMessage());
        }

        //remove refersToMaterial link between Chemical and Material
        TriplePattern Pattern = chemical.has(refersToMaterial, material);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOCAPE_CPS_BEHAVIOR);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForMaterialWithRefersToMaterial("http://www.theworldavatar.com/kb/ontolab/Chemical_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for material IRI!", e.getMessage());
        }
    }

    @Test
    public void queryPhaseSuccessAndFail() throws IOException {
        String IRI = builder.queryForPhaseWithThermodynamicBehavior("http://www.theworldavatar.com/kb/ontolab/Material_01");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontolab/SolidPhase_01", IRI);

        //invalid material IRI
        try {
            IRI = builder.queryForPhaseWithThermodynamicBehavior("http://www.theworldavatar.com/kb/ontolab/Material_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for phase IRI!", e.getMessage());
        }

        //remove thermodynamicBehavior link between Material and Phase
        TriplePattern Pattern = material.has(thermodynamicBehavior, phase);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOCAPE_MATERIAL);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForPhaseWithThermodynamicBehavior("http://www.theworldavatar.com/kb/ontolab/Material_01");
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
            IRI = builder.queryForPhaseWithThermodynamicBehavior("http://www.theworldavatar.com/kb/ontolab/SolidPhase_01");
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
            IRI = builder.queryForPhaseComponentWithIsComposedOfSubsystem("http://www.theworldavatar.com/kb/ontolab/PhaseComponent_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for species IRI!", e.getMessage());
        }

        //remove representsOccurenceOf link between Phase Component and Species
        TriplePattern Pattern = phaseComponent.has(representsOccurenceOf, species);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOCAPE_PHASE_SYSTEM);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForPhaseWithThermodynamicBehavior("http://www.theworldavatar.com/kb/ontolab/PhaseComponent_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for species IRI!", e.getMessage());
        }
    }

    @Test
    public void queryMolecularFormulaSuccessAndFail() throws IOException {
        String IRI = builder.queryForMolecularFormulaWithHasMolecularFormula("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontospecies/MolecularFormula_0a1489ff-c315-4607-93fc-b70b633bf060", IRI);

        //invalid species IRI
        try {
            IRI = builder.queryForMolecularFormulaWithHasMolecularFormula("http://www.theworldavatar.com/kb/ontospecies/Species");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for molecular formula IRI!", e.getMessage());
        }

        //remove hasMolecularFormula link between Species and Molecular Formula
        TriplePattern Pattern = species.has(hasMolecularFormula, molecularFormula);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOSPECIES);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForMolecularFormulaWithHasMolecularFormula("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for molecular formula IRI!", e.getMessage());
        }
    }

    @Test
    public void queryElementNumberSuccessAndFail() throws IOException {
        String IRI = builder.queryForElementNumberViaHasElementNumber("http://www.theworldavatar.com/kb/ontospecies/MolecularFormula_0a1489ff-c315-4607-93fc-b70b633bf060");
        Assert.assertEquals("http://www.theworldavatar.com/kb/ontospecies/ElementNumber_0a1489ff-c315-4607-93fc-b70b633bf060_Br", IRI);

        //invalid element number IRI
        try {
            IRI = builder.queryForElementNumberViaHasElementNumber("http://www.theworldavatar.com/kb/ontospecies/MolecularFormula");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for element number IRI!", e.getMessage());
        }

        //remove hasElementNumber link between Molecular Formula and Element Number
        TriplePattern Pattern = molecularFormula.has(hasElementNumber, elementNumber);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOKIN);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForMolecularFormulaWithHasMolecularFormula("http://www.theworldavatar.com/kb/ontospecies/MolecularFormula_0a1489ff-c315-4607-93fc-b70b633bf060");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for element number IRI!", e.getMessage());
        }
    }

    @Test
    public void queryNumberOfElementSuccessAndFail() throws IOException {
        String number = builder.queryForNumberViaHasNumberOfElement("http://www.theworldavatar.com/kb/ontospecies/ElementNumber_0a1489ff-c315-4607-93fc-b70b633bf060_Br");
        Assert.assertEquals("3", number);

        //invalid element number IRI
        try {
            number = builder.queryForNumberViaHasNumberOfElement("http://www.theworldavatar.com/kb/ontospecies/ElementNumber");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for number of element!", e.getMessage());
        }

        //remove hasNumberOfElement link between Element Number and Number
        TriplePattern Pattern = elementNumber.has(hasNumberOfElement, "3");
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTOKIN);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            number = builder.queryForNumberViaHasNumberOfElement("http://www.theworldavatar.com/kb/ontospecies/ElementNumber_0a1489ff-c315-4607-93fc-b70b633bf060_Br");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for number of element!", e.getMessage());
        }
    }

    @Test
    public void queryLabelsSuccessAndFail() throws IOException {
        JSONArray a = new JSONArray();
        a = builder.queryForLabelsViaAltLabel("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        Assert.assertEquals(2, a.length());

        //invalid species IRI
        try {
            a = builder.queryForLabelsViaAltLabel("http://www.theworldavatar.com/kb/ontospecies/Species");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for alternate labels!", e.getMessage());
        }

        //remove altLabel link between Species and String values
        TriplePattern Pattern = species.has(altLabel, "test").andHas(altLabel, "testing");
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_SKOS);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            a = builder.queryForLabelsViaAltLabel("http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for alternate labels!", e.getMessage());
        }
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
    	
    
   
    
    
    

