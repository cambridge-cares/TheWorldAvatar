package uk.ac.cam.cares.jps.agent.esphome;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
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
 * This test class is to test the Query Builder with a running KG.
 */


@Ignore("Requires both triple store endpoint set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")

@Testcontainers
public class QueryBuilderIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("docker.cmclinnovations.com/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);

    @Rule
    //temp folder for temp client.properties file
    public TemporaryFolder folder = new TemporaryFolder();

    //QueryAgent
    private QueryBuilder builder;

    //endpoint
    String endpoint;

    // Set up a kb client that points to the location of the triple store
    RemoteStoreClient kbClient = new RemoteStoreClient();

    /**
     * Namespaces for ontologies
     */
	public static final String ONTODEVICE_NS = "https://www.theworldavatar.com/kg/ontodevice/";
    public static final String SAREF_NS = "https://saref.etsi.org/core/";
    public static final String OM_NS = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    
	/**
     * Prefixes
     */ 
	private static final Prefix PREFIX_ONTODEVICE = SparqlBuilder.prefix("ontodevice", iri(ONTODEVICE_NS));
    private static final Prefix PREFIX_SAREF = SparqlBuilder.prefix("saref", iri(SAREF_NS));
    private static final Prefix PREFIX_OM = SparqlBuilder.prefix("om", iri(OM_NS));

	/**
     * Relationships
     */ 
	private static final Iri hasSetpoint = PREFIX_ONTODEVICE.iri("hasSetpoint");
    private static final Iri hasState = PREFIX_SAREF.iri("hasState");
    private static final Iri hasQuantity = PREFIX_ONTODEVICE.iri("hasQuantity");
    private static final Iri hasValue = PREFIX_OM.iri("hasValue");
    private static final Iri hasNumericalValue = PREFIX_OM.iri("hasNumericalValue");
    /**
     * Instances IRIs
     */
    private static final Iri state = iri("http://www.theworldavatar.com/kg/ontodevice/tag_01_status");
    private static final Iri device = iri("http://www.theworldavatar.com/kg/ontodevice/device_01");
    private static final Iri setpoint = iri("http://www.theworldavatar.com/kg/ontodevice/setpoint_01");
    private static final Iri quantity = iri("http://www.theworldavatar.com/kg/ontodevice/quantity_01");
    private static final Iri measure = iri("http://www.theworldavatar.com/kg/ontodevice/measure_01");
    /**
     * Classes
     */
    private static final Iri State = PREFIX_SAREF.iri("State");
    private static final Iri Device = PREFIX_SAREF.iri("Device");
    private static final Iri Setpoint = PREFIX_ONTODEVICE.iri("Setpoint");
    private static final Iri Quantity = PREFIX_OM.iri("Quantity");
    private static final Iri Measure = PREFIX_OM.iri("Measure");

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
        builder = new QueryBuilder(propertiesFile);

        //Initialise mock triples in triple store
        TriplePattern updatePattern = device.has(hasState, state);
        TriplePattern updatePattern2 = device.has(hasSetpoint, setpoint);
        TriplePattern updatePattern3 = setpoint.has(hasQuantity, quantity);
        TriplePattern updatePattern4 = quantity.has(hasValue, measure);
        TriplePattern updatePattern5 = measure.has(hasNumericalValue, 20.0);
        TriplePattern updatePattern6 = device.isA(Device);
        TriplePattern updatePattern7 = state.isA(State);
        TriplePattern updatePattern8 = setpoint.isA(Setpoint);
        TriplePattern updatePattern9 = quantity.isA(Quantity);
        TriplePattern updatePattern10 = measure.isA(Measure);
        InsertDataQuery insert = Queries.INSERT_DATA(updatePattern, updatePattern2, updatePattern3, updatePattern4, updatePattern5, updatePattern6, updatePattern7, updatePattern8, updatePattern9, updatePattern10);
        insert.prefix(PREFIX_OM, PREFIX_ONTODEVICE, PREFIX_SAREF);
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
    public void queryDeviceSuccessAndFail() throws IOException {
        String IRI = builder.queryForDeviceWithStateIRI("http://www.theworldavatar.com/kg/ontodevice/tag_01_status");
        Assert.assertEquals("http://www.theworldavatar.com/kg/ontodevice/device_01", IRI);

        //invalid state IRI
        try {
            IRI = builder.queryForDeviceWithStateIRI("http://www.theworldavatar.com/kg/ontodevice/tag_01_stat");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for device IRI via saref:hasState!", e.getMessage());
        }

        //remove hasState link between device and state
        TriplePattern Pattern = device.has(hasState, state);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_SAREF);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForDeviceWithStateIRI("http://www.theworldavatar.com/kg/ontodevice/tag_01_status");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for device IRI via saref:hasState!", e.getMessage());
        }
    }

    @Test
    public void querySetpointSuccessAndFail() throws IOException {
        String IRI = builder.queryForSetpointWithHasSetpoint("http://www.theworldavatar.com/kg/ontodevice/device_01");
        Assert.assertEquals("http://www.theworldavatar.com/kg/ontodevice/setpoint_01", IRI);

        //invalid device IRI
        try {
            IRI = builder.queryForSetpointWithHasSetpoint("http://www.theworldavatar.com/kg/ontodevice/device_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for setpoint IRI via ontodevice:hasSetpoint!", e.getMessage());
        }

        //remove hasSetpoint between device and setpoint
        TriplePattern Pattern = device.has(hasSetpoint, setpoint);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_ONTODEVICE);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForSetpointWithHasSetpoint("http://www.theworldavatar.com/kg/ontodevice/device_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for setpoint IRI via ontodevice:hasSetpoint!", e.getMessage());
        }
    }

    @Test
    public void queryForQuantitySuccessAndFail() throws IOException {
        String IRI = builder.queryForQuantityWithHasQuantity("http://www.theworldavatar.com/kg/ontodevice/setpoint_01");
        Assert.assertEquals("http://www.theworldavatar.com/kg/ontodevice/quantity_01", IRI);

        //invalid setpoint IRI
        try {
            IRI = builder.queryForQuantityWithHasQuantity("http://www.theworldavatar.com/kg/ontodevice/setpoint_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for quantity IRI via ontodevice:hasQuantity!", e.getMessage());
        }

        //remove hasQuantity between setpoint and quantity
        TriplePattern pattern = setpoint.has(hasQuantity, quantity);
        DeleteDataQuery delete = Queries.DELETE_DATA(pattern);
        delete.prefix(PREFIX_ONTODEVICE);
        kbClient.executeUpdate(delete.getQueryString());
        try {
            IRI = builder.queryForQuantityWithHasQuantity("http://www.theworldavatar.com/kg/ontodevice/setpoint_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for quantity IRI via ontodevice:hasQuantity!", e.getMessage());
        }


    }

    @Test
    public void queryMeasureSuccessAndFail() throws IOException {
        String IRI = builder.queryForMeasureWithHasValue("http://www.theworldavatar.com/kg/ontodevice/quantity_01");
        Assert.assertEquals("http://www.theworldavatar.com/kg/ontodevice/measure_01", IRI);

        //invalid quantity IRI
        try {
            IRI = builder.queryForMeasureWithHasValue("http://www.theworldavatar.com/kg/ontodevice/quantity_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for measure IRI via om:hasValue!", e.getMessage());
        }

        //remove hasValue link between quantity and measure
        TriplePattern Pattern = quantity.has(hasValue, measure);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_OM);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            IRI = builder.queryForMeasureWithHasValue("http://www.theworldavatar.com/kg/ontodevice/quantity_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for measure IRI via om:hasValue!", e.getMessage());
        }
    }
    
    @Test
    public void queryNumericalValueSuccessAndFail() throws IOException {
        Double value = builder.queryForNumericalValueWithHasNumericalValue("http://www.theworldavatar.com/kg/ontodevice/measure_01");
        Assert.assertTrue(value == 20);

        //invalid measure IRI
        try {
            value = builder.queryForNumericalValueWithHasNumericalValue("http://www.theworldavatar.com/kg/ontodevice/measure_02");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for numerical value via om:hasNumericalValue!", e.getMessage());
        }

        //remove hasNumericalValue link between meaure and 20.0
        TriplePattern Pattern = measure.has(hasNumericalValue, 20.0);
        DeleteDataQuery delete = Queries.DELETE_DATA(Pattern);
        delete.prefix(PREFIX_OM);
        kbClient.executeUpdate(delete.getQueryString());

        try {
            value = builder.queryForNumericalValueWithHasNumericalValue("http://www.theworldavatar.com/kg/ontodevice/measure_01");
        } catch (Exception e) {
            Assert.assertEquals("Unable to query for numerical value via om:hasNumericalValue!", e.getMessage());
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
    	
    
   
    
    
    

