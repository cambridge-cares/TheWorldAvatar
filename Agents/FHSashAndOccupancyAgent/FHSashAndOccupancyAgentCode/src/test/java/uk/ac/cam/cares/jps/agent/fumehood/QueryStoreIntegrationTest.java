package uk.ac.cam.cares.jps.agent.fumehood;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.junit.*;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

/**
 * This test class is to test the QueryStore with a running KG.
 */


@Ignore("Requires triple store endpoint set up and running (using testcontainers)\n" +
        "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")

@Testcontainers
public class QueryStoreIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);

    //remote store client
    private RemoteStoreClient kbClient;

        /**
     * Namespaces for ontologies
     */
	public static final String ONTODEVICE_NS = "https://www.theworldavatar.com/kg/ontodevice/";
	public static final String ONTOBMS_NS = "https://www.theworldavatar.com/kg/ontobms/";
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String SAREF_NS = "https://saref.etsi.org/core/";
    public static final String OM_NS = "http://www.ontology-of-units-of-measure.org/resource/om-2/"; 

	/**
     * Prefixes
     */ 
	private static final Prefix PREFIX_ONTODEVICE = SparqlBuilder.prefix("ontodevice", iri(ONTODEVICE_NS));
	private static final Prefix PREFIX_ONTOBMS = SparqlBuilder.prefix("ontobms", iri(ONTOBMS_NS));
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));
    private static final Prefix PREFIX_SAREF = SparqlBuilder.prefix("saref", iri(SAREF_NS));
    private static final Prefix PREFIX_OM = SparqlBuilder.prefix("om", iri(OM_NS));
    
	/**
     * Relationships
     */ 
    private static final Iri label = PREFIX_RDFS.iri("label");
    private static final Iri hasState = PREFIX_SAREF.iri("hasState");
    private static final Iri hasSashOpenPercentage = PREFIX_ONTOBMS.iri("hasSashOpenPercentage");
    private static final Iri hasValue = PREFIX_OM.iri("hasValue");

    /**
     * Classes
     */
    private static final Iri OccupiedState = PREFIX_ONTODEVICE.iri("OccupiedState");
    private static final Iri Fumehood = PREFIX_ONTOBMS.iri("FumeHood");
    private static final Iri WalkinFumehood = PREFIX_ONTOBMS.iri("WalkInFumeHood");
    private static final Iri Percentage = PREFIX_OM.iri("Percentage");

    String bgUsername;
    String bgPassword;
    String dbUrl;
    String dbUsername;
    String dbPassword;
    String endpoint;
    String propertiesFile;

    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";
 	
    @Before
    public void initializeMockTriplesAndAgent() throws IOException {
        // Start the containers
        try {
            // Start Blazegraph container
            blazegraph.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }

        String FH_01_test_IRI = examplePrefix + "FH01";
        String WFH_02_test_IRI = examplePrefix + "WFH02";
        String FH_03_test_IRI = examplePrefix + "FH03";
        String FH_01_Sash_IRI = examplePrefix + "FH01_Sash";
        String V_FH_01_Sash_IRI = examplePrefix + "V_FH01_Sash";
        String WFH_02_OccupiedState_IRI = examplePrefix + "WFH02_OccupiedState";
        String FH_03_Sash_IRI = examplePrefix + "FH03_Sash";
        String V_FH_03_Sash_IRI = examplePrefix + "V_FH03_Sash";
        String FH_03_OccupiedState_IRI = examplePrefix + "FH03_OccupiedState";

        TriplePattern updatePattern = iri(FH_01_test_IRI).isA(Fumehood).andHas(label, "FH_01").andHas(hasSashOpenPercentage, iri(FH_01_Sash_IRI));
        TriplePattern updatePattern2 = iri(WFH_02_test_IRI).isA(WalkinFumehood).andHas(label, "WFH_02").andHas(hasState, iri(WFH_02_OccupiedState_IRI));
        TriplePattern updatePattern3 = iri(FH_03_test_IRI).isA(Fumehood).andHas(label, "FH_03").andHas(hasState, iri(FH_03_OccupiedState_IRI)).andHas(hasSashOpenPercentage, iri(FH_03_Sash_IRI));
        TriplePattern updatePattern4 = iri(FH_01_Sash_IRI).isA(Percentage);
        TriplePattern updatePattern5 = iri(WFH_02_OccupiedState_IRI).isA(OccupiedState);
        TriplePattern updatePattern6 = iri(FH_03_OccupiedState_IRI).isA(OccupiedState);
        TriplePattern updatePattern7 = iri(FH_03_Sash_IRI).isA(Percentage);
        TriplePattern updatePattern8 = iri(FH_01_Sash_IRI).has(hasValue, V_FH_01_Sash_IRI);
        TriplePattern updatePattern9 = iri(FH_03_Sash_IRI).has(hasValue, V_FH_03_Sash_IRI);

        InsertDataQuery insert = Queries.INSERT_DATA(updatePattern, updatePattern2, updatePattern3, updatePattern4, updatePattern5, updatePattern6, updatePattern7, updatePattern8, updatePattern9);
        insert.prefix(PREFIX_OM, PREFIX_ONTOBMS, PREFIX_ONTODEVICE, PREFIX_RDFS, PREFIX_SAREF);
        
        // Set endpoint to the triple store. The host and port are read from the container
        endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        // Default namespace in blazegraph is "kb"
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql";

        // Set up a kb client that points to the location of the triple store
        kbClient = new RemoteStoreClient();
        kbClient.setUpdateEndpoint(endpoint);
        kbClient.setQueryEndpoint(endpoint);
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
    public void testQueryForFHandWFHDevices() throws IOException {
        QueryStore queryStore = new QueryStore("Invalid endpoint", "Invalid endpoint", bgUsername, bgPassword);
        
        Map<String, List<String>> map = new HashMap<>();
        
        try {
    	map = queryStore.queryForFHandWFHDevices();
        } catch (Exception e) {
            Assert.assertTrue(e.toString().contains("Unable to query for fumehood and/or walkin-fumehood devices and their labels!"));
        }
        
        queryStore = new QueryStore(endpoint, endpoint, bgUsername, bgPassword);
        map = queryStore.queryForFHandWFHDevices();
        Assert.assertTrue(map.get("Label").get(map.get("FHandWFH").indexOf(examplePrefix + "FH01")).contains("FH_01"));
        Assert.assertTrue(map.get("Label").get(map.get("FHandWFH").indexOf(examplePrefix + "WFH02")).contains("WFH_02"));
        Assert.assertTrue(map.get("Label").get(map.get("FHandWFH").indexOf(examplePrefix + "FH03")).contains("FH_03"));
    }

    @Test
    public void testQueryForOccupiedState() throws IOException {
        QueryStore queryStore = new QueryStore("Invalid endpoint", "Invalid endpoint", bgUsername, bgPassword);
        
    	String result = queryStore.queryForOccupiedState("Invalid IRI");
        Assert.assertTrue(result.contains("This device does not have a occupied state."));

        queryStore = new QueryStore(endpoint, endpoint, bgUsername, bgPassword);

        result = queryStore.queryForOccupiedState(examplePrefix + "FH01");
        Assert.assertTrue(result.contains("This device does not have a occupied state."));

        result = queryStore.queryForOccupiedState(examplePrefix + "WFH02");
        Assert.assertTrue(result.contains(examplePrefix + "WFH02_OccupiedState"));

        result = queryStore.queryForOccupiedState(examplePrefix + "FH03");
        Assert.assertTrue(result.contains(examplePrefix + "FH03_OccupiedState"));
    }

    @Test
    public void testQueryForSashOpening() throws IOException {
        QueryStore queryStore = new QueryStore("Invalid endpoint", "Invalid endpoint", bgUsername, bgPassword);
        
    	String result = queryStore.queryForSashOpening("Invalid IRI");
        Assert.assertTrue(result.contains("This device does not have a Sash Opening Percentage."));

        queryStore = new QueryStore(endpoint, endpoint, bgUsername, bgPassword);

        result = queryStore.queryForSashOpening(examplePrefix + "FH01");
        Assert.assertTrue(result.contains(examplePrefix + "V_FH01_Sash"));

        result = queryStore.queryForSashOpening(examplePrefix + "WFH02");
        Assert.assertTrue(result.contains("This device does not have a Sash Opening Percentage."));

        result = queryStore.queryForSashOpening(examplePrefix + "FH03");
        Assert.assertTrue(result.contains(examplePrefix + "V_FH03_Sash"));
    }
    
}
    	
    
   
    
    
    

