package uk.ac.cam.cares.jps.agent.carpark;

import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.*;
import org.junit.rules.TemporaryFolder;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * This test class is to test the SparqlHandler with a running blazegraph triple store
 * test for instantiateGeoSpatialInfoIfNotExist will require support from the "stack" before it can be properly written
 */
@Ignore("Requires triple store set up and running (using testcontainers)\n" +
       "Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.")

@Testcontainers
public class SparqlHandlerIntegrationTest {

    // Create Docker container with Blazegraph image from CMCL registry (image uses port 9999)
    // For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    @Container
    private final GenericContainer<?> blazegraph = new GenericContainer<>(DockerImageName.parse("ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0"))
            .withExposedPorts(9999);

    // Temporary folder to place a properties file
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();
    
    // Remote store client to query triple store
    private RemoteStoreClient kbClient;

    // Readings used by several tests
    JSONObject carparkDataReadings;

    //keys
    private final String[] keys = {"AvailableLots_1_C","AvailableLots_1_Y","AvailableLots_2_H"};

    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";

    // IRIs corresponding to the keys
    private ArrayList<String> IRIs;

    // Mappings
    List<JSONKeyToIRIMapper> tsmappings;

    //sparql handler
    SparqlHandler testSparqlHandler;

    @Before
    public void initialize() throws Exception {
        // Start the containers
        try {
            blazegraph.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }

        // Create a properties file that points to a dummy mapping folder //
        // Create an empty folder
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Add mapping file into the empty folder
        String carparkMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "carpark.properties").toString();
        ArrayList<String> carparkMappings = new ArrayList<>();
        IRIs = new ArrayList<>();
        for (String key: keys) {
            carparkMappings.add(key + "="+examplePrefix+key);
            IRIs.add(examplePrefix+key);
        }
        writeToFile(carparkMappingFile, carparkMappings);
        // Create and write content to temporary agent.properties file
        String agentPropertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writeToFile(agentPropertiesFile, Collections.singletonList("carpark.mappingfolder=TEST_MAPPINGS"));

        // Set endpoint to the triple store. The host and port are read from the container
        String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        // Default namespace in blazegraph is "kb"
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql";

        //set up kbClient
        kbClient = new RemoteStoreClient(endpoint, endpoint);

        tsmappings = new ArrayList<>();
        tsmappings = readMappings(mappingFolder.getAbsolutePath());

        testSparqlHandler = new SparqlHandler(tsmappings, kbClient);
    }


    // Cleaning up containers after each test, otherwise unused containers will first be killed when all tests finished
    @After
    public void stopContainers() {
        if (blazegraph.isRunning()) {
            blazegraph.stop();
        }
    }

    private void writeToFile(String filepath, List<String> properties) throws IOException {
        FileWriter writer = new FileWriter(filepath, false);
        // Populate file
        for (String s : properties) {
            writer.write(s + "\n");
        }
        // Close the file and return the file
        writer.close();
    }

    @Test
    public void testInstantiateAvailableLotsIfNotExist() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // Set private method to be accessible
        Method instantiateAvailableLotsIfNotExist = SparqlHandler.class.getDeclaredMethod("instantiateAvailableLotsIfNotExist", String.class);
        instantiateAvailableLotsIfNotExist.setAccessible(true);
        for (int i = 0; i < IRIs.size(); i++) {
            instantiateAvailableLotsIfNotExist.invoke(testSparqlHandler, IRIs.get(i));
        }

        //test for correct instantiation and presence of duplicates
        Variable var = SparqlBuilder.var("var");
        TriplePattern queryPattern = var.isA(iri("https://www.theworldavatar.com/kg/ontocarpark/AvailableLots"));
        SelectQuery query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        //there are 3 data IRIs and each of them should be a rdf:type ontocarpark:AvailableLots
        Assert.assertEquals(3, queryResult.length());
    }

    @Test
    public void testInstantiateLotTypeAndAttributesIfNotExist() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // Set private method to be accessible
        Method instantiateAvailableLotsIfNotExist = SparqlHandler.class.getDeclaredMethod("instantiateAvailableLotsIfNotExist", String.class);
        instantiateAvailableLotsIfNotExist.setAccessible(true);
        Method instantiateLotTypeAndAttributesIfNotExist = SparqlHandler.class.getDeclaredMethod("instantiateLotTypeAndAttributesIfNotExist", String.class, String.class);
        instantiateLotTypeAndAttributesIfNotExist.setAccessible(true);
        for (int i = 0; i < IRIs.size(); i++) {
            instantiateAvailableLotsIfNotExist.invoke(testSparqlHandler, IRIs.get(i));
            instantiateLotTypeAndAttributesIfNotExist.invoke(testSparqlHandler, IRIs.get(i), keys[i].split("_")[2]);
        }

        Variable var = SparqlBuilder.var("var");
        TriplePattern queryPattern = var.isA(iri("https://www.theworldavatar.com/kg/ontocarpark/Cars")).andHas(iri("https://www.theworldavatar.com/kg/ontocarpark/hasLots"), iri(IRIs.get(0)));
        SelectQuery query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        //There should only be one IRI with rdf:type ontocarpark:Cars ; ontocarpark:hasLots <example:prefix/api_AvailableLots_1_C>
        Assert.assertEquals(1, queryResult.length());

        queryPattern = iri(IRIs.get(0)).has(iri("http://www.w3.org/2000/01/rdf-schema#label"), var);
        query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        Assert.assertEquals("Cars Available Lots", queryResult.getJSONObject(0).getString("var"));

        queryPattern = var.isA(iri("https://www.theworldavatar.com/kg/ontocarpark/Motorcycles")).andHas(iri("https://www.theworldavatar.com/kg/ontocarpark/hasLots"), iri(IRIs.get(1)));
        query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        //There should only be one IRI with rdf:type ontocarpark:Motorcycles ; ontocarpark:hasLots <example:prefix/api_AvailableLots_1_Y>
        Assert.assertEquals(1, queryResult.length());

        queryPattern = iri(IRIs.get(1)).has(iri("http://www.w3.org/2000/01/rdf-schema#label"), var);
        query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        Assert.assertEquals("Motorcycles Available Lots", queryResult.getJSONObject(0).getString("var"));

        queryPattern = var.isA(iri("https://www.theworldavatar.com/kg/ontocarpark/HeavyVehicles")).andHas(iri("https://www.theworldavatar.com/kg/ontocarpark/hasLots"), iri(IRIs.get(2)));
        query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        //There should only be one IRI with rdf:type ontocarpark:HeavyVehicles ; ontocarpark:hasLots <example:prefix/api_AvailableLots_2_H>
        Assert.assertEquals(1, queryResult.length());

        queryPattern = iri(IRIs.get(2)).has(iri("http://www.w3.org/2000/01/rdf-schema#label"), var);
        query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        Assert.assertEquals("Heavy Vehicles Available Lots", queryResult.getJSONObject(0).getString("var"));
    }

    @Test
    public void testInstantiateCarpark() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // Set private method to be accessible
        Method instantiateCarpark = SparqlHandler.class.getDeclaredMethod("instantiateCarpark", String.class);
        instantiateCarpark.setAccessible(true);

        instantiateCarpark.invoke(testSparqlHandler, examplePrefix + "testCarpark");
        Variable var = SparqlBuilder.var("var");
        TriplePattern queryPattern = var.isA(iri("https://www.theworldavatar.com/kg/ontocarpark/Carpark"));
        SelectQuery query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertEquals(examplePrefix + "testCarpark", queryResult.getJSONObject(0).getString("var"));
    }

    @Test
    public void testInstantiateCarparkLabel() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // Set private method to be accessible
        Method instantiateCarparkLabel = SparqlHandler.class.getDeclaredMethod("instantiateCarparkLabel", String.class, String.class);
        instantiateCarparkLabel.setAccessible(true);

        instantiateCarparkLabel.invoke(testSparqlHandler, examplePrefix + "testCarpark", "Testing carpark name");
        Variable var = SparqlBuilder.var("var");
        TriplePattern queryPattern = iri(examplePrefix + "testCarpark").has(iri("http://www.w3.org/2000/01/rdf-schema#label"), var);
        SelectQuery query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertEquals("Testing Carpark Name", queryResult.getJSONObject(0).getString("var"));
    }

    @Test
    public void testInstantiateCarparkID() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // Set private method to be accessible
        Method instantiateCarparkID = SparqlHandler.class.getDeclaredMethod("instantiateCarparkID", String.class, String.class);
        instantiateCarparkID.setAccessible(true);

        instantiateCarparkID.invoke(testSparqlHandler, examplePrefix + "testCarpark", "1");
        Variable var = SparqlBuilder.var("var");
        TriplePattern queryPattern = iri(examplePrefix + "testCarpark").has(iri("https://www.theworldavatar.com/kg/ontocarpark/hasID"), var);
        SelectQuery query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertEquals("1", queryResult.getJSONObject(0).getString("var"));
    }

    @Test
    public void testInstantiateCarparkAgency() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // Set private method to be accessible
        Method instantiateCarparkAgency = SparqlHandler.class.getDeclaredMethod("instantiateCarparkAgency", String.class, String.class);
        instantiateCarparkAgency.setAccessible(true);

        instantiateCarparkAgency.invoke(testSparqlHandler, examplePrefix + "testCarpark", "HDB");
        Variable var = SparqlBuilder.var("var");
        TriplePattern queryPattern = iri(examplePrefix + "testCarpark").has(iri("https://www.theworldavatar.com/kg/ontocarpark/hasAgency"), var);
        SelectQuery query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertEquals("HDB", queryResult.getJSONObject(0).getString("var"));
    }

    @Test
    public void testInstantiateHasLotType() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // Set private method to be accessible
        Method instantiateHasLotType = SparqlHandler.class.getDeclaredMethod("instantiateHasLotType", String.class, String.class);
        instantiateHasLotType.setAccessible(true);

        instantiateHasLotType.invoke(testSparqlHandler, examplePrefix + "testCarpark", examplePrefix + "cars_lot_type");
        Variable var = SparqlBuilder.var("var");
        TriplePattern queryPattern = iri(examplePrefix + "testCarpark").has(iri("https://www.theworldavatar.com/kg/ontocarpark/hasLotType"), var);
        SelectQuery query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertEquals(examplePrefix + "cars_lot_type", queryResult.getJSONObject(0).getString("var"));
    }

    @Test
    public void testParseCarparkRates() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        Map<String, String> map = new HashMap<>();
        JSONObject rates = new JSONObject();
        rates.put("weekdays_rate_1", "Daily: $1.30 / 30 Mins");
        rates.put("weekdays_rate_2", "-");
        rates.put("saturday_rate", "Sat, Sun / Ph: $1.30 for 1st hr; $0.65 for sub. ½ hr or part thereof.");
        rates.put("sunday_publicholiday_rate", "$2 per entry");

        map = testSparqlHandler.parseCarparkRates(rates);

        Assert.assertEquals("Daily: $1.30 / 30 Mins", map.get("Weekday rates"));
        Assert.assertEquals( "Sat, Sun / Ph: $1.30 for 1st hr; $0.65 for sub. ½ hr or part thereof.", map.get("Saturday rates"));
        Assert.assertEquals("$2 per entry", map.get("Sunday and PH rates"));

        rates.put("sunday_publicholiday_rate", "Same as Saturday");
        map = testSparqlHandler.parseCarparkRates(rates);
        Assert.assertEquals("Sat, Sun / Ph: $1.30 for 1st hr; $0.65 for sub. ½ hr or part thereof.", map.get("Sunday and PH rates"));

        rates.put("sunday_publicholiday_rate", "Same as wkdays");
        map = testSparqlHandler.parseCarparkRates(rates);
        Assert.assertEquals("Daily: $1.30 / 30 Mins", map.get("Sunday and PH rates"));

        rates.put("sunday_publicholiday_rate", "Same as weekdays");
        map = testSparqlHandler.parseCarparkRates(rates);
        Assert.assertEquals("Daily: $1.30 / 30 Mins", map.get("Sunday and PH rates"));

        rates.put("saturday_rate", "Same as wkdays");
        map = testSparqlHandler.parseCarparkRates(rates);
        Assert.assertEquals("Daily: $1.30 / 30 Mins", map.get("Saturday rates"));

        rates.put("saturday_rate", "Same as weekdays");
        map = testSparqlHandler.parseCarparkRates(rates);
        Assert.assertEquals("Daily: $1.30 / 30 Mins", map.get("Saturday rates"));

        rates.put("weekdays_rate_2", "Daily: $1.30 / 30 Mins");
        map = testSparqlHandler.parseCarparkRates(rates);
        Assert.assertEquals("Daily: $1.30 / 30 Mins", map.get("Weekday rates"));

        rates.put("weekdays_rate_2", "5pm-1am: $0.50 for ½ hr");
        map = testSparqlHandler.parseCarparkRates(rates);
        Assert.assertEquals("Daily: $1.30 / 30 Mins ; 5pm-1am: $0.50 for ½ hr", map.get("Weekday rates"));
    }

    @Test
    public void testCheckAndUpdateRates() throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        // Set private method to be accessible
        Method checkAndUpdateRates = SparqlHandler.class.getDeclaredMethod("checkAndUpdateRates", Map.class, String.class);
        checkAndUpdateRates.setAccessible(true);

        Map<String, String> map = new HashMap<>();
        JSONObject rates = new JSONObject();
        rates.put("weekdays_rate_1", "Daily: $1.30 / 30 Mins");
        rates.put("weekdays_rate_2", "-");
        rates.put("saturday_rate", "Sat, Sun / Ph: $1.30 for 1st hr; $0.65 for sub. ½ hr or part thereof.");
        rates.put("sunday_publicholiday_rate", "$2 per entry");

        map = testSparqlHandler.parseCarparkRates(rates);

        checkAndUpdateRates.invoke(testSparqlHandler, map, examplePrefix + "testCarpark");
        Variable var_weekdayrates = SparqlBuilder.var("var_weekdayrates");
        Variable var_saturdayrates = SparqlBuilder.var("var_saturdayrates");
        Variable var_sundayandphrates = SparqlBuilder.var("var_sundayandphrates");
        TriplePattern queryPattern = iri(examplePrefix + "testCarpark").has(iri("https://www.theworldavatar.com/kg/ontocarpark/hasWeekdayRates"), var_weekdayrates)
        .andHas(iri("https://www.theworldavatar.com/kg/ontocarpark/hasSaturdayRates"), var_saturdayrates)
        .andHas(iri("https://www.theworldavatar.com/kg/ontocarpark/hasSundayAndPHRates"), var_sundayandphrates);
        SelectQuery query = Queries.SELECT();
        query.select(var_weekdayrates, var_saturdayrates, var_sundayandphrates).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertEquals("Daily: $1.30 / 30 Mins", queryResult.getJSONObject(0).getString("var_weekdayrates"));
        Assert.assertEquals("Sat, Sun / Ph: $1.30 for 1st hr; $0.65 for sub. Â½ hr or part thereof.", queryResult.getJSONObject(0).getString("var_saturdayrates"));
        Assert.assertEquals("$2 per entry", queryResult.getJSONObject(0).getString("var_sundayandphrates"));

        rates.put("weekdays_rate_1", "Daily: $1.30 / 30 Mins");
        rates.put("weekdays_rate_2", "5pm-1am: $0.50 for ½ hr");
        rates.put("saturday_rate", "Daily: $1.30 / 30 Mins");
        rates.put("sunday_publicholiday_rate", "Same as Saturday");

        map = testSparqlHandler.parseCarparkRates(rates);

        checkAndUpdateRates.invoke(testSparqlHandler, map, examplePrefix + "testCarpark");

        queryResult = kbClient.executeQuery();
        Assert.assertEquals("Daily: $1.30 / 30 Mins ; 5pm-1am: $0.50 for Â½ hr", queryResult.getJSONObject(0).getString("var_weekdayrates"));
        Assert.assertEquals("Daily: $1.30 / 30 Mins", queryResult.getJSONObject(0).getString("var_saturdayrates"));
        Assert.assertEquals("Daily: $1.30 / 30 Mins", queryResult.getJSONObject(0).getString("var_sundayandphrates"));
    }
    

    /**
     * Reads the JSON key to IRI mappings from files in the provided folder.
     * @param mappingFolder The path to the folder in which the mapping files are located.
     * @return 
     */
    private List<JSONKeyToIRIMapper> readMappings(String mappingFolder) throws IOException {
        List<JSONKeyToIRIMapper> mappings = new ArrayList<>();
        File folder = new File(mappingFolder);
        File[] mappingFiles = folder.listFiles();
        // Make sure the folder exists and contains files
        if (mappingFiles == null) {
            throw new IOException("Folder does not exist: " + mappingFolder);
        }
        if (mappingFiles.length == 0) {
            throw new IOException("No files in the folder: " + mappingFolder);
        }
        // Create a mapper for each file
        else {
            for (File mappingFile: mappingFiles) {
                JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(CarparkAgent.TIMESERIES_IRI_PREFIX, mappingFile.getAbsolutePath());
                mappings.add(mapper);
                // Save the mappings back to the file to ensure using same IRIs next time
                mapper.saveToFile(mappingFile.getAbsolutePath());
            }
        }
        return mappings;
    }
}