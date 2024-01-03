package uk.ac.cam.cares.jps.agent.caresWeatherStation;

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

import com.github.stefanbirkner.systemlambda.SystemLambda;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * This test class is to test the SparqlHandler with a running blazegraph triple store
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
    JSONObject weatherDataReadings;

    // Default list of timestamps
    private final String[] timestamps = {"2022-07-11T16:10:00Z", "2022-07-11T16:15:00Z", "2022-07-11T16:20:00Z", "2022-07-11T16:25:00Z"};

    // Default lists of JSON keys (also defining the type)
    private final String[] keys = {"tempMax","dewpt","heatindexLow","windchillAvg","precipRate", "precipTotal"};
    private final String[] quantityTypes = {"http://www.ontology-of-units-of-measure.org/resource/om-2/Temperature",
    "https://www.theworldavatar.com/kg/ontoems/DewPoint",
    "https://www.theworldavatar.com/kg/ontoems/HeatIndex",
    "https://www.theworldavatar.com/kg/ontoems/WindChill",
    "https://www.theworldavatar.com/kg/ontoems/PrecipitationRate",
    "https://www.theworldavatar.com/kg/ontoems/Rainfall"};
    // Example prefix for IRIs
    private final String examplePrefix = "example:prefix/api_";
    // IRIs corresponding to the keys
    private ArrayList<String> IRIs;

    //SparqlHandler
    private WeatherSparqlHandler sparqlHandler;

    @Before
    public void initialize() throws IOException {
        // Start the containers
        try {
            // Start Blazegraph container
            blazegraph.start();
        } catch (Exception e) {
            throw new AssertionError("IntegrationTest: Docker container startup failed. Please try running tests again");
        }

        // Create a properties file that points to a dummy mapping folder //
        // Create an empty folder
        String folderName = "mappings";
        File mappingFolder = folder.newFolder(folderName);
        // Add mapping file into the empty folder
        String weatherMappingFile = Paths.get(mappingFolder.getAbsolutePath(), "weather.properties").toString();
        ArrayList<String> weatherMappings = new ArrayList<>();
        IRIs = new ArrayList<>();
        for (String key: keys) {
            weatherMappings.add(key + "="+examplePrefix+key);
            IRIs.add(examplePrefix+key);
        }
        writePropertyFile(weatherMappingFile, weatherMappings);
        // Create and write content to temporary agent.properties file
        String agentPropertiesFile = Paths.get(folder.getRoot().toString(), "agent.properties").toString();
        writePropertyFile(agentPropertiesFile, Collections.singletonList("caresWeatherStation.mappingfolder=TEST_MAPPINGS"));

        // Set endpoint to the triple store. The host and port are read from the container
        String endpoint = "http://" + blazegraph.getHost() + ":" + blazegraph.getFirstMappedPort();
        // Default namespace in blazegraph is "kb"
        endpoint = endpoint + "/blazegraph/namespace/kb/sparql";

        //set up kbClient
        kbClient = new RemoteStoreClient(endpoint, endpoint);

        // Create and write content to temporary client.properties file
        String clientPropertiesFile = Paths.get(folder.getRoot().toString(), "cient.properties").toString();
        writePropertyFile(clientPropertiesFile, Arrays.asList("sparql.query.endpoint=" + endpoint, "sparql.update.endpoint=" + endpoint));
        
        // Create and write content to temporary client.properties file
        String apiPropertiesFile = Paths.get(folder.getRoot().toString(), "api.properties").toString();
        writePropertyFile(apiPropertiesFile, Arrays.asList("weather.stationId=12345"));

        //create SparqlHandler
        try {
            SystemLambda.withEnvironmentVariable("TEST_MAPPINGS", mappingFolder.getCanonicalPath()).execute(() -> {
                sparqlHandler = new WeatherSparqlHandler(agentPropertiesFile, clientPropertiesFile, apiPropertiesFile);
            });
        }
        catch (Exception e) {
            //no exception should be thrown here
        }
    }

    @Before
    public void createExampleReadings() {
        double value =0.0;
        weatherDataReadings = new JSONObject();
        JSONArray jsArr= new JSONArray();
        for(int i=0; i<timestamps.length;i++) {
            JSONObject currentWeatherData = new JSONObject();
            currentWeatherData.put(CARESWeatherStationInputAgent.timestampKey, timestamps[i]);
            JSONObject measurements = new JSONObject();
            for(String key: keys) {
                measurements.put(key, value);
            }
            currentWeatherData.put("metric_si",measurements);
            currentWeatherData.put("lat", 1.304);
            currentWeatherData.put("lon", 103.774);
            jsArr.put(i,currentWeatherData);
            value++;
        }
        weatherDataReadings.put("observations",jsArr);
    }


    // Cleaning up containers after each test, otherwise unused containers will first be killed when all tests finished
    @After
    public void stopContainers() {
        if (blazegraph.isRunning()) {
            blazegraph.stop();
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

    @Test
    public void testMeasureForInstantiateIfNotExist() {
        //first run of instantiateIfNotExist()
        sparqlHandler.instantiateIfNotExist(weatherDataReadings);

        //second run of instantiateIfNotExist()
        sparqlHandler.instantiateIfNotExist(weatherDataReadings);

        //test for correct instantiation and presence of duplicates
        Variable var = SparqlBuilder.var("var");
        TriplePattern queryPattern = var.isA(iri("http://www.ontology-of-units-of-measure.org/resource/om-2/Measure"));
        SelectQuery query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        //there are 6 data IRIs and each of them should be a rdf:type om:Measure
        Assert.assertEquals(6, queryResult.length());

        //check for label of each data IRI
        String[] list = {"Temperature","Dew Point","Heat Index","Wind Chill","Precipitation Rate", "Rainfall"};
        //check label of reporting station instance
        for (int i = 0; i < IRIs.size(); i++) {
        queryPattern = iri(IRIs.get(i)).has(iri("http://www.w3.org/2000/01/rdf-schema#label"), var);
        query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        Assert.assertEquals(list[i], queryResult.getJSONObject(0).getString("var"));
        }

        //check for correct units
        String[] unitList = {"http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius",
        "http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius",
        "http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius",
        "http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius",
        "http://www.ontology-of-units-of-measure.org/resource/om-2/millimetrePerHour",
        "http://www.ontology-of-units-of-measure.org/resource/om-2/millimetre"};

        for (int i = 0; i < IRIs.size(); i++) {
        queryPattern = iri(IRIs.get(i)).has(iri("http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit"), var);
        query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        Assert.assertEquals(unitList[i], queryResult.getJSONObject(0).getString("var"));
        }
    }

    @Test
    public void testQuantityForInstantiateIfNotExist() {
        //first run of instantiateIfNotExist()
        sparqlHandler.instantiateIfNotExist(weatherDataReadings);

        //second run of instantiateIfNotExist()
        sparqlHandler.instantiateIfNotExist(weatherDataReadings);

        //check whether a quantity IRI has been created for each data IRI and linked via om:hasValue
        Variable var = SparqlBuilder.var("var");

        for (int i = 0; i < IRIs.size(); i++) {
            TriplePattern queryPattern = var.has(iri("http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue"), iri(IRIs.get(i)));
            SelectQuery query = Queries.SELECT();
            query.select(var).where(queryPattern);
            kbClient.setQuery(query.getQueryString());
            JSONArray queryResult = kbClient.executeQuery();
            Assert.assertTrue(queryResult != null);
            Assert.assertTrue(queryResult.getJSONObject(0).getString("var").contains("https://www.theworldavatar.com/kg/ontoems/Quantity_"));
        }

        //check total number of quantity IRI created for duplicates
        Variable var2 = SparqlBuilder.var("var2");
        TriplePattern queryPattern = var.has(iri("http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue"), var2);
        SelectQuery query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertEquals(6, queryResult.length());

        //check whether rdf:type has been assigned correctly to each quantity IRI
        for (int i = 0; i < IRIs.size(); i++) {
            queryPattern = var.has(iri("http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue"), iri(IRIs.get(i)));
            TriplePattern queryPattern2 = var.isA(var2);
            query = Queries.SELECT();
            query.select(var2).where(queryPattern, queryPattern2);
            kbClient.setQuery(query.getQueryString());
            queryResult = kbClient.executeQuery();
            Assert.assertTrue(queryResult != null);
            Assert.assertEquals(quantityTypes[i], queryResult.getJSONObject(0).getString("var2"));
        }
    }

    @Test
    public void testAggregateFunctionForInstantiateIfNotExist() {
        //first run of instantiateIfNotExist()
        sparqlHandler.instantiateIfNotExist(weatherDataReadings);

        //second run of instantiateIfNotExist()
        sparqlHandler.instantiateIfNotExist(weatherDataReadings);

        Variable var = SparqlBuilder.var("var");
        Variable var2 = SparqlBuilder.var("var2");

        //check for number of aggregate function instances
        TriplePattern queryPattern = var.has(iri("http://www.ontology-of-units-of-measure.org/resource/om-2/hasAggregateFunction"), var2);
        SelectQuery query = Queries.SELECT();
        query.select(var2).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertTrue(queryResult != null);
        Assert.assertEquals(4, queryResult.length());

        //List of data IRIs that have aggregate functions
        String[] list = {"example:prefix/api_tempMax", "example:prefix/api_heatindexLow","example:prefix/api_windchillAvg","example:prefix/api_precipTotal"};
        //List of aggregate functions IRI
        String[] aggFunctionList = {"http://www.ontology-of-units-of-measure.org/resource/om-2/maximum",
        "http://www.ontology-of-units-of-measure.org/resource/om-2/minimum",
        "http://www.ontology-of-units-of-measure.org/resource/om-2/average", 
        "http://www.ontology-of-units-of-measure.org/resource/om-2/sum"};

        //check for correct aggregate function
        for (int i = 0; i < list.length; i++) {
            queryPattern = var.has(iri("http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue"), iri(list[i]))
            .andHas(iri("http://www.ontology-of-units-of-measure.org/resource/om-2/hasAggregateFunction"), var2);
            query = Queries.SELECT();
            query.select(var2).where(queryPattern);
            kbClient.setQuery(query.getQueryString());
            queryResult = kbClient.executeQuery();
            Assert.assertEquals(aggFunctionList[i], queryResult.getJSONObject(0).getString("var2"));
        }
    }

    @Test
    public void testReportingStationForInstantiateIfNotExist() {
        //first run of instantiateIfNotExist()
        sparqlHandler.instantiateIfNotExist(weatherDataReadings);

        //second run of instantiateIfNotExist()
        sparqlHandler.instantiateIfNotExist(weatherDataReadings);

        Variable var = SparqlBuilder.var("var");
        Variable var2 = SparqlBuilder.var("var2");

        //check for number of triples that has ontoems:reports as the predicate
        TriplePattern queryPattern = var.has(iri("https://www.theworldavatar.com/kg/ontoems/reports"), var2);
        SelectQuery query = Queries.SELECT();
        query.select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertTrue(queryResult != null);
        Assert.assertEquals(6, queryResult.length());

        //check that each of the subject with predicate ontoems:reports are actually the same instance
        //meaning there is only one reporting station instance
        for (int i = 0; i < queryResult.length() - 1; i++) {
            Assert.assertEquals(queryResult.getJSONObject(i).getString("var"), queryResult.getJSONObject(i+1).getString("var"));
        }

        //check rdf:type of reporting station instance
        String reportingStationIRI = queryResult.getJSONObject(0).getString("var");
        queryPattern = iri(reportingStationIRI).isA(var2);
        query = Queries.SELECT();
        query.select(var2).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        Assert.assertEquals("https://www.theworldavatar.com/kg/ontoems/ReportingStation", queryResult.getJSONObject(0).getString("var2"));

        //check label of reporting station instance
        queryPattern = iri(reportingStationIRI).has(iri("http://www.w3.org/2000/01/rdf-schema#label"), var2);
        query = Queries.SELECT();
        query.select(var2).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        Assert.assertEquals("Weather Station 12345", queryResult.getJSONObject(0).getString("var2"));

    }

    @Test
    public void testGeoLocationForInstantiateIfNotExist() {
        //first run of instantiateIfNotExist()
        sparqlHandler.instantiateIfNotExist(weatherDataReadings);

        //second run of instantiateIfNotExist()
        sparqlHandler.instantiateIfNotExist(weatherDataReadings);

        Variable var = SparqlBuilder.var("var");
        Variable var2 = SparqlBuilder.var("var2");

        //check for number of triples that has ontodevice:hasGeoLocation as the predicate
        TriplePattern queryPattern = var.has(iri("https://www.theworldavatar.com/kg/ontodevice/hasGeoLocation"), var2);
        SelectQuery query = Queries.SELECT();
        query.select(var2).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        Assert.assertTrue(queryResult != null);
        Assert.assertEquals(1, queryResult.length());

        //check rdf:type of GeoLocation Instance
        String geoLocationIRI = queryResult.getJSONObject(0).getString("var2");
        queryPattern = iri(geoLocationIRI).isA(var2);
        query = Queries.SELECT();
        query.select(var2).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        Assert.assertEquals("http://www.opengis.net/ont/sf#Point", queryResult.getJSONObject(0).getString("var2"));
        
        //check for wktLiteral
        queryPattern = iri(geoLocationIRI).has(iri("http://www.opengis.net/ont/geosparql#asWKT"), var2);
        query = Queries.SELECT();
        query.select(var2).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        queryResult = kbClient.executeQuery();
        Assert.assertEquals("POINT(103.774 1.304)", queryResult.getJSONObject(0).getString("var2"));
    }
}