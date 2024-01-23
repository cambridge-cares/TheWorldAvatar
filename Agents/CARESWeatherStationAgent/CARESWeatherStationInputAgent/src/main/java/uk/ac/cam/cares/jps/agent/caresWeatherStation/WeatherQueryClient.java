package uk.ac.cam.cares.jps.agent.caresWeatherStation;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.sql.Connection;
import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.docker.ContainerClient;
import com.cmclinnovations.stack.clients.ontop.OntopClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;


/**
 * Client to construct queries and instantiations
 * @author Wilson */
public class WeatherQueryClient {
	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(CARESWeatherStationInputAgentLauncher.class);

    /**
     * Log messages
     */
    private static final String UPDATEORQUERY_ERROR_MSG = "Unable to run the following Update/Query the following: ";
    private static final String UPDATE_SUCCESS_MSG = "Successfully inserted the following triples: ";
    private static final String CREATEGEOSPATIAL_ERROR_MSG = "Unable to create geospatial information!";
    private static final String RETRIEVE_LATLONG_ERROR_MSG = "Error retrieving latitude and longitude values from JSONObject!" ;

    /**
     * Namespaces for ontologies
     */
    public static final String ONTOEMS_NS = "https://www.theworldavatar.com/kg/ontoems/";
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String OM_NS = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    public static final String ONTODEVICE_NS = "https://www.theworldavatar.com/kg/ontodevice/";
    public static final String SF_NS = "http://www.opengis.net/ont/sf#"; 
    public static final String GEO_NS = "http://www.opengis.net/ont/geosparql#";

	/**
     * Prefixes
     */ 
	private static final Prefix PREFIX_ONTOEMS = SparqlBuilder.prefix("ontoems", iri(ONTOEMS_NS));
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));
    private static final Prefix PREFIX_OM = SparqlBuilder.prefix("om", iri(OM_NS));

	/**
     * Relationships
     */ 
    private static final Iri label = PREFIX_RDFS.iri("label");
    private static final Iri reports = PREFIX_ONTOEMS.iri("reports");
    private static final Iri hasValue = PREFIX_OM.iri("hasValue");
    private static final Iri hasAggregateFunction = PREFIX_OM.iri("hasAggregateFunction");
    private static final Iri hasUnit = PREFIX_OM.iri("hasUnit");
    private static final Iri symbol = PREFIX_OM.iri("symbol");

    /**
     * Classes
     */
    private static final Iri reportingStation = PREFIX_ONTOEMS.iri("ReportingStation");
    private static final Iri uvIndex = PREFIX_ONTOEMS.iri("UVIndex"); //degree celsius
    private static final Iri windDirection = PREFIX_ONTOEMS.iri("WindDirection"); //degree
    private static final Iri windSpeed = PREFIX_ONTOEMS.iri("WindSpeed"); //metre per second
    private static final Iri windGust = PREFIX_ONTOEMS.iri("WindGust"); //metre per second
    private static final Iri dewPoint = PREFIX_ONTOEMS.iri("DewPoint"); //degree celsius
    private static final Iri windChill = PREFIX_ONTOEMS.iri("WindChill"); //degree celsius
    private static final Iri heatIndex = PREFIX_ONTOEMS.iri("HeatIndex"); //degree celsius
    private static final Iri precipitationRate = PREFIX_ONTOEMS.iri("PrecipitationRate"); //mm per hour
    private static final Iri rainfall = PREFIX_ONTOEMS.iri("Rainfall"); // mm
    private static final Iri relativeHumidity = PREFIX_OM.iri("RelativeHumidity"); //percent
    private static final Iri temperature = PREFIX_OM.iri("Temperature"); //degree celsius
    private static final Iri pressure = PREFIX_OM.iri("Pressure"); // hectopascal
    private static final Iri irradiance = PREFIX_OM.iri("Irradiance"); //wattPerSquareMetre
    private static final Iri maximum = PREFIX_OM.iri("maximum");
    private static final Iri minimum = PREFIX_OM.iri("minimum");
    private static final Iri average = PREFIX_OM.iri("average");
    private static final Iri sum = PREFIX_OM.iri("sum");
    private static final Iri measure = PREFIX_OM.iri("Measure");
    private static final Iri function = PREFIX_OM.iri("Function");
    private static final Iri unit = PREFIX_OM.iri("Unit");
    private static final Iri singularUnit = PREFIX_OM.iri("SingularUnit");
    private static final Iri unitDivision = PREFIX_OM.iri("UnitDivision");
    private static final Iri prefixedUnit = PREFIX_OM.iri("PrefixedUnit");

    //units instances
    private static final Iri percent  = PREFIX_OM.iri("percent"); //unit and singular unit
    private static final Iri degreeCelsius = PREFIX_OM.iri("degreeCelsius"); //unit and singular unit
    private static final Iri degree = PREFIX_OM.iri("degree"); //unit and singular unit
    private static final Iri metrePerSecond = PREFIX_OM.iri("meterPerSecond-Time"); //unit division
    private static final Iri millimetrePerHour = PREFIX_OM.iri("millimetrePerHour"); //unit division
    private static final Iri millimetre = PREFIX_OM.iri("millimetre"); //prefixed unit
    private static final Iri hectoPascal = PREFIX_OM.iri("hectopascal"); //prefixed unit
    private static final Iri wattPerSquareMetre = PREFIX_OM.iri("wattPerSquareMetre"); //unit division


    //client to interact with remote store
    RemoteStoreClient kbClient;

    //list to contain mappings
    private List<JSONKeyToIRIMapper> mappings;

	private WeatherPostGISClient postgisClient;

    //endpoints and credentials
    String sparqlQueryEndpoint;
    String sparqlUpdateEndpoint;
    String sparqlUser;
    String sparqlPassword;
    String dbUrl;
    String dbUser;
    String dbPassword;

    //weather station ID
    private String stationId;

    /**
     * Weather query client constructor
     * @param agentPropertiesFile the filepath of the agent.properties file
     * @param clientPropertiesFile the filepath of the client.properties file
     */
    public WeatherQueryClient(String agentPropertiesFile, String clientPropertiesFile, String apiPropertiesFile) throws IOException {
        try (InputStream input = new FileInputStream(agentPropertiesFile)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            String mappingFolder;
            // Read the mappings folder from the properties file
            try {
                // Read the mappings folder from the properties file
                mappingFolder = System.getenv(prop.getProperty("caresWeatherStation.mappingfolder"));
            }
            catch (NullPointerException e) {
                throw new IOException ("The key caresWeatherStation.mappingfolder cannot be found in the properties file.");
            }
            if (mappingFolder == null) {
                throw new InvalidPropertiesFormatException("The properties file does not contain the key caresWeatherStation.mappingfolder " +
                        "with a path to the folder containing the required JSON key to IRI mappings.");
            }
            // Read the JSON key to IRI mappings from
            readMappings(mappingFolder);
        }
        try {
            //load configs from client.properties file for setting up remote store client
            loadClientConfigs(clientPropertiesFile);
        } catch (Exception e) {
            throw new JPSRuntimeException ("Unable to load properties from the timeseries config file!");
        }
        try {
            //load configs from api.properties file
            loadAPIconfigs(apiPropertiesFile);
        } catch (Exception e) {
            throw new JPSRuntimeException ("Unable to load properties from the api config file!");
        }
    }

    /**
     * Reads the JSON key to IRI mappings from files in the provided folder.
     * @param mappingFolder The path to the folder in which the mapping files are located.
     */
    private void readMappings(String mappingFolder) throws IOException {
        mappings = new ArrayList<>();
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
                JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(CARESWeatherStationInputAgent.generatedIRIPrefix, mappingFile.getAbsolutePath());
                mappings.add(mapper);
                // Save the mappings back to the file to ensure using same IRIs next time
                mapper.saveToFile(mappingFile.getAbsolutePath());
            }
        }
    }

    /**
     * Carries out multiple queries and checks for ABoxes, if the ABoxes do not exist, they will be instantiated
     * @param weatherReadings the JSONObject containing the data retrieved via API
     */
    public void instantiateIfNotExist(JSONObject weatherReadings) {
       List<String> quantityIRIs;
        // Iterate through all mappings (each represents one time series)
        for (JSONKeyToIRIMapper mapping: mappings) {
            //list to contain the quantity IRIs
            quantityIRIs = new ArrayList<>();
            // The IRIs used by the current mapping
            List<String> iris = mapping.getAllIRIs();
            for (int i = 0; i < iris.size(); i++) {
                instantiateMeasureIfNotExist(iris.get(i), mapping.getJSONKey(iris.get(i)));
                String quantityIRI = instantiateQuantityIfNotExist(iris.get(i), mapping.getJSONKey(iris.get(i)));
                quantityIRIs.add(quantityIRI);
            }
            String reportingStationIRI = instantiateReportingStationIfNotExist(quantityIRIs);
            instantiateGeoSpatialInfoIfNotExist(reportingStationIRI, weatherReadings);
        }
        
    }

    /**
     * Check whether all data IRIs has a rdf:type om:Measure and if not, add the data to the remote store
     * @param IRI the data IRI to check for
     * @param jsonKey a key used to identify what type of measure does each data IRI represents
     */
    private void instantiateMeasureIfNotExist(String IRI, String jsonKey) {
        //this is based on the variables retrievable via the API
        String measureLabel = null;
        Iri unitInstance = null;
        Iri unitType = null;
        String unitSymbol = null;
        String unitLabel = null;
        if (jsonKey.contains("solarRadiation")) {
            measureLabel = "Irradiance";
            unitInstance = wattPerSquareMetre;
            unitType = unitDivision;
            unitSymbol = "W/m2";
            unitLabel = "watt per square metre";
        } else if (jsonKey.contains("uv")) {
            measureLabel = "UV Index";
        } else if (jsonKey.contains("winddir")) {
            measureLabel = "Wind Direction";
            unitInstance = degree;
            unitType = singularUnit;
            unitLabel = "degree";
        } else if (jsonKey.contains("humidity")) {
            measureLabel = "Relative Humidity";
            unitInstance = percent;
            unitType = singularUnit;
            unitSymbol = "%";
            unitLabel = "percent";
        } else if (jsonKey.contains("temp")) {
            measureLabel = "Temperature";
            unitInstance = degreeCelsius;
            unitType = singularUnit;
            unitLabel = "degree Celsius";
        } else if (jsonKey.contains("windspeed")) {
            measureLabel = "Wind Speed";
            unitInstance = metrePerSecond;
            unitType = unitDivision;
            unitSymbol = "m/s";
            unitLabel = "metre per second";
        } else if (jsonKey.contains("windgust")) {
            measureLabel = "Wind Gust";
            unitInstance = metrePerSecond;
            unitType = unitDivision;
            unitSymbol = "m/s";
            unitLabel = "metre per second";
        } else if (jsonKey.contains("dewpt")) {
            measureLabel = "Dew Point";
            unitInstance = degreeCelsius;
            unitType = singularUnit;
            unitLabel = "degree Celsius";
        } else if (jsonKey.contains("windchill")) {
            measureLabel = "Wind Chill";
            unitInstance = degreeCelsius;
            unitType = singularUnit;
            unitLabel = "degree Celsius";
        } else if (jsonKey.contains("heatindex")) {
            measureLabel = "Heat Index";
            unitInstance = degreeCelsius;
            unitType = singularUnit;
            unitLabel = "degree Celsius";
        } else if (jsonKey.contains("pressure") && !jsonKey.contains("trend")) {
            measureLabel = "Pressure";
            unitInstance = hectoPascal;
            unitType = prefixedUnit;
            unitSymbol = "hPa";
            unitLabel = "hectopascal";
        } else if (jsonKey.contains("precipRate")) {
            measureLabel = "Precipitation Rate";
            unitInstance = millimetrePerHour;
            unitType = unitDivision;
            unitSymbol = "mm/h";
            unitLabel = "millimetre per hour";
        } else if (jsonKey.contains("precip") && !jsonKey.contains("Rate")) {
            measureLabel = "Rainfall";
            unitInstance = millimetre;
            unitType = prefixedUnit;
            unitSymbol = "mm";
            unitLabel = "millimetre";
        }

        if (measureLabel != null && unitInstance != null && unitType != singularUnit && unitSymbol != null) {
            SelectQuery query = Queries.SELECT();
            Variable var = SparqlBuilder.var("var");
            //create triple pattern:
            // <IRI> rdf:type ?var
            TriplePattern queryPattern = iri(IRI).isA(var);
            query.prefix(PREFIX_OM).select(var).where(queryPattern);
            kbClient.setQuery(query.getQueryString());
            try {
                JSONArray queryResult = kbClient.executeQuery();
                // if the query result is not empty and the rdf:type is equivalent to om:Measure
                if (!queryResult.isEmpty() && queryResult.getJSONObject(0).getString("var") == measure.toString()){
                LOGGER.info(IRI + " already has a rdf:type om:Measure!");
                } else {
                    //create triple pattern:
                    // <IRI> rdf:type om:Measure .
                    // <unitInstance> rdf:type <unitType> ;
                    //                rdfs:label "unitLabel" ;
                    //                om:symbol "unitSymbol" .
                    queryPattern = iri(IRI).isA(measure).andHas(label, measureLabel).andHas(hasUnit, unitInstance);
                    TriplePattern queryPattern2 = unitInstance.isA(unitType).andHas(label, unitLabel).andHas(symbol, unitSymbol);
                    InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern, queryPattern2).prefix(PREFIX_OM, PREFIX_RDFS);
                    kbClient.executeUpdate(insertQuery.getQueryString());
                    LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.getQueryString());
                }
            } catch (Exception e) {
                throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString());
            }
        } else if  (measureLabel != null && unitInstance != null && unitType == singularUnit  && unitSymbol != null) {
            SelectQuery query = Queries.SELECT();
            Variable var = SparqlBuilder.var("var");
            //create triple pattern:
            // <IRI> rdf:type ?var
            TriplePattern queryPattern = iri(IRI).isA(var);
            query.prefix(PREFIX_OM).select(var).where(queryPattern);
            kbClient.setQuery(query.getQueryString());
            try {
                JSONArray queryResult = kbClient.executeQuery();
                // if the query result is not empty and the rdf:type is equivalent to om:Measure
                if (!queryResult.isEmpty() && queryResult.getJSONObject(0).getString("var") == measure.toString()){
                LOGGER.info(IRI + " already has a rdf:type om:Measure!");
                } else {
                    //create triple pattern:
                    // <IRI> rdf:type om:Measure .
                    // <IRI> om:hasUnit <unitInstance> .
                    // <unitInstance> rdf:type <unitType> ;
                    //                rdf:type om:unit ;
                    //                rdfs:label "unitLabel" ;
                    //                om:symbol "unitSymbol" .
                    queryPattern = iri(IRI).isA(measure).andHas(label, measureLabel).andHas(hasUnit, unitInstance);
                    TriplePattern queryPattern2 = unitInstance.isA(unitType).andIsA(unit).andHas(label, unitLabel).andHas(symbol, unitSymbol);
                    InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern, queryPattern2).prefix(PREFIX_OM, PREFIX_RDFS);
                    kbClient.executeUpdate(insertQuery.getQueryString());
                    LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.getQueryString());
                }
            } catch (Exception e) {
                throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString());
            }
        } else if  (measureLabel != null && unitInstance != null && unitType == singularUnit && unitSymbol == null) {
            SelectQuery query = Queries.SELECT();
            Variable var = SparqlBuilder.var("var");
            //create triple pattern:
            // <IRI> rdf:type ?var
            TriplePattern queryPattern = iri(IRI).isA(var);
            query.prefix(PREFIX_OM).select(var).where(queryPattern);
            kbClient.setQuery(query.getQueryString());
            try {
                JSONArray queryResult = kbClient.executeQuery();
                // if the query result is not empty and the rdf:type is equivalent to om:Measure
                if (!queryResult.isEmpty() && queryResult.getJSONObject(0).getString("var") == measure.toString()){
                LOGGER.info(IRI + " already has a rdf:type om:Measure!");
                } else {
                    //create triple pattern:
                    // <IRI> rdf:type om:Measure .
                    // <IRI> om:hasUnit <unitInstance> .
                    // <unitInstance> rdf:type <unitType> ;
                    //                rdf:type om:unit ;
                    //                rdfs:label "unitLabel" .
                    queryPattern = iri(IRI).isA(measure).andHas(label, measureLabel).andHas(hasUnit, unitInstance);
                    TriplePattern queryPattern2 = unitInstance.isA(unitType).andIsA(unit).andHas(label, unitLabel);
                    InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern, queryPattern2).prefix(PREFIX_OM, PREFIX_RDFS);
                    kbClient.executeUpdate(insertQuery.getQueryString());
                    LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.getQueryString());
                }
            } catch (Exception e) {
                throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString());
            }
        } else if  (measureLabel != null && unitInstance == null) {
            SelectQuery query = Queries.SELECT();
            Variable var = SparqlBuilder.var("var");
            //create triple pattern:
            // <IRI> rdf:type ?var
            TriplePattern queryPattern = iri(IRI).isA(var);
            query.prefix(PREFIX_OM).select(var).where(queryPattern);
            kbClient.setQuery(query.getQueryString());
            try {
                JSONArray queryResult = kbClient.executeQuery();
                // if the query result is not empty and the rdf:type is equivalent to om:Measure
                if (!queryResult.isEmpty() && queryResult.getJSONObject(0).getString("var") == measure.toString()){
                LOGGER.info(IRI + " already has a rdf:type om:Measure!");
                } else {
                    //create triple pattern:
                    // <IRI> rdf:type om:Measure .
                    queryPattern = iri(IRI).isA(measure).andHas(label, measureLabel);
                    InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern).prefix(PREFIX_OM, PREFIX_RDFS);
                    kbClient.executeUpdate(insertQuery.getQueryString());
                    LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.getQueryString());
                }
            } catch (Exception e) {
                throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString());
            }
        }
    }
    
    /**
     * Check for existence of quantity IRI linked to each data IRI and if they do not exist, create the IRIs and instantiate them
     * @param IRI the data IRI to check for
     * @param jsonKey a key used to identify what type of quantity does each data IRI represents
     * @return quantity IRI
     */
    private String instantiateQuantityIfNotExist(String IRI, String jsonKey) {
        final String quantityIRI;
        Variable quantity = SparqlBuilder.var("quantity");
        SelectQuery query = Queries.SELECT();
        //create triple pattern:
        // ?quantity om:hasValue <IRI> .
        TriplePattern queryPattern = quantity.has(hasValue, iri(IRI));
        query.prefix(PREFIX_OM).select(quantity).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient.executeQuery();
            if(!queryResult.isEmpty()){
                LOGGER.info(queryResult.getJSONObject(0).getString("quantity"));   
                quantityIRI = queryResult.getJSONObject(0).getString("quantity");
            } else {
                quantityIRI = ONTOEMS_NS + "Quantity_" + UUID.randomUUID();
                //create triple pattern:
                // <quantityIRI> om:hasValue <IRI> .
                queryPattern = iri(quantityIRI).has(hasValue, iri(IRI));
                InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern).prefix(PREFIX_OM);
                kbClient.executeUpdate(insertQuery.getQueryString());
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.getQueryString());

                Iri quantityType = null;
                //check jsonKey against om:Quantity subclasses
                //this is based on the variables retrievable via the API
                if (jsonKey.contains("solarRadiation")) {
                    quantityType = irradiance;
                } else if (jsonKey.contains("uv")) {
                    quantityType = uvIndex;
                } else if (jsonKey.contains("winddir")) {
                    quantityType = windDirection;
                } else if (jsonKey.contains("humidity")) {
                    quantityType = relativeHumidity;
                } else if (jsonKey.contains("temp")) {
                    quantityType = temperature;
                } else if (jsonKey.contains("windspeed")) {
                    quantityType = windSpeed;
                } else if (jsonKey.contains("windgust")) {
                    quantityType = windGust;
                } else if (jsonKey.contains("dewpt")) {
                    quantityType = dewPoint;
                } else if (jsonKey.contains("windchill")) {
                    quantityType = windChill;
                } else if (jsonKey.contains("heatindex")) {
                    quantityType = heatIndex;
                } else if (jsonKey.contains("pressure") && !jsonKey.contains("trend")) {
                    quantityType = pressure;
                } else if (jsonKey.contains("precipRate")) {
                    quantityType = precipitationRate;
                } else if (jsonKey.contains("precip") && !jsonKey.contains("Rate")) {
                    quantityType = rainfall;
                }

                if (quantityType != null) {
                //create triple pattern:
                // <quantityIRI> rdf:type <quantityType> .
                queryPattern = iri(quantityIRI).isA(quantityType);
                insertQuery = Queries.INSERT_DATA(queryPattern).prefix(PREFIX_OM, PREFIX_ONTOEMS);
                kbClient.executeUpdate(insertQuery.getQueryString());
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.getQueryString());
                }
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString());
        }
        instantiateAggregateFunctionIfNotExist(quantityIRI, jsonKey);
        return quantityIRI;
    }

    /**
     * check for aggregate function of each quantity (e.g. min, max, avg) and instantiate them
     * @param quantityIRI the quantity IRI to check for
     * @param jsonKey a key used to identify the type of aggregate function attached to each quantity
     */
    private void instantiateAggregateFunctionIfNotExist(String quantityIRI, String jsonKey) {
        Iri aggregateFunctionIRI = null;
        String queryString = null;
        String aggregateFunctionLabel = null;
        //check jsonKey against om:Quantity subclasses
        if (jsonKey.contains("High") | jsonKey.contains("Max")) {
            aggregateFunctionIRI = maximum;
            aggregateFunctionLabel = "maximum" ;
        } else if (jsonKey.contains("Low") | jsonKey.contains("Min")) {
            aggregateFunctionIRI = minimum;
            aggregateFunctionLabel = "minimum" ;
        } else if (jsonKey.contains("Avg")) {
            aggregateFunctionIRI = average;
            aggregateFunctionLabel = "average" ;
        } else if (jsonKey.contains("Total")) {
            aggregateFunctionIRI = sum;
            aggregateFunctionLabel = "sum" ;
        }
        if (aggregateFunctionIRI != null) {
            try {
                //create triple pattern:
                // <quantityIRI> om:hasAggregateFunction <aggregateFunctionIRI> .
                // <aggregateFunctionIRI> rdf:type om:Function .
                // <aggregateFunctionIRI> rdfs:label "aggregateFunctionLabel" .
                TriplePattern queryPattern = iri(quantityIRI).has(hasAggregateFunction, aggregateFunctionIRI);
                TriplePattern queryPattern2 = aggregateFunctionIRI.isA(function).andHas(label, aggregateFunctionLabel);
                InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern, queryPattern2).prefix(PREFIX_OM, PREFIX_RDFS);
                queryString = insertQuery.getQueryString();
                kbClient.executeUpdate(insertQuery.getQueryString());
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.getQueryString());
            } catch (Exception e) {
                throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryString, e);
            }
        }
    }

	/**
	 * Check for reporting station instance and instantiate it if it does not exist
	 * @param quantityIRIs a list of quantity IRIs that should be linked to the reporting station instance via ontoems:reports
	 * @return reporting station IRI
	 */
    private String instantiateReportingStationIfNotExist(List<String> quantityIRIs) {
        SelectQuery query = Queries.SELECT();
        Variable reportingStationVar = SparqlBuilder.var("reportingStation");
        //create triple pattern:
        // ?reportStation ontoems:reports <quantityIRI 01> ;
        //                ontoems:reports <quantityIRI 02> ;
        //                ontoems:reports <quantityIRI 03> ;
        //                            .
        //                            .
        //                            .
        TriplePattern queryPattern = reportingStationVar.has(reports,  iri(quantityIRIs.get(0)));
        for (int i = 1; i < quantityIRIs.size(); i++) {   
            String IRIString = quantityIRIs.get(i);
            queryPattern = queryPattern.andHas(reports, iri(IRIString));
        }
        query.prefix(PREFIX_ONTOEMS).select(reportingStationVar).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        String reportingStationIRI ;
        try {
            JSONArray queryResult = kbClient.executeQuery();
            if(!queryResult.isEmpty()){
                LOGGER.info("The reporting station already exist: " + queryResult.getJSONObject(0).getString("reportingStation"));
                reportingStationIRI = queryResult.getJSONObject(0).getString("reportingStation");
            } else {
                reportingStationIRI = ONTOEMS_NS + "reportingStation_" + UUID.randomUUID();
                //create triple pattern:
                // <reportingStationIRI> rdf:type ontoems:ReportingStation ;
                //                       ontoems:reports <quantityIRI 01> ;
                //                       ontoems:reports <quantityIRI 02> ;
                //                            .
                //                            .
                queryPattern = iri(reportingStationIRI).isA(reportingStation).andHas(reports, iri(quantityIRIs.get(0))).andHas(label, "Weather Station " + stationId);
                for (int i = 1; i < quantityIRIs.size(); i++) {
                    String IRIString = quantityIRIs.get(i);
                    queryPattern = queryPattern.andHas(reports, iri(IRIString));
                }
                InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern).prefix(PREFIX_ONTOEMS, PREFIX_RDFS);
                kbClient.executeUpdate(insertQuery.getQueryString());
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.getQueryString());   
            }
            return reportingStationIRI;
        } catch (Exception e){
            throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString(), e);
        }
    }

    /**
     * Instantiate the latitude and longitude of the weather station if it does not exist
     * @param reportingStationIRI the ReportingStation IRI to check for
     * @param weatherReadings weather readings retrieved via API
     * @throws IOException
     */
    private void instantiateGeoSpatialInfoIfNotExist(String reportingStationIRI, JSONObject weatherReadings) {
        //retrieve long lat from JSONObject weatherReadings
        Double longitude;
        Double latitude;
        try {
            longitude = Double.parseDouble(weatherReadings.getJSONArray("observations").getJSONObject(weatherReadings.getJSONArray("observations").length() - 1).get("lon").toString());
            latitude = Double.parseDouble(weatherReadings.getJSONArray("observations").getJSONObject(weatherReadings.getJSONArray("observations").length() - 1).get("lat").toString());

		} catch (Exception e) {
			LOGGER.error(RETRIEVE_LATLONG_ERROR_MSG, e);
			throw new JPSRuntimeException(e);
		}
        //postgisClient is used to interact with the stack's database that will store the geolocation information
        postgisClient = new WeatherPostGISClient();

        try (Connection conn = postgisClient.getConnection()) {
            JSONObject response = new JSONObject();
            WeatherGeospatialClient geospatialClient = new WeatherGeospatialClient();
			if (!postgisClient.checkTableExists(CARESWeatherStationInputAgentLauncher.LAYERNAME, conn)) {
                // add ontop mapping file
			    Path obda_file = new ClassPathResource("ontop.obda").getFile().toPath();
                OntopClient ontopClient = OntopClient.getInstance();
			    ontopClient.updateOBDA(obda_file);
                
                geospatialClient.createGeospatialInformation(latitude, longitude, "Weather Station " + stationId, reportingStationIRI);
				response.put("message", "Geospatial information instantiated for the following: Weather Station " + stationId);
			} else {
				// table exists, check table contents for an equivalent point
				if (!postgisClient.checkPointExists(latitude, longitude, conn)) {
                    geospatialClient.createGeospatialInformation(latitude, longitude, "Weather Station " + stationId, reportingStationIRI);
					response.put("message", "Geospatial information instantiated for the following: Weather Station " + stationId);
				} else {
					response.put("message", "There is already a station at the given coordinates");
				}
			}
		} catch (Exception e) {
			LOGGER.error(CREATEGEOSPATIAL_ERROR_MSG, e);
            throw new JPSRuntimeException(CREATEGEOSPATIAL_ERROR_MSG);
		}
    }

    /**
     * load cofigs for the remot store client
     * @param filepath filepath of client.properties file
     * @throws IOException
     */
    private void loadClientConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }

        // Try-with-resource to ensure closure of input stream
        try (InputStream input = new FileInputStream(file)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);

            if (prop.containsKey("sparql.query.endpoint")) {
                this.sparqlQueryEndpoint = prop.getProperty("sparql.query.endpoint");
            } else {
                throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"");
            }
            if (prop.containsKey("sparql.update.endpoint")) {
                this.sparqlUpdateEndpoint = prop.getProperty("sparql.update.endpoint");
            } else {
                throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"");
            }
            if (prop.containsKey("sparql.username")) {
                this.sparqlUser = prop.getProperty("sparql.username");
            }
            if (prop.containsKey("sparql.password")) {
                this.sparqlPassword = prop.getProperty("sparql.password");
            }
            if (prop.containsKey("db.url")) {
                dbUrl = prop.getProperty("db.url");
            } else {
                throw new IOException("Properties file is missing \"db.url=<db_url>\"");
            }
            if (prop.containsKey("db.user")) {
                dbUser = prop.getProperty("db.user");
            } else {
                throw new IOException("Properties file is missing \"db.user=<db_user>\"");
            }
            if (prop.containsKey("db.password")) {
                dbPassword = prop.getProperty("db.password");
            } else {
                throw new IOException("Properties file is missing \"db.password=<db_password>\"");
            }

            kbClient = new RemoteStoreClient(sparqlQueryEndpoint, sparqlUpdateEndpoint, sparqlUser, sparqlPassword);
        }
    }

    /**
     * Reads the stationId from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the stationId
     */
    private void loadAPIconfigs(String filepath) throws IOException{
        File file=new File(filepath);
        if(!file.exists()){
            throw new FileNotFoundException("There was no properties file found in the specified path: "+filepath);
        }
        try(InputStream input= new FileInputStream(file)){
            Properties prop=new Properties();
            prop.load(input);
            if (prop.containsKey("weather.stationId")){
                this.stationId=prop.getProperty("weather.stationId");
            }else{
                throw new IOException("The properties file is missing \"weather.stationId=<stationId>\"");
            }
        }
    }

}

