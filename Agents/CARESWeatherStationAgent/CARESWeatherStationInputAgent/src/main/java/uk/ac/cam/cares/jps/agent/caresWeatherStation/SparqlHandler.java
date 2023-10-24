package uk.ac.cam.cares.jps.agent.caresWeatherStation;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
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

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;


/**
 * Class to construct queries for SPARQL stores
 * @author Wilson */
public class SparqlHandler {
	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(CARESWeatherStationInputAgentLauncher.class);

    /**
     * Log messages
     */
    private static final String UPDATEORQUERY_ERROR_MSG = "Unable to run the following Update/Query the following: ";
    private static final String UPDATE_SUCCESS_MSG = "Successfully inserted the following triples: ";

    /**
     * Namespaces for ontologies
     */
    public static final String ONTOEMS_NS = "https://www.theworldavatar.com/kg/ontoems/";
	public static final String ONTOBIM_NS = "https://www.theworldavatar.com/kg/ontobim/";
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String BOT_NS = "https://w3id.org/bot#";
    public static final String OM_NS = "http://www.ontology-of-units-of-measure.org/resource/om-2/"; 

	/**
     * Prefixes
     */ 
	private static final Prefix PREFIX_ONTOEMS = SparqlBuilder.prefix("ontoems", iri(ONTOEMS_NS));
	private static final Prefix PREFIX_ONTOBIM = SparqlBuilder.prefix("ontobim", iri(ONTOBIM_NS));
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));
    private static final Prefix PREFIX_BOT = SparqlBuilder.prefix("saref", iri(BOT_NS));
    private static final Prefix PREFIX_OM = SparqlBuilder.prefix("om", iri(OM_NS));

    /*
     * stationID = label
     * tz= convert to appropriate timestamp format
     * obsTimeUtc= ignore
     * obsTimeLocal= ignore
     * epoch= ignore
     * lat= ignore
     * lon= ignore
     * solarRadiationHigh= om:Irradiance
     * uvHigh= 	<https://www.theworldavatar.com/kg/ontoems/UVIndex>
winddirAvg= WindDirection
humidityHigh= om:RelativeHumidity
humidityLow= om:RelativeHumidity
humidityAvg= om:RelativeHumidity
qcStatus= ignore
tempHigh= om:Temperature
tempLow= om:Temperature
tempAvg= om:Temperature
windspeedHigh= windspeed
windspeedLow= windspeed
windspeedAvg= windspeed
windgustHigh= windgust
windgustLow= windgust
windgustAvg= windgust
dewptHigh= dew point
dewptLow= dew point
dewptAvg= dew point
windchillHigh= wind chill (add to ontoems) subclass of <https://www.theworldavatar.com/kg/ontoems/FeelsLikeTemperature>
windchillLow= wind chill (add to ontoems)
windchillAvg= wind chill (add to ontoems)
heatindexHigh= heat index (add to ontoems) subclass of <https://www.theworldavatar.com/kg/ontoems/FeelsLikeTemperature>
heatindexLow= heat index (add to ontoems)
heatindexAvg= heat index (add to ontoems)
pressureMax= Pressure
pressureMin= Pressure
pressureTrend= Pressure (ignore for now)
precipRate= precipitation rate (add to ontoems)
precipTotal= Rainfall with sum function
     */

	/**
     * Relationships
     */ 
    private static final Iri label = PREFIX_RDFS.iri("label");
    private static final Iri reports = PREFIX_ONTOEMS.iri("reports");
    private static final Iri hasValue = PREFIX_OM.iri("hasValue");
    private static final Iri hasAggregateFunction = PREFIX_OM.iri("hasAggregateFunction");

    /**
     * Classes
     */
    private static final Iri reportingStation = PREFIX_ONTOEMS.iri("ReportingStation");
    private static final Iri uvIndex = PREFIX_ONTOEMS.iri("UVIndex");
    private static final Iri windDirection = PREFIX_ONTOEMS.iri("WindDirection");
    private static final Iri windSpeed = PREFIX_ONTOEMS.iri("WindSpeed");
    private static final Iri windGust = PREFIX_ONTOEMS.iri("WindGust");
    private static final Iri dewPoint = PREFIX_ONTOEMS.iri("DewPoint");
    private static final Iri windChill = PREFIX_ONTOEMS.iri("WindChill");
    private static final Iri heatIndex = PREFIX_ONTOEMS.iri("HeatIndex");
    private static final Iri precipitationRate = PREFIX_ONTOEMS.iri("PrecipitationRate");
    private static final Iri rainfall = PREFIX_ONTOEMS.iri("Rainfall");

    private static final Iri relativeHumidity = PREFIX_OM.iri("RelativeHumidity");
    private static final Iri temperature = PREFIX_OM.iri("Temperature");  
    private static final Iri pressure = PREFIX_OM.iri("Pressure");
    private static final Iri irradiance = PREFIX_OM.iri("Irradiance");
    private static final Iri maximum = PREFIX_OM.iri("maximum");
    private static final Iri minimum = PREFIX_OM.iri("minimum");
    private static final Iri average = PREFIX_OM.iri("average");
    private static final Iri sum = PREFIX_OM.iri("sum");
    private static final Iri measure = PREFIX_OM.iri("Measure");
    private static final Iri function = PREFIX_OM.iri("Function");

    //client to interact with remote store
    RemoteStoreClient kbClient;

    //list to contain mappings
    private List<JSONKeyToIRIMapper> mappings;

    //endpoints and credentials
    String sparqlQueryEndpoint;
    String sparqlUpdateEndpoint;
    String bgUsername;
    String bgPassword;

    /**
     * Standard constructor
     * 
     */
    public SparqlHandler(String agentPropertiesFile, String clientPropertiesFile) throws IOException {
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
            loadTSClientConfigs(clientPropertiesFile);
        }
            catch (Exception e) {
                throw new JPSRuntimeException ("Unable to load properties from the timeseries config file!");
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
     * Query for all instances with rdf:type ontobms:FumeHood and ontobms:WalkInFumeHood
     */
    public void instantiateIfNotExist() {
       List<String> quantityIRIs;
        // Iterate through all mappings (each represents one time series)
        for (JSONKeyToIRIMapper mapping: mappings) {
            //list to contain the quantity IRIs
            quantityIRIs = new ArrayList<>();
            // The IRIs used by the current mapping
            List<String> iris = mapping.getAllIRIs();
            for (int i = 0; i < iris.size(); i++) {
                instantiateMeasureIfNotExist(iris.get(i));
                String quantityIRI = instantiateQuantityIfNotExist(iris.get(i), mapping.getJSONKey(iris.get(i)));
                quantityIRIs.add(quantityIRI);
            }
            InstantiateReportingStationIfNotExist(quantityIRIs);
        }
        
    }

    /**
     * Query for all instances with rdf:type ontobms:FumeHood and ontobms:WalkInFumeHood
     */
    private void instantiateMeasureIfNotExist(String IRI) {
        
        SelectQuery query = Queries.SELECT();
        Variable var = SparqlBuilder.var("var");
        //create triple pattern
        TriplePattern queryPattern = iri(IRI).isA(var);
        query.prefix(PREFIX_OM).select(var).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient.executeQuery();
            if(!queryResult.isEmpty() && queryResult.getJSONObject(0).getString("var") == measure.toString()){
                LOGGER.info(IRI + " already has a rdf:type om:Measure!");
            } else {
                queryPattern = iri(IRI).isA(measure);
                InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern).prefix(PREFIX_OM);
                kbClient.executeUpdate(insertQuery.getQueryString());
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.toString());
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString());
        }
    }
    
    private String instantiateQuantityIfNotExist(String IRI, String jsonKey) {
        final String quantityIRI;
        Variable quantity = SparqlBuilder.var("quantity");
        SelectQuery query = Queries.SELECT();
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
                queryPattern = iri(quantityIRI).has(hasValue, iri(IRI));
                InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern).prefix(PREFIX_OM);
                kbClient.executeUpdate(insertQuery.getQueryString());
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.toString());

                //default will be om:Quantity
                Iri quantityType = null;
                //check jsonKey against om:Quantity subclasses
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
                //check with MH how to represent pressure trend

                if (quantityType != null) {
                queryPattern = iri(quantityIRI).isA(quantityType);
                insertQuery = Queries.INSERT_DATA(queryPattern).prefix(PREFIX_OM, PREFIX_ONTOEMS);
                kbClient.executeUpdate(insertQuery.getQueryString());
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.toString());
                }
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString());
        }
        instantiateAggregateFunctionIfNotExist(quantityIRI, jsonKey);
        return quantityIRI;
    }

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
                TriplePattern queryPattern = iri(quantityIRI).has(hasAggregateFunction, aggregateFunctionIRI);
                TriplePattern queryPattern2 = aggregateFunctionIRI.isA(function).andHas(label, aggregateFunctionLabel);
                InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern, queryPattern2).prefix(PREFIX_OM, PREFIX_RDFS);
                queryString = insertQuery.getQueryString();
                kbClient.executeUpdate(insertQuery.getQueryString());
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.toString());
            } catch (Exception e) {
                throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryString, e);
            }
        }
    }

    private void InstantiateReportingStationIfNotExist(List<String> quantityIRIs) {
        SelectQuery query = Queries.SELECT();
        Variable reportingStationVar = SparqlBuilder.var("reportingStation");
        TriplePattern queryPattern = reportingStationVar.has(reports,  iri(quantityIRIs.get(0)));
        for (int i = 1; i < quantityIRIs.size(); i++) {   
            String IRIString = quantityIRIs.get(i);
            queryPattern = queryPattern.andHas(reports, iri(IRIString));
        }
            query.prefix(PREFIX_ONTOEMS).select(reportingStationVar).where(queryPattern);
            kbClient.setQuery(query.getQueryString());
            try {
                JSONArray queryResult = kbClient.executeQuery();
                if(!queryResult.isEmpty()){
                    //if one of the quantity IRI is already linked to the reporting station via ontoems:reports
                    //then all of the quantity IRIs should be linked as well
                    LOGGER.info("The reporting station already exist: " + queryResult.getJSONObject(0).getString("reportingStation"));
                } else {
                    String reportingStationIRI = ONTOEMS_NS + "reportingStation_" + UUID.randomUUID();
                    queryPattern = iri(reportingStationIRI).isA(reportingStation).andHas(reports, iri(quantityIRIs.get(0)));
                    for (int i = 1; i < quantityIRIs.size(); i++) {   
                        String IRIString = quantityIRIs.get(i);
                        queryPattern = queryPattern.andHas(reports, iri(IRIString));
                    }
                    InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern).prefix(PREFIX_ONTOEMS);
                    kbClient.executeUpdate(insertQuery.getQueryString());
                    LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.toString());
                    
                }
            } catch (Exception e){
                throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString(), e);
            }
        }

    private void loadTSClientConfigs(String filepath) throws IOException {
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
            if (prop.containsKey("bg.username")) {
                this.bgUsername = prop.getProperty("bg.username");
            }
            if (prop.containsKey("bg.password")) {
                this.bgPassword = prop.getProperty("bg.password");
            }

            kbClient = new RemoteStoreClient();
            kbClient.setUpdateEndpoint(sparqlUpdateEndpoint);
            kbClient.setQueryEndpoint(sparqlQueryEndpoint);
            kbClient.setUser(bgUsername);
            kbClient.setPassword(bgPassword);
        }
    }

}

