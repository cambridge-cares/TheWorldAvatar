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
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String OM_NS = "http://www.ontology-of-units-of-measure.org/resource/om-2/"; 

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

    //weather station ID
    private String stationId;

    /**
     * Standard constructor
     * @param agentPropertiesFile the filepath of the agent.properties file
     * @param clientPropertiesFile the filepath of the client.properties file
     */
    public SparqlHandler(String agentPropertiesFile, String clientPropertiesFile, String apiPropertiesFile) throws IOException {
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
            loadRSClientConfigs(clientPropertiesFile);
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
     * Check whether all data IRIs has a rdf:type om:Measure and if not, add the data to the remote store
     * @param IRI the data IRI to check for
     */
    private void instantiateMeasureIfNotExist(String IRI) {
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
            if(!queryResult.isEmpty() && queryResult.getJSONObject(0).getString("var") == measure.toString()){
                LOGGER.info(IRI + " already has a rdf:type om:Measure!");
            } else {
                //create triple pattern:
                // <IRI> rdf:type om:Measure .
                queryPattern = iri(IRI).isA(measure);
                InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern).prefix(PREFIX_OM);
                kbClient.executeUpdate(insertQuery.getQueryString());
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.toString());
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString());
        }
    }
    
    /**
     * Check for existence of quantity IRI linked to each data IRI and if they do not exist, create the IRIs and instantiate them
     * @param IRI the data IRI to check for
     * @param jsonKey a key used to identify what type of quantity does each data IRI represents
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
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.toString());

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
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.toString());
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
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.toString());
            } catch (Exception e) {
                throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryString, e);
            }
        }
    }

    /**
     * Check for reporting station instance and instantiate it if it does not exist
     * @param quantityIRIs a list of quantity IRIs that should be linked to the reporting station instance via ontoems:reports
     */
    private void InstantiateReportingStationIfNotExist(List<String> quantityIRIs) {
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
        try {
            JSONArray queryResult = kbClient.executeQuery();
            if(!queryResult.isEmpty()){
                LOGGER.info("The reporting station already exist: " + queryResult.getJSONObject(0).getString("reportingStation"));
            } else {
                String reportingStationIRI = ONTOEMS_NS + "reportingStation_" + UUID.randomUUID();
                //create triple pattern:
                // <reportingStationIRI> rdf:type ontoems:ReportingStation ;
                //                       ontoems:reports <quantityIRI 01> ;
                //                       ontoems:reports <quantityIRI 02> ;
                //                       ontoems:reports <quantityIRI 03> ;
                //                            .
                //                            .
                //                            .
                queryPattern = iri(reportingStationIRI).isA(reportingStation).andHas(reports, iri(quantityIRIs.get(0))).andHas(label, "Weather Station " + stationId);
                for (int i = 1; i < quantityIRIs.size(); i++) {
                    String IRIString = quantityIRIs.get(i);
                    queryPattern = queryPattern.andHas(reports, iri(IRIString));
                }
                InsertDataQuery insertQuery = Queries.INSERT_DATA(queryPattern).prefix(PREFIX_ONTOEMS, PREFIX_RDFS);
                kbClient.executeUpdate(insertQuery.getQueryString());
                LOGGER.info(UPDATE_SUCCESS_MSG + queryPattern.toString());   
            }
        } catch (Exception e){
            throw new JPSRuntimeException(UPDATEORQUERY_ERROR_MSG + queryPattern.getQueryString(), e);
        }
    }

    /**
     * load cofigs for the remot store client
     * @param filepath filepath of client.properties file
     * @throws IOException
     */
    private void loadRSClientConfigs(String filepath) throws IOException {
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

