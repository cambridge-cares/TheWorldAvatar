package uk.ac.cam.cares.jps.agent.smartmeteragent;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.util.FileUtil;

@WebServlet(urlPatterns = { "/upload" })
public class SmartMeterAgent extends JPSAgent {

    private static final Logger LOGGER = LogManager.getLogger(SmartMeterAgent.class);
    protected RemoteStoreClient kbClient = new RemoteStoreClient();

    protected String dbUrl;
    protected String dbUsername;
    protected String dbPassword;
    protected String sparqlQueryEndpoint;
    protected String sparqlUpdateEndpoint;

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        LOGGER.info("Request received.");

        if (!validateInput(requestParams)) {
            LOGGER.error("Invalid request.");
            throw new JPSRuntimeException("Invalid request.");
        }

        // Data source: database "database" or csv file "csv"
        String dataSource = requestParams.getString("dataSource");
        // Data required: only latest data "latest" or all historical data "historical"
        String dataRequired = requestParams.getString("dataRequired");
        // Microgrid: targetResourceID of the microgrid blazegraph for Access Agent to query
        String targetResourceID = requestParams.getString("microgrid");

        // Data before: date time in "yyyy-MM-dd HH:mm:ss" format (optional, use current UTC time if not given)
        String dataBefore;
        if (requestParams.has("dataBefore")) {
            dataBefore = requestParams.getString("dataBefore");
        } else {
            LOGGER.info("dataBefore not given, using current datetime instead...");
            OffsetDateTime now = OffsetDateTime.now(ZoneOffset.UTC);
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
            dataBefore = now.format(formatter);
        }
        // TODO add dataAfter for historical input

        if (dataSource.toLowerCase().equals("database")) {
            readDataFromDatabase(dataRequired, targetResourceID, dataBefore);
        } else if (dataSource.toLowerCase().equals("csv")) {
            List<String[]> mappings = getDataMappings();
            readDataFromCsvFile(mappings, dataRequired, dataBefore);
        } else {
            throw new JPSRuntimeException("Invalid data source: dataSource should be database or csv.\n");
        }

        LOGGER.info("Upload complete.");
        return new JSONObject().put("uploadStatus", "Data uploaded successfully!");
    }

    public void readDataFromDatabase(String dataRequired, String targetResourceID, String dataBefore) {
        LOGGER.info("Reading from database...");
        if (dataRequired.toLowerCase().equals("latest")) {
            List<String[]> mappings = getDataMappings();
            JSONArray result = queryLatestDataFromDb(dataBefore, mappings);
            uploadLatestSmartMeterData(result, targetResourceID, mappings);
            return;
        } else if (dataRequired.toLowerCase().equals("historical")) {
            // TODO read historical data
        } else {
            throw new JPSRuntimeException("Invalid data requirement: dataRequired should be latest or historical.\n");
        }
    }

    public JSONArray queryLatestDataFromDb(String beforeTime, List<String[]> mappings) {
        // All devices that may have smart meter readings
        List<String> devices = new ArrayList<>();
        for (int i = 0; i < mappings.size(); i++) {
            if (mappings.get(i).length > 1) {
                devices.add(mappings.get(i)[1]);
            }
        }

        // TODO check all devices are in selected readings
        // if not, it means that not all devices are switched on at this time point
        // find previous readings with all devices: 1/2/3 mins ago -> 1/2/3 hrs ago -> 1/2/3 days ago...

        // Assuming time readings in smart meter database is UTC time
        String query = "SELECT strftime('%Y-%m-%dT%H:%M:00+00:00', MAX(ts)) as time, \"data_source\" as device, " 
        + "(\"ch1Watt\" + \"ch2Watt\" + \"ch3Watt\")/3 as pd, (\"ch1Current\" + \"ch2Current\" + \"ch3Current\")/3 as current, " 
        + "(\"ch1Voltage\" + \"ch2Voltage\" + \"ch3Voltage\")/3 as voltage, (\"ch1Hz\" + \"ch2Hz\" + \"ch3Hz\")/3 as frequency " 
        + "FROM \"Measurement\" " 
        
        + "WHERE " 
        + "\"ch1Watt\" <> 0 AND \"ch2Watt\" <> 0 AND \"ch3Watt\" <> 0 AND " 
        + "\"ch1Current\" <> 0 AND \"ch2Current\" <> 0 AND \"ch3Current\" <> 0 AND " 
        + "\"ch1Voltage\" <> 0 AND \"ch2Voltage\" <> 0 AND \"ch3Voltage\" <> 0 AND " 
        + "\"ch1Hz\" <> 0 AND \"ch2Hz\" <> 0 AND \"ch3Hz\" <> 0 " 
        + "AND (";

        // Only select devices needed
        for (int i = 0; i < devices.size(); i++) {
            query += "\"data_source\" = '" + devices.get(i) + "'";
            if (i < devices.size() - 1) {
                query += " OR ";
            }
        }

        query += ") "
        + "AND ts < '" + beforeTime + "' "
        + "GROUP BY device, id " 
        + "ORDER BY ts DESC "
        + "LIMIT " + devices.size() + ";";

        try {
            loadConfigs(AgentLocator.getCurrentJpsAppDirectory(this) + "/config/db.properties");
        } catch (IOException e) {
            throw new JPSRuntimeException("Failed to read smart meter RDB info from config file.\n");
        }
        RemoteRDBStoreClient rdbClient = new RemoteRDBStoreClient(dbUrl, dbUsername, dbPassword);
        LOGGER.info("Executing SQL query: " + query);
        JSONArray result = rdbClient.executeQuery(query);
        return result;
    }

    public void uploadLatestSmartMeterData(JSONArray queryResult, String targetResourceID, List<String[]> mappings) {
        OffsetDateTime time = OffsetDateTime.parse("2000-01-01T00:00:00+00:00");
        List<OffsetDateTime> timeValues = new ArrayList<OffsetDateTime>();
        List<List<?>> allValues = new ArrayList<>();
        List<String> dataIRIs = new ArrayList<>();
        JSONArray dataIRIArray = getDataIris(targetResourceID, mappings);

        for (int i = 0; i < dataIRIArray.length(); i++) {
            JSONObject oneBusIRIs = dataIRIArray.getJSONObject(i);
            String device = oneBusIRIs.getString("device");
            // when no smart meter is attached to the bus, assume the values are 0
            if (device.equals("0")) {
                if (oneBusIRIs.has("PdIri") && oneBusIRIs.has("QdIri")) {
                    dataIRIs.add(oneBusIRIs.getString("PdIri"));
                    dataIRIs.add(oneBusIRIs.getString("QdIri"));
                    allValues.add(Arrays.asList(new String[]{"0"})); // Pd
                    allValues.add(Arrays.asList(new String[]{"0"})); // Qd
                }
                if (oneBusIRIs.has("currentIri")) {
                    allValues.add(Arrays.asList(new String[]{"0"}));
                    dataIRIs.add(oneBusIRIs.getString("currentIri"));
                }
                if (oneBusIRIs.has("voltageIri")) {
                    allValues.add(Arrays.asList(new String[]{"0"}));
                    dataIRIs.add(oneBusIRIs.getString("voltageIri"));
                }
                if (oneBusIRIs.has("frequencyIri")) {
                    allValues.add(Arrays.asList(new String[]{"0"}));
                    dataIRIs.add(oneBusIRIs.getString("frequencyIri"));
                }
                continue;
            }

            boolean hasReadings = false;
            // when the bus has smart meter readings
            for (int j = 0; j < queryResult.length() ; j++) {
                // TODO check if time is the same, if not, query for the next latest time 
                // (ts <= the lowest one in previous attempt)
                time = OffsetDateTime.parse(queryResult.getJSONObject(j).getString("time"));
                if (!queryResult.getJSONObject(j).getString("device").toLowerCase().equals(device.toLowerCase())) {
                    continue;
                }

                // Check if bus load time series are instantiated
                if (oneBusIRIs.has("PdIri") && oneBusIRIs.has("QdIri")) {
                    List<String> oneDevicePd = new ArrayList<String>();
                    // Change the sign of active load
                    oneDevicePd.add(Double.toString(-queryResult.getJSONObject(j).getDouble("pd")));
                    allValues.add(oneDevicePd);
                    dataIRIs.add(oneBusIRIs.getString("PdIri"));
                    List<String> oneDeviceQd = new ArrayList<String>();                            
                    oneDeviceQd.add("0"); // Reactive load
                    allValues.add(oneDeviceQd);
                    dataIRIs.add(oneBusIRIs.getString("QdIri"));
                } else {
                    throw new JPSRuntimeException("Bus load time series not initialized.\n");
                }

                // Current, Voltage and Frequency (optional) may not be instantiated
                if (oneBusIRIs.has("currentIri")) {
                    List<String> oneDeviceCurrent = new ArrayList<String>();
                    oneDeviceCurrent.add(Double.toString(queryResult.getJSONObject(j).getDouble("current")));
                    allValues.add(oneDeviceCurrent);
                    dataIRIs.add(oneBusIRIs.getString("currentIri"));
                }
                if (oneBusIRIs.has("voltageIri")) {
                    List<String> oneDeviceVoltage = new ArrayList<String>();
                    oneDeviceVoltage.add(Double.toString(queryResult.getJSONObject(j).getDouble("voltage")));
                    allValues.add(oneDeviceVoltage);
                    dataIRIs.add(oneBusIRIs.getString("voltageIri"));
                }
                if (oneBusIRIs.has("frequencyIri")) {
                    List<String> oneDeviceFrequency = new ArrayList<String>();
                    oneDeviceFrequency.add(Double.toString(queryResult.getJSONObject(j).getDouble("frequency")));
                    allValues.add(oneDeviceFrequency);
                    dataIRIs.add(oneBusIRIs.getString("frequencyIri"));
                }
                hasReadings = true;
                break;
            }

            if (!hasReadings) {
                throw new JPSRuntimeException("Bus mapping exist but smart meter reading is not available. "
                                                + "Delete mapping for device " + device + " and try again.\n");
            }
        }
        timeValues.add(time);
        uploadTimeSeriesData(timeValues, dataIRIs, allValues);
    }

    // TODO read from csv file
    public void readDataFromCsvFile(List<String[]> mappings, String dataRequired, String dataBefore) {
        LOGGER.info("Reading from csv file...");
        if (dataRequired.toLowerCase().equals("latest")) {

        } else if (dataRequired.toLowerCase().equals("historical")) {

        } else {
            throw new JPSRuntimeException("Invalid data requirement: dataRequired should be latest or historical.\n");
        }
    }

    public JSONArray getDataIris(String targetResourceID, List<String[]> mappings) {
        // query the triple store for data IRIs
        LOGGER.info("Querying triple store for time series data IRIs...");
        JSONArray busArray = callAccessAgentToQuery(targetResourceID, busQuery);

        // Add smart meter device information to busArray
        for (int i = 0; i < busArray.length(); i++) {
            JSONObject bus = busArray.getJSONObject(i);
            String busNumber = bus.getString("BusNumbervalue");
            boolean hasMapping = false;
            for (int j = 0; j < mappings.size(); j++) {
                if (mappings.get(j)[0].equals(busNumber)) {
                    if (mappings.get(j).length > 1) {
                        // when the bus has a smart meter device attached to it
                        bus.put("device", mappings.get(j)[1]);
                    } else {
                        // when the bus does not have a smart meter
                        bus.put("device", "0");
                    }
                    busArray.put(i, bus);
                    hasMapping = true;
                    mappings.remove(j);
                    break;
                }
            }
            if (!hasMapping) {
                throw new JPSRuntimeException("Mapping file not complete, bus " 
                        + busNumber + " does not have mapping information.\n");
            }
        }
        return busArray;
    }

    public List<String[]> getDataMappings() {
        String baseUrl = AgentLocator.getCurrentJpsAppDirectory(this) + "/config";
        String csvString = FileUtil.readFileLocally(baseUrl + "/mappings.csv");
        List<String[]> mappings = new ArrayList<String[]>(fromCsvToArray(csvString));
        return mappings;
    }

    public List<String[]> fromCsvToArray(String s) {
        List<String[]> result = new ArrayList<String[]>();		
        StringTokenizer tokenizer = new StringTokenizer(s, "\n");
        
        while (tokenizer.hasMoreTokens()) {
            String[] row = tokenizer.nextToken().split(",");
            result.add(row);
        }
        return result;
    }

    public JSONArray callAccessAgentToQuery(String targetResourceID, String sparqlQuery) {
        JSONArray queryResult = AccessAgentCaller.queryStore(targetResourceID, sparqlQuery);
        return queryResult;
    }

    public void uploadTimeSeriesData(List<OffsetDateTime> timeValues, List<String> dataIRIs, List<List<?>> dataValues) {
        try {
            loadConfigs(AgentLocator.getCurrentJpsAppDirectory(this) + "/config/client.properties");
        } catch (IOException e) {
            throw new JPSRuntimeException("Failed to read target RDB info from config file.");
        }
		RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(dbUrl, dbUsername, dbPassword);
		TimeSeriesClient<OffsetDateTime> tsClient = new TimeSeriesClient<>(kbClient ,OffsetDateTime.class);
        TimeSeries<OffsetDateTime> outputTimeSeries = new TimeSeries<OffsetDateTime>(timeValues, dataIRIs, dataValues);
		List<TimeSeries<OffsetDateTime>> timeSeriesList = new ArrayList<>();
        timeSeriesList.add(outputTimeSeries);
		try (Connection conn = rdbStoreClient.getConnection()) {
			tsClient.bulkaddTimeSeriesData(timeSeriesList, conn);
		} catch (Exception e) {
			throw new JPSRuntimeException("Failed to upload timeseries data.\n");
		}
    }

    /**
     * Reads the username, password, and sparql endpoint (if needed) from a properties file and saves it in fields.
     * @param filepath Path to the properties file from which to read the username, password, and sparql endpoints
     */
    public void loadConfigs(String filepath) throws IOException {
        // Check whether properties file exists at specified location
        File file = new File(filepath);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath);
        }
        
        try (InputStream input = new FileInputStream(file)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            if (prop.containsKey("db.url")) {
                this.dbUrl = prop.getProperty("db.url");
            } else {
                throw new IOException("Properties file is missing \"db.url=<db_url>\"");
            }
            if (prop.containsKey("db.user")) {
                this.dbUsername = prop.getProperty("db.user");
            } else {
                throw new IOException("Properties file is missing \"db.user=<db_user>\"");
            }
            if (prop.containsKey("db.password")) {
                this.dbPassword = prop.getProperty("db.password");
            } else {
                throw new IOException("Properties file is missing \"db.password=<db_password>\"");
            }
            if (filepath.contains("db")) {
                return;
            }
            if (prop.containsKey("sparql.query.endpoint")) {
                kbClient.setQueryEndpoint(prop.getProperty("sparql.query.endpoint"));
            } else {
                throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"");
            }
            if (prop.containsKey("sparql.update.endpoint")) {
                kbClient.setUpdateEndpoint(prop.getProperty("sparql.update.endpoint"));
            } else {
                throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"");
            }
        }
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException {
        if (requestParams.isEmpty()) {
            LOGGER.error("Empty request.");
            return false;
        } else if (!requestParams.has("dataSource")) {
            LOGGER.error("Input should contain data source: database or csv .");
            return false;
        } else if (!requestParams.has("dataRequired")) {
            LOGGER.error("Input should contain upload method: latest or historical .");
            return false;
        } else if (!requestParams.has("microgrid")) {
            LOGGER.error("Input should contain microgrid targetResourceID.");
            return false;
        }
        return true;
    }

    // SPARQL query for bus number and time series data IRIs
    protected String busQuery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
                                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                                + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
                                + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
                                + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
                                + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontopowsys/OntoPowSys.owl#> "
                                + "PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/> "
                                + "SELECT ?BusNumbervalue ?PdIri ?QdIri ?currentIri ?voltageIri ?frequencyIri ?solarPdIri "

                                + "WHERE {?entity  a  j1:BusNode  ." 
                                + "?entity   j2:isModeledBy ?model ."
                                + "?model   j5:hasModelVariable ?num ." 
                                + "?num  a  j3:BusNumber  ." 
                                + "?num  j2:hasValue ?vnum ."
                                + "?vnum   j2:numericalValue ?BusNumbervalue ." 

                                // dataIRI of Pd
                                + "?entity  j6:hasActivePowerAbsorbed ?Pd ."
                                + "?Pd  a  j6:AbsorbedActivePower ." 
                                + "?Pd  om:hasValue ?PdIri ."  

                                // dataIRI of Qd
                                + "?entity  j6:hasReactivePowerAbsorbed ?Qd ."
                                + "?Qd  a  j6:AbsorbedReactivePower ."
                                + "?Qd  om:hasValue ?QdIri ."  

                                // dataIRI of current (from smart meter reading)
                                + "OPTIONAL {"
                                + "?entity  j6:hasCurrent ?current ."
                                + "?current  a  j6:Current ."
                                + "?current  om:hasValue ?currentIri ."  

                                // dataIRI of voltage (from smart meter reading)
                                + "?entity  j6:hasActualVoltage ?voltage ."
                                + "?voltage  a  j6:Voltage ."
                                + "?voltage  om:hasValue ?voltageIri ."  

                                // dataIRI of frequency (from smart meter reading)
                                + "?entity  j6:hasFrequency ?frequency ."
                                + "?frequency  a  j6:Frequency ."
                                + "?frequency  om:hasValue ?frequencyIri ."

                                + "?building	j7:hasBusNode	?entity ."
                                + "?building	a	j1:Building ."

                                + "?building	j2:contains	?solarPV ."
                                + "?solarPV	a	j1:PhotovoltaicPanel ."

                                // dataIRI of solarPd
                                + "?solarPV	j7:hasGeneratedPower	?solarPd ."
                                + "?solarPd	a	j7:GeneratedPower ."
                                + "?solarPd	om:hasValue	?solarPdIri ."  

                                + "}"

                                + "}";
}
