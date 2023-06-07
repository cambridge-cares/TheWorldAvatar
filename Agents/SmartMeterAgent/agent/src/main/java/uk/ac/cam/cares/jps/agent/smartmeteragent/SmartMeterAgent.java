package uk.ac.cam.cares.jps.agent.smartmeteragent;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import com.opencsv.CSVReader;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.util.FileUtil;

public class SmartMeterAgent {

    private static final Logger LOGGER = LogManager.getLogger(SmartMeterAgent.class);
    protected RemoteStoreClient kbClient = new RemoteStoreClient();

    protected String dbUrl;
    protected String dbUsername;
    protected String dbPassword;
    protected String sparqlQueryEndpoint;
    protected String sparqlUpdateEndpoint;

    /**
     * Query for latest valid data from smart meter database.
     * (All devices in the mapping file should be switched on, and 
     * all data should be non-zero in order for the reading to be valid)
     * @param currentTime
     * @param mappings
     * @return
     */
    public JSONArray queryLatestDataFromDb(String currentTime, List<String[]> mappings) {
        // All devices that should have smart meter readings
        List<String> devices = new ArrayList<>();
        for (int i = 0; i < mappings.size(); i++) {
            if (mappings.get(i).length > 1) {
                devices.add(mappings.get(i)[1]);
            }
        }
        try {
            loadConfigs(AgentLocator.getCurrentJpsAppDirectory(this) + "/config/db.properties");
        } catch (IOException e) {
            throw new JPSRuntimeException("Failed to read smart meter RDB info from config file.");
        }
        try {
            Class.forName("org.sqlite.JDBC");
        } catch (ClassNotFoundException e) {
            throw new JPSRuntimeException("SQLite JDBC driver class not found.", e);
        }
        RemoteRDBStoreClient rdbClient = new RemoteRDBStoreClient(dbUrl, dbUsername, dbPassword);
        String query = getSqlQueryForData(devices, currentTime, "2000-01-01 00:00:00");
        LOGGER.info("Executing SQL query: " + query);
        JSONArray result = rdbClient.executeQuery(query);

        // when not all devices are switched on:
        // minus one minutes until valid results are found, maximum 10 attempts
        int numberOfAttempt = 0;
        while (!validateResult(devices, result)) {
            LOGGER.info("Not all devices are switched on, finding previous valid readings...");
            currentTime = OffsetDateTime.parse(currentTime.replace(" ", "T") + "+00:00")
                            .minusMinutes(1).toString().replace("T", " ").replace("Z", "");
            query = getSqlQueryForData(devices, currentTime, "2000-01-01 00:00:00");
            LOGGER.info("Executing SQL query: " + query);
            result = rdbClient.executeQuery(query);
            numberOfAttempt += 1;
            if (numberOfAttempt >= 10) {
                throw new JPSRuntimeException("Failed to find valid readings within the last 10 minutes. " 
                                        + "Please check that all devices in the mapping file are switched on.");
            }
        }
        return result;
    }

    /**
     * Retrieve historical valid readings from smart meter database,
     * and upload the data to instantiated timeseries.
     * (All devices in the mapping file should be switched on, and 
     * all data should be non-zero in order for the reading to be valid)
     * @param beforeTime
     * @param afterTime
     * @param mappings
     * @param targetResourceID
     * @return
     */
    public int uploadHistoricalDataFromDb(String beforeTime, String afterTime, List<String[]> mappings, String targetResourceID) {
        // All devices that should have smart meter readings
        List<String> devices = new ArrayList<>();
        for (int i = 0; i < mappings.size(); i++) {
            if (mappings.get(i).length > 1) {
                devices.add(mappings.get(i)[1]);
            }
        }
        try {
            loadConfigs(AgentLocator.getCurrentJpsAppDirectory(this) + "/config/db.properties");
        } catch (IOException e) {
            throw new JPSRuntimeException("Failed to read smart meter RDB info from config file.");
        }
        try {
            Class.forName("org.sqlite.JDBC");
        } catch (ClassNotFoundException e) {
            throw new JPSRuntimeException("SQLite JDBC driver class not found.", e);
        }

        // Select possible time points in this period
        String timeQuery = "SELECT DISTINCT strftime('%Y-%m-%d %H:%M:00', ts) as time "
                        + "FROM \"Measurement\" " 
                        + "WHERE time <= '" + beforeTime + "' " 
                        + "AND time >= '" + afterTime + "';";
                        
        RemoteRDBStoreClient rdbClient = new RemoteRDBStoreClient(dbUrl, dbUsername, dbPassword);
        LOGGER.info("Executing SQL query: " + timeQuery);
        JSONArray timeArray = rdbClient.executeQuery(timeQuery);        
        JSONArray result = new JSONArray();
        int numOfReadings = 0;
        JSONArray dataIRIArray = getDataIris(targetResourceID, mappings);
        for (int i = 0; i < timeArray.length(); i++) {
            JSONArray previousResult = result;
            String query = getSqlQueryForData(devices, timeArray.getJSONObject(i).optString("time"), afterTime);
            LOGGER.info("Executing SQL query: " + query);
            result = rdbClient.executeQuery(query);
            if (!result.similar(previousResult) && validateResult(devices, result)) {
                LOGGER.info("Valid readings found. Uploading data...");
                uploadSmartMeterData(result, targetResourceID, dataIRIArray);
                numOfReadings += 1;
            } else {
                LOGGER.info("No new valid readings, checking next time point...");
            }
        }
        return numOfReadings;
    }

    /**
     * Structure SQL query to retrieve smart meter readings.
     * (Average values are used instead of values in 3 phases)
     * (Assuming the update frequency of all smart meters is the same under normal conditions)
     * @param devices
     * @param beforeTime
     * @param afterTime
     * @return
     */
    public String getSqlQueryForData(List<String> devices, String beforeTime, String afterTime) {
        // Assuming time readings in smart meter database is UTC time
        String query = "SELECT strftime('%Y-%m-%dT%H:%M:00+00:00', MAX(ts)) as time, \"data_source\" as device, " 
        + "(\"ch1Watt\" + \"ch2Watt\" + \"ch3Watt\")/3 as pd, (\"ch1Current\" + \"ch2Current\" + \"ch3Current\")/3 as current, " 
        + "(\"ch1Voltage\" + \"ch2Voltage\" + \"ch3Voltage\")/3 as voltage, (\"ch1Hz\" + \"ch2Hz\" + \"ch3Hz\")/3 as frequency " 
        + "FROM \"Measurement\" " 
        // Make sure there is no data package lost (some values are 0)
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
        + "AND ts <= '" + beforeTime + "' "
        + "AND ts >= '" + afterTime + "' "
        + "GROUP BY device, id " 
        + "ORDER BY ts DESC "
        + "LIMIT " + devices.size() + ";";
        return query;
    }

    /**
     * Check if result readings are valid: 
     * all devices have readings and all selected readings have the same time.
     * @param devices devices required
     * @param result  query result from smart meter database
     * @return        whether the query result is valid
     */
    public boolean validateResult(List<String> devices, JSONArray result) {
        String time = "";
        boolean found = false;
        for (int i = 0; i < devices.size(); i++) {
            found = false;
            for (int j = 0; j < result.length(); j++) {
                if (result.getJSONObject(j).optString("device").equalsIgnoreCase(devices.get(i))) {
                    found = true;
                    if (i == 0) {
                        time = result.getJSONObject(j).optString("time");
                    } else if (!result.getJSONObject(j).optString("time").equals(time)) {
                        return false;
                    }
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }
        return true;
    }

    /**
     * Query for instantiated timeseries dataIRIs of bus values, 
     * map the IRIs to devices in the readings, and upload readings to KG.
     * @param queryResult       smart meter readings
     * @param targetResourceID
     * @param dataIRIArray
     */
    public void uploadSmartMeterData(JSONArray queryResult, String targetResourceID, JSONArray dataIRIArray) {
        OffsetDateTime time = OffsetDateTime.parse("2000-01-01T00:00:00+00:00");
        List<OffsetDateTime> timeValues = new ArrayList<OffsetDateTime>();
        List<List<?>> allValues = new ArrayList<>();
        List<String> dataIRIs = new ArrayList<>();

        for (int i = 0; i < dataIRIArray.length(); i++) {
            JSONObject oneBusIRIs = dataIRIArray.getJSONObject(i);
            String device = oneBusIRIs.optString("device");
            // when no smart meter is attached to the bus, assume the values are 0
            if (device.equals("0")) {
                if (oneBusIRIs.has("PdIri") && oneBusIRIs.has("QdIri")) {
                    dataIRIs.add(oneBusIRIs.optString("PdIri"));
                    dataIRIs.add(oneBusIRIs.optString("QdIri"));
                    allValues.add(Arrays.asList(new String[]{"0"})); // Pd
                    allValues.add(Arrays.asList(new String[]{"0"})); // Qd
                }
                if (oneBusIRIs.has("currentIri")) {
                    allValues.add(Arrays.asList(new String[]{"0"}));
                    dataIRIs.add(oneBusIRIs.optString("currentIri"));
                }
                if (oneBusIRIs.has("voltageIri")) {
                    allValues.add(Arrays.asList(new String[]{"0"}));
                    dataIRIs.add(oneBusIRIs.optString("voltageIri"));
                }
                if (oneBusIRIs.has("frequencyIri")) {
                    allValues.add(Arrays.asList(new String[]{"0"}));
                    dataIRIs.add(oneBusIRIs.optString("frequencyIri"));
                }
                continue;
            }

            boolean hasReadings = false;
            // when the bus has smart meter readings
            for (int j = 0; j < queryResult.length() ; j++) {
                time = OffsetDateTime.parse(queryResult.getJSONObject(j).optString("time"));
                if (!queryResult.getJSONObject(j).optString("device").equalsIgnoreCase(device)) {
                    continue;
                }

                // Check if bus load time series are instantiated
                if (oneBusIRIs.has("PdIri") && oneBusIRIs.has("QdIri")) {
                    List<String> oneDevicePd = new ArrayList<String>();
                    // Change the sign of active load
                    oneDevicePd.add(Double.toString(-queryResult.getJSONObject(j).getDouble("pd")));
                    allValues.add(oneDevicePd);
                    dataIRIs.add(oneBusIRIs.optString("PdIri"));
                    List<String> oneDeviceQd = new ArrayList<String>();                            
                    oneDeviceQd.add("0"); // Reactive load
                    allValues.add(oneDeviceQd);
                    dataIRIs.add(oneBusIRIs.optString("QdIri"));
                } else {
                    throw new JPSRuntimeException("Bus load time series not initialized.");
                }

                // Current, Voltage and Frequency (optional) may not be instantiated
                if (oneBusIRIs.has("currentIri")) {
                    List<String> oneDeviceCurrent = new ArrayList<String>();
                    oneDeviceCurrent.add(Double.toString(queryResult.getJSONObject(j).getDouble("current")));
                    allValues.add(oneDeviceCurrent);
                    dataIRIs.add(oneBusIRIs.optString("currentIri"));
                }
                if (oneBusIRIs.has("voltageIri")) {
                    List<String> oneDeviceVoltage = new ArrayList<String>();
                    oneDeviceVoltage.add(Double.toString(queryResult.getJSONObject(j).getDouble("voltage")));
                    allValues.add(oneDeviceVoltage);
                    dataIRIs.add(oneBusIRIs.optString("voltageIri"));
                }
                if (oneBusIRIs.has("frequencyIri")) {
                    List<String> oneDeviceFrequency = new ArrayList<String>();
                    oneDeviceFrequency.add(Double.toString(queryResult.getJSONObject(j).getDouble("frequency")));
                    allValues.add(oneDeviceFrequency);
                    dataIRIs.add(oneBusIRIs.optString("frequencyIri"));
                }
                hasReadings = true;
                break;
            }

            if (!hasReadings) {
                throw new JPSRuntimeException("Bus mapping exist but smart meter reading is not available. "
                                                + "Delete mapping for device " + device + " and try again.");
            }
        }
        timeValues.add(time);
        uploadTimeSeriesData(timeValues, dataIRIs, allValues);
    }

    public int readDataFromCsvFile(String fileName, List<String> devices, OffsetDateTime beforeTime, 
                                    OffsetDateTime afterTime, JSONArray dataIRIArray) {
        LOGGER.info("Reading from csv file...");
        int numOfReadings = 0;
		try (CSVReader csvReader = new CSVReader(new FileReader(fileName));) {
			String[] values = null;
            String time = "";
            JSONArray records = new JSONArray();
			while ((values = csvReader.readNext()) != null) {
                // Assuming readings are in UTC time, check until minute only
                values[1] = values[1].replace(" ", "T").replace(values[1].split(":")[2], "00+00:00");
                if (values[1].equals("ts") || OffsetDateTime.parse(values[1]).compareTo(beforeTime) > 0
                    || OffsetDateTime.parse(values[1]).compareTo(afterTime) < 0) {
                    continue;  // skip headers and lines where time is not within the given period
                }
                if (!values[1].equals(time)) {
                    // when the reading's time is different from the previous reading
                    JSONArray oneGroup = new JSONArray();
                    // If there are previous records, save them first
                    if (!records.isEmpty()) {
                        // Select one record for each device, and throw readings for devices
                        // not in the mapping file and readings with data package lost (value = 0)
                        for (int i = 0; i < devices.size(); i++) {
                            for (int j = 0; j < records.length(); j++) {
                                if (records.getJSONObject(j).optString("device").equalsIgnoreCase(devices.get(i)) &&
                                    Double.parseDouble(records.getJSONObject(i).get("pd").toString()) != 0 &&
                                    Double.parseDouble(records.getJSONObject(i).get("current").toString()) != 0 &&
                                    Double.parseDouble(records.getJSONObject(i).get("voltage").toString()) != 0 &&
                                    Double.parseDouble(records.getJSONObject(i).get("frequency").toString()) != 0) {
                                    oneGroup.put(records.getJSONObject(j));
                                    break;
                                }
                            }
                        }
                        if (validateResult(devices, oneGroup)) {
                            uploadSmartMeterData(oneGroup, time, dataIRIArray);
                            numOfReadings += 1;
                        }
                    }
                    records = new JSONArray();
                    records.put(processCsvReadings(values));
                    time = values[1];
                } else {
                    // when the reading's time is the same as the previous reading
                    records.put(processCsvReadings(values));
                }
			}

            // Save the last group of results with the same time
            JSONArray oneGroup = new JSONArray();
            if (!records.isEmpty()) {
                // Select one record for each device, and throw readings for devices
                // not in the mapping file and readings with data package lost (value = 0)
                for (int i = 0; i < devices.size(); i++) {
                    for (int j = 0; j < records.length(); j++) {
                        if (records.getJSONObject(j).optString("device").equalsIgnoreCase(devices.get(i)) &&
                            Double.parseDouble(records.getJSONObject(i).get("pd").toString()) != 0 &&
                            Double.parseDouble(records.getJSONObject(i).get("current").toString()) != 0 &&
                            Double.parseDouble(records.getJSONObject(i).get("voltage").toString()) != 0 &&
                            Double.parseDouble(records.getJSONObject(i).get("frequency").toString()) != 0) {
                            oneGroup.put(records.getJSONObject(j));
                            break;
                        }
                    }
                }
                if (validateResult(devices, oneGroup)) {
                    uploadSmartMeterData(oneGroup, time, dataIRIArray);
                    numOfReadings += 1;
                }
            }
		} catch (IOException e) {
            throw new JPSRuntimeException("Failed to read from csv file.", e);
        }
		return numOfReadings;
	}
    
    /**
     * Given one line of readings from the csv file, calculate 
     * average values using readings of 3 phases.
     * @param reading
     * @return
     */
    public JSONObject processCsvReadings(String[] reading) {
        String time = reading[1];
        String device = reading[32];
        String pd = Double.toString((Double.parseDouble(reading[6]) 
                    + Double.parseDouble(reading[7]) + Double.parseDouble(reading[8]))/3);
        String current = Double.toString((Double.parseDouble(reading[11]) 
                    + Double.parseDouble(reading[12]) + Double.parseDouble(reading[13]))/3);
        String voltage = Double.toString((Double.parseDouble(reading[14]) 
                    + Double.parseDouble(reading[15]) + Double.parseDouble(reading[16]))/3);
        String frequency = Double.toString((Double.parseDouble(reading[23]) 
                    + Double.parseDouble(reading[24]) + Double.parseDouble(reading[25]))/3);
        JSONObject readingValues = new JSONObject()
                                .put("time", time)
                                .put("device", device)
                                .put("pd", pd)
                                .put("current", current)
                                .put("voltage", voltage)
                                .put("frequency", frequency);
        return readingValues;
    }

    /**
     * Query for dataIRIs of bus values
     * @param targetResourceID
     * @param mappings
     * @return
     */
    public JSONArray getDataIris(String targetResourceID, List<String[]> mappings) {
        // query the triple store for data IRIs
        LOGGER.info("Querying triple store for time series data IRIs...");
        JSONArray busArray = callAccessAgentToQuery(targetResourceID, busQuery);

        // Add smart meter device information to busArray
        for (int i = 0; i < busArray.length(); i++) {
            JSONObject bus = busArray.getJSONObject(i);
            String busNumber = bus.optString("BusNumbervalue");
            boolean hasMapping = false;
            for (int j = 0; j < mappings.size(); j++) {
                if (Double.parseDouble(mappings.get(j)[0]) == Double.parseDouble(busNumber)) {
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
                        + busNumber + " does not have mapping information.");
            }
        }
        return busArray;
    }

    /**
     * Read mappings between buses and devices from mapping file.
     * @return
     */
    public List<String[]> getDataMappings(String baseUrl) {
        String csvString = FileUtil.readFileLocally(baseUrl + "/mappings.csv");
        List<String[]> mappings = new ArrayList<String[]>(fromCsvToArray(csvString));
        return mappings;
    }

    /**
     * Read csv file to List of string arrays
     * @param s
     * @return
     */
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
        return AccessAgentCaller.queryStore(targetResourceID, sparqlQuery);
    }

    /**
     * Connect to timeseries rdb and upload timeseries data.
     * @param timeValues
     * @param dataIRIs
     * @param dataValues
     */
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
			throw new JPSRuntimeException("Failed to upload timeseries data.");
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

                                // dataIRI of solarPd, not used for now
                                + "?solarPV	j7:hasGeneratedPower	?solarPd ."
                                + "?solarPd	a	j7:GeneratedPower ."
                                + "?solarPd	om:hasValue	?solarPdIri ."  

                                + "}"

                                + "}";
}
