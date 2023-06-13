package uk.ac.cam.cares.jps.agent.smartmeteragent;

import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

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

@WebServlet(urlPatterns = { "/upload" })
public class SmartMeterAgentLauncher extends JPSAgent {

    private static final Logger LOGGER = LogManager.getLogger(SmartMeterAgentLauncher.class);

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

        // Necessary inputs:
        // Data source: database "database" or csv file "csv"
        String dataSource = requestParams.getString("dataSource");
        // Data required: only latest data "latest" or all historical data "historical"
        String dataRequired = requestParams.getString("dataRequired");
        // Microgrid: targetResourceID of the microgrid blazegraph for Access Agent to query
        String targetResourceID = requestParams.getString("microgrid");

        String currentTime = getCurrentDateTime();

        // Optional inputs:
        // Parameters for historical data reading
        // Data before (inclusive): UTC date time in "yyyy-MM-dd HH:mm" format 
        // (optional, use current UTC time if not given)
        String dataBefore;
        String dataAfter;
        if (requestParams.has("dataBefore")) {
            dataBefore = requestParams.getString("dataBefore");
        } else {
            LOGGER.info("dataBefore not given, using current datetime instead...");
            dataBefore = currentTime;
        }
        // Data after (inclusive): UTC date time in "yyyy-MM-dd HH:mm" format 
        // (optional, use default time if not given)
        if (requestParams.has("dataAfter")) {
            dataAfter = requestParams.getString("dataAfter");
        } else {
            LOGGER.info("dataAfter not given, using default value: 2000-01-01 00:00 ...");
            dataAfter = "2000-01-01 00:00";
        }

        SmartMeterAgent agent = getSmartMeterAgent();
        List<String[]> mappings = getDataMappings(agent);

        if (dataSource.equalsIgnoreCase("database") && dataRequired.equalsIgnoreCase("latest")) {
            // read latest valid data from database, update every minute
            JSONArray previousResult = new JSONArray();
            while (true) {
                LOGGER.info("Reading latest data from database...");
                mappings = getDataMappings(agent);
                JSONArray result = agent.queryLatestDataFromDb(currentTime, mappings);
                if (!result.similar(previousResult)) {
                    LOGGER.info("Uploading data...");
                    JSONArray dataIRIArray = agent.getDataIris(targetResourceID, mappings);
                    agent.uploadSmartMeterData(result, dataIRIArray);
                    previousResult = result;
                    LOGGER.info("Upload complete. Sleeping...");
                } else {
                    LOGGER.info("No new valid readings found, next attempt will be 1 minute later...");
                }
                try {
                    TimeUnit.SECONDS.sleep(60);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                }
            }
        } else if (dataSource.equalsIgnoreCase("database") && dataRequired.equalsIgnoreCase("historical")) {
            // read all valid historical readings from database
            LOGGER.info("Reading historical data from database...");
            int numOfReadings = agent.uploadHistoricalDataFromDb(dataBefore, dataAfter, mappings, targetResourceID);
            if (numOfReadings == 0) {
                LOGGER.info("No valid reading found in this period of time.");
                return new JSONObject().put("uploadStatus", "No valid reading found.");
            } else {
                LOGGER.info(numOfReadings + " readings uploaded.");
                return new JSONObject().put("uploadStatus", numOfReadings + " readings uploaded successfully.");
            }
        } else if (dataSource.equalsIgnoreCase("csv") && dataRequired.equalsIgnoreCase("historical")) {
            // read all valid data from csv file
            LOGGER.info("Reading historical data from csv file...");
            // Input time need to be UTC time
            OffsetDateTime beforeTime;
            OffsetDateTime afterTime;
            try {
                beforeTime = OffsetDateTime.parse(dataBefore.replace(" ", "T") + ":59+00:00");
                afterTime = OffsetDateTime.parse(dataAfter.replace(" ", "T") + ":00+00:00");
            } catch (DateTimeParseException e) {
                throw new JPSRuntimeException("Incorrect time format, input time should be yyyy-MM-dd HH:mm .", e);
            }
            
            List<String> devices = new ArrayList<>();
            for (int i = 0; i < mappings.size(); i++) {
                if (mappings.get(i).length > 1) {
                    devices.add(mappings.get(i)[1]);
                }
            }
            String filename = getCsvFilename();
            JSONArray dataIRIArray = agent.getDataIris(targetResourceID, mappings);

            int numOfReadings = agent.readDataFromCsvFile(filename, devices, beforeTime, afterTime, dataIRIArray);
            if (numOfReadings == 0) {
                LOGGER.info("No valid reading found in this period of time.");
                return new JSONObject().put("uploadStatus", "No valid reading found.");
            } else {
                LOGGER.info(numOfReadings + " readings uploaded.");
                return new JSONObject().put("uploadStatus", numOfReadings + " readings uploaded successfully.");
            }
        } else if (dataSource.equalsIgnoreCase("csv") && dataRequired.equalsIgnoreCase("latest")) {
            throw new JPSRuntimeException("dataRequired should be historical if reading from csv files.");
        } else {
            throw new JPSRuntimeException("Invalid data source or data required.");
        }
    }

    // For easier testing
    public SmartMeterAgent getSmartMeterAgent() {
        return new SmartMeterAgent();
    }

    public List<String[]> getDataMappings(SmartMeterAgent agent) {
        return agent.getDataMappings(AgentLocator.getCurrentJpsAppDirectory(this) + "/config");
    }

    public String getCurrentDateTime() {
        OffsetDateTime now = OffsetDateTime.now(ZoneOffset.UTC);
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
        return now.format(formatter);
    }

    public String getCsvFilename() {
        return AgentLocator.getCurrentJpsAppDirectory(this) + "/database/readings.csv";
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
            LOGGER.error("Input should contain data required: latest or historical .");
            return false;
        } else if (!requestParams.has("microgrid")) {
            LOGGER.error("Input should contain microgrid targetResourceID.");
            return false;
        }
        return true;
    }
}