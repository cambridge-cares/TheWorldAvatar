package uk.ac.cam.cares.jps.agent.smartmeteragent;

import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
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

        // Data source: database "database" or csv file "csv"
        String dataSource = requestParams.getString("dataSource");
        // Data required: only latest data "latest" or all historical data "historical"
        String dataRequired = requestParams.getString("dataRequired");
        // Microgrid: targetResourceID of the microgrid blazegraph for Access Agent to query
        String targetResourceID = requestParams.getString("microgrid");

        OffsetDateTime now = OffsetDateTime.now(ZoneOffset.UTC);
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        String currentTime = now.format(formatter);

        // Data before: date time in "yyyy-MM-dd HH:mm:ss" format (optional, use current UTC time if not given)
        // for historical data reading
        String dataBefore;
        if (requestParams.has("dataBefore")) {
            dataBefore = requestParams.getString("dataBefore");
        } else {
            LOGGER.info("dataBefore not given, using current datetime instead...");
            dataBefore = currentTime;
        }

        SmartMeterAgent agent = new SmartMeterAgent();
        if (dataSource.toLowerCase().equals("database") && dataRequired.toLowerCase().equals("latest")) {
            while (true) {
                LOGGER.info("Reading latest data from database...");
                List<String[]> mappings = agent.getDataMappings();
                JSONArray result = agent.queryLatestDataFromDb(currentTime, mappings);
                LOGGER.info("Uploading data...");
                agent.uploadLatestSmartMeterData(result, targetResourceID, mappings);
                LOGGER.info("Upload complete. Sleeping...");
                try {
                    TimeUnit.SECONDS.sleep(60);
                } catch (InterruptedException ie) {
                    break;
                }
            }
        } else if (dataSource.toLowerCase().equals("csv") && dataRequired.toLowerCase().equals("latest")) {
            throw new JPSRuntimeException("dataRequired should be historical if reading from csv files.");
        }else if (dataSource.toLowerCase().equals("database") && dataRequired.toLowerCase().equals("historical")) {
            // TODO historical readings from db
        } else if (dataSource.toLowerCase().equals("csv") && dataRequired.toLowerCase().equals("historical")) {
            // TODO historical readings from csv
        } else {
            throw new JPSRuntimeException("Invalid data source or data required.");
        }

        LOGGER.info("Upload complete.");
        return new JSONObject().put("uploadStatus", "Data uploaded successfully!");
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
}