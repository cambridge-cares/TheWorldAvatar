package uk.ac.cam.cares.jps.agent.rfidquery;

import uk.ac.cam.cares.jps.base.email.EmailSender;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import java.io.IOException;
import java.sql.Connection;
import java.text.SimpleDateFormat;
import java.time.*;
import java.util.*;
import java.util.TimeZone;


import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;


/**
 * Class to query for the latest RFID tag status from the knowledge graph and subsequently the chemical species information.
 * @author  */
public class RFIDQueryAgent{

	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(RFIDQueryAgentLauncher.class);

    /**
     * The time series client to interact with the knowledge graph and data storage
     */
    private TimeSeriesClient<OffsetDateTime> tsClient;

    ArrayList<String> dataIRIList = new ArrayList<String>();

    /**
     * Log messages
     */
    private static final String GETLATESTDATA_ERROR_MSG = "Unable to query for latest data!" ;

    /**
     * List to store timeseries string values
     */
    private List<String> dataValuesAsString;

    /**
     * Number of hours
     */
    private long numOfHours ;

    /**
     * timeSeries Object
     */
    TimeSeries<OffsetDateTime> timeseries;
    
    /**
     * RDB Client Object
     */
    private RemoteRDBStoreClient RDBClient;

    /**
     * The Zone offset of the timestamp (https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/ZoneOffset.html)
     */
    public static final ZoneOffset ZONE_OFFSET = ZoneOffset.UTC;

    /**
     * Standard constructor which receives and parses DataIRIs from the AgentLauncher class.
     * @param dataIRIs The data IRIs to query latest data for
     */
    public RFIDQueryAgent(String dataIRIs, String Hours) throws IOException {
        for (int i = 0; i <= dataIRIs.split(",").length - 1; i++) {
            this.dataIRIList.add(i, dataIRIs.split(",")[i]);
        }
        LOGGER.info("The first element in this list is " + dataIRIList.get(0));
        this.numOfHours = Long.valueOf(Hours);
    }

    /**
     * Setter for the time series client.
     * @param tsClient The time series client to use.
     */
    public void setTsClient(TimeSeriesClient<OffsetDateTime> tsClient) {
        this.tsClient = tsClient;
    }

    /**
     * Setter for the remote rdb store client.
     * @param RDBClient The remote rdb store client to use.
     */
    public void setRDBClient(RemoteRDBStoreClient RDBClient) {
        this.RDBClient = RDBClient;
    }

    /**
     * Query the database for the latest RFID tag status and timestamp.
     * @param dataIRI the data IRI to query values from
     */
    public TimeSeries<OffsetDateTime> queryLatestRFIDStatus(String dataIRI)throws IllegalArgumentException {
        try (Connection conn = RDBClient.getConnection()){
            timeseries = tsClient.getLatestData(dataIRI, conn);
        } catch (Exception e) {
            throw new JPSRuntimeException(GETLATESTDATA_ERROR_MSG, e);
        }
        return timeseries;
    }

    /**
     * Queries for latest data and check whether latest timestamps exceed threshold set by user
     * @return resuTts A Json Object with the following format {iri_1: {exceedThreshold: true/false, timestamp: timestamp value, dataIRI: data IRI 1}, 
     *                                                          iri_2: {exceedThreshold: true/false, timestamp: timestamp value, dataIRI: data IRI 2} }
     */
    public JSONObject queriesStatusAndCheckTimeStamps() {
        JSONObject results = new JSONObject();
        for (int i = 0; i <= dataIRIList.size() - 1; i++) {
            JSONObject values = new JSONObject();
            TimeSeries<OffsetDateTime> LatestTimeSeries = queryLatestRFIDStatus(dataIRIList.get(i));
            values = checkRFIDStatusThreshold(LatestTimeSeries, dataIRIList.get(i), numOfHours);
            results.put("iri_"+i, values);
            LOGGER.info(results);
        }
        LOGGER.info("The final result is " + results);
        return results;
    }

    /**
     * 
     * @param timeSeriesObject The timeseries containing the latest timeseries data
     * @param dataIRI The data IRI to retrieve data for
     * @param hours Number of hours
     * @return values A JSONObject with the following format: {exceedThreshold: true/false, timestamp: timestamp value, dataIRI: data IRI 1}
     */
    public JSONObject checkRFIDStatusThreshold (TimeSeries<OffsetDateTime> timeSeriesObject, String dataIRI, Long hours) {
    	Boolean exceedThreshold = false ;
        //process timeseries object and convert to a suitable form, retrieve values only
    	dataValuesAsString = timeSeriesObject.getValuesAsString(dataIRI);
    	String latestTimeSeriesValue = dataValuesAsString.get(dataValuesAsString.size() - 1);
    	OffsetDateTime latestTimeStamp = timeSeriesObject.getTimes().get(timeSeriesObject.getTimes().size() - 1);
    	if (latestTimeSeriesValue.contains("In")) {
    	LOGGER.info("The latest RFID status is " + latestTimeSeriesValue);
    	
    	LOGGER.info("The latest timestamp is "+ latestTimeStamp);

        exceedThreshold = false;

    	} else if (latestTimeSeriesValue.contains("Out")) {
            // if latest status is Out and timestamp is "2022-10-27 18:20:02+08"
			// take timestamp and add the number of hours in which a bottle is allowed to be outside of the cabinet for 
			// e.g. "2022-10-27 18:20:02+08 + 4 hours = 2022-10-27 22:20:02+08"
            OffsetDateTime thresholdTimeStamp = latestTimeStamp.plusHours(hours);
            long timestamp = System.currentTimeMillis();
            Date date = new java.util.Date(timestamp);
            SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            sdf.setTimeZone(TimeZone.getTimeZone("UTC"));
            Object ts = sdf.format(date);
            LocalDateTime localTime = LocalDateTime.parse(ts.toString());
            // Then add the zone id
            OffsetDateTime currentDateTime = OffsetDateTime.of(localTime, ZoneOffset.UTC);

            // if 2022-10-27 22:20:02+08 is before the current date time, it means that the bottle has been outside of the cabinet for a time
            // period longer than the allowed duration
            if (thresholdTimeStamp.isBefore(currentDateTime)) {
                exceedThreshold = true;
            }
            else {
                exceedThreshold = false;
            }
        }
        JSONObject values = new JSONObject();
        values.put("exceedThreshold", exceedThreshold);
        values.put("timestamp", latestTimeStamp.toString());
        values.put("dataIRI", dataIRI);
        return values;
    }

    /**
     * @param tagID RFID tag ID number that is usually append at the end of the data IRI
     * @param chemicalSpeciesName Name of the chemical species that is stored in the tagged bottle
     * @param latestTimeStamp latest timestamp value
     */
    public void sendEmail(String tagID, String chemicalSpeciesName, String latestTimeStamp) {
        try {
            EmailSender sender = new EmailSender();
            String emailMessages;
            emailMessages = "The bottle with the following information has been removed since " + latestTimeStamp.toString() + 
            ". The bottle has the following tag ID " + tagID + " and it is storing " + chemicalSpeciesName + ". This chemical species has the following information: ";
            sender.sendEmail("Alert!", emailMessages);
        
        } catch (Exception e) {
            throw new JPSRuntimeException("Unable to send out alert email!");
        }
    }
}

