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
     * Standard constructor for the check route which receives and parses DataIRIs from the AgentLauncher class.
     * @param dataIRIs The data IRIs to query latest data for
     * @param Hours  The threshold number of hours
     */
    public RFIDQueryAgent(String dataIRIs, String Hours) throws IOException {
        for (int i = 0; i <= dataIRIs.split(",").length - 1; i++) {
            this.dataIRIList.add(i, dataIRIs.split(",")[i]);
        }
        LOGGER.info("Created agent for check route...");
        LOGGER.info("The first element in this list is " + dataIRIList.get(0));
        this.numOfHours = Long.valueOf(Hours);
    }

    /**
     * Standard constructor for the retrieveData and sendNotification route
     */
    public RFIDQueryAgent() throws IOException {
        LOGGER.info("Created agent for retrieveData route...");
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
     * @return results
     * A Json Object with the following format {iri_0: {exceedThreshold: true/false, timestamp: timestamp value, dataIRI: data IRI 1}, 
     *                                          iri_1: {exceedThreshold: true/false, timestamp: timestamp value, dataIRI: data IRI 2} }
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
        String latestTimeSeriesValue ;
        OffsetDateTime latestTimeStamp ;

        try {
        //process timeseries object and convert to a suitable form, retrieve values only
    	dataValuesAsString = timeSeriesObject.getValuesAsString(dataIRI);
    	latestTimeSeriesValue = dataValuesAsString.get(dataValuesAsString.size() - 1);
    	latestTimeStamp = timeSeriesObject.getTimes().get(timeSeriesObject.getTimes().size() - 1);
        } catch (Exception e){
            throw new JPSRuntimeException("Unable to retrieve latest value and timestamp from timeseries object.");
        }

    	if (latestTimeSeriesValue.contains("In")) {
    	LOGGER.info("The latest RFID status is " + latestTimeSeriesValue);
    	LOGGER.info("The latest timestamp is "+ latestTimeStamp);
        exceedThreshold = false;
    	} 
        else if (latestTimeSeriesValue.contains("Out")) {
            // if latest status is Out and timestamp is "2022-10-27 18:20:02+08"
			// take timestamp and add the number of hours in which a bottle is allowed to be outside of the cabinet for 
			// e.g. "2022-10-27 18:20:02+08 + 4 hours = 2022-10-27 22:20:02+08"
            //This is the thresholdTimeStamp
            OffsetDateTime thresholdTimeStamp = latestTimeStamp.plusHours(hours);

            //get current datetime
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
            LOGGER.info("Comparing threshold timestamp " + thresholdTimeStamp + " with current timestamp " + currentDateTime);
            if (thresholdTimeStamp.isBefore(currentDateTime)) {
                exceedThreshold = true;
            }
            else {
                exceedThreshold = false;
            }
        }
        
        JSONObject values = new JSONObject();
        Date date = new java.util.Date(latestTimeStamp.toEpochSecond()*1000);
        SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss a z");
        sdf.setTimeZone(TimeZone.getDefault());
        Object ts = sdf.format(date);
        values.put("exceedThreshold", exceedThreshold);
        values.put("timestamp", ts.toString());
        values.put("dataIRI", dataIRI);
        return values;
    }

    /**
     * @param tagStatusIRI tag status IRI
     * @param objectLabel tagged object label
     * @param speciesLabel species label
     * @param latestTimeStamp latest timestamp value
     * @param map Hashmap containing the labels and comments for each GHS Hazard Statement
     */
    public void sendExceedThresholdEmail(String tagStatusIRI, String objectLabel, String speciesLabel, String latestTimeStamp, Map<String, List<String>> map) {
        String emailMessage;
        String tldrMessage;
        EmailSender sender = new EmailSender();
        if (speciesLabel != null && map != null) {
            try {
                tldrMessage = "<b>tl;dr Container with " + speciesLabel + " has been removed for longer than " + numOfHours + " hour.</b> <br> <br>" ;
                emailMessage = tldrMessage + "The chemical container with the following information has been removed for longer than <b>" + numOfHours + " hours since " + latestTimeStamp + "</b>. The container has the label <b>" + objectLabel + "</b> and tag ID <b>" + tagStatusIRI.split("_")[2] + "</b>. The container is storing a chemical with the label <b>" + speciesLabel + "</b> which has the following GHS hazard statements. <br>";
                for (int i = 0; i <= map.get("label").size() - 1; i++) {
                    LOGGER.info("The label from the map is " + map.get("label").get(i));
                    LOGGER.info("The comment from the map is " + map.get("comment").get(i));
                    emailMessage = emailMessage.concat("<b>" + map.get("label").get(i) + ": <i>" + map.get("comment").get(i) + "</i> </b> <br>");
                }
                LOGGER.info("The email message is " + emailMessage);
                sender.sendEmail("Alert!", emailMessage);
            } catch (Exception e) {
                throw new JPSRuntimeException("Unable to send out alert email!");
            }
        } else if (speciesLabel != null && map == null) {
            try {
                tldrMessage = "<b>tl;dr Container with " + speciesLabel + " has been removed for longer than " + numOfHours + " hour.</b> <br> <br>" ;
                emailMessage = tldrMessage + "The chemical container with the following information has been removed for longer than <b>" + numOfHours + " hours since " + latestTimeStamp + "</b>. The container has the label <b>" + objectLabel + "</b> and tag ID <b>" + tagStatusIRI.split("_")[2] + "</b>. The container is storing a chemical with the label <b>" + speciesLabel + "</b>.";

                LOGGER.info("The email message is " + emailMessage);
                sender.sendEmail("Alert!", emailMessage);
            } catch (Exception e) {
                throw new JPSRuntimeException("Unable to send out alert email!");
            }
        } else if (speciesLabel == null && map == null) {
            try {
                emailMessage = "The tagged object has been removed for longer than <b>" + numOfHours + " hours since " + latestTimeStamp + "</b>. The object has the label <b>" + objectLabel + "</b> and tag ID <b>" + tagStatusIRI.split("_")[2] + "</b>.";
                sender.sendEmail("Alert!", emailMessage);
            } catch (Exception e) {
                throw new JPSRuntimeException("Unable to send out alert email!");
            }
        }
    }

    /**
     * @param tagStatusIRI tag status IRI
     * @param objectLabel tagged object label
     * @param speciesLabel species label
     * @param latestStatus latest status and timestamp
     * @param map Hashmap containing the labels and comments for each GHS Hazard Statement
     */
    public void sendStatusChangeEmail(String tagStatusIRI, String objectLabel, String speciesLabel, String latestStatus, Map<String, List<String>> map) {
        String emailMessage;
        String tldrMessage;
        EmailSender sender = new EmailSender();
        if (speciesLabel != null && map != null) {
            try {
                tldrMessage = "<b>tl;dr Container with " + speciesLabel + " is currently " + latestStatus +".</b> <br> <br>" ;
                emailMessage = tldrMessage + "The container has the label <b>" + objectLabel + "</b> and tag ID <b>" + tagStatusIRI.split("_")[2] + "</b>. The container is storing a chemical with the label <b>" + speciesLabel + "</b> which has the following GHS hazard statements. <br>";
                for (int i = 0; i <= map.get("label").size() - 1; i++) {
                    LOGGER.info("The label from the map is " + map.get("label").get(i));
                    LOGGER.info("The comment from the map is " + map.get("comment").get(i));
                    emailMessage = emailMessage.concat("<b>" + map.get("label").get(i) + ": <i>" + map.get("comment").get(i) + "</i> </b> <br>");
                }
                LOGGER.info("The email message is " + emailMessage);
                sender.sendEmail("Alert!", emailMessage);
            } catch (Exception e) {
                throw new JPSRuntimeException("Unable to send out alert email!");
            }
        } else if (speciesLabel != null && map == null) {
            try {
                tldrMessage = "<b>tl;dr Container with " + speciesLabel + " is currently " + latestStatus +".</b> <br> <br>" ;
                emailMessage = tldrMessage + "The container has the label <b>" + objectLabel + "</b> and tag ID <b>" + tagStatusIRI.split("_")[2] + "</b>. The container is storing a chemical with the label <b>" + speciesLabel + "</b>.";

                LOGGER.info("The email message is " + emailMessage);
                sender.sendEmail("Alert!", emailMessage);
            } catch (Exception e) {
                throw new JPSRuntimeException("Unable to send out alert email!");
            }
        } else if (speciesLabel == null && map == null) {
            try {
                emailMessage = "The tagged object " + " is currently <b>" + latestStatus +" .</b> The object has the label <b>" + objectLabel + "</b> and tag ID <b>" + tagStatusIRI.split("_")[2] + "</b>.";
                sender.sendEmail("Alert!", emailMessage);
            } catch (Exception e) {
                throw new JPSRuntimeException("Unable to send out alert email!");
            }
        }
    }
}

