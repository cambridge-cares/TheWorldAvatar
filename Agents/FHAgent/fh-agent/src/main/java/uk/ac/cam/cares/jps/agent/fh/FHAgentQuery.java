package uk.ac.cam.cares.jps.agent.fh;

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


public class FHAgentQuery {
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(FHAgentLauncher.class);

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
    public FHAgentQuery(String dataIRIs) throws IOException {
        for (int i = 0; i <= dataIRIs.split(",").length - 1; i++) {
            this.dataIRIList.add(i, dataIRIs.split(",")[i]);
        }
        LOGGER.info("The first element in this list is " + dataIRIList.get(0));
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
    
    public TimeSeries<OffsetDateTime> queryLatestDistance(String dataIRI)throws IllegalArgumentException {
        try (Connection conn = RDBClient.getConnection()){
            timeseries = tsClient.getLatestData(dataIRI, conn);
        } catch (Exception e) {
            throw new JPSRuntimeException(GETLATESTDATA_ERROR_MSG, e);
        }
        return timeseries;
    }

    public Boolean getLastOccupiedState(String dataIRI) {
        TimeSeriesClient<OffsetDateTime> occStateTS;
        String latestTimeSeriesValue;
        try (Connection conn = RDBClient.getConnection()){
            occStateTS = tsClient.getLatestData(dataIRI, conn);
            try {
                List<String> occState = occStateTS.getValuesAsString(dataIRI);
                latestTimeSeriesValue = dataValuesAsString.get(dataValuesAsString.size() - 1);
            } catch (Exception e){
                throw new JPSRuntimeException("Unable to retrieve latest value and timestamp from timeseries object.");
            }

        } catch (Exception e) {
            throw new JPSRuntimeException(GETLATESTDATA_ERROR_MSG, e);
        }

        if (latestTimeSeriesValue == "1"){
            return true;
        }

        else {
            return false;
        }        
    }


    

}
