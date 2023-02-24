package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;

public class BMSQueryAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSQueryAgent.class);
    TimeSeriesClient<OffsetDateTime> tsClient;

    public void setTsClient(TimeSeriesClient<OffsetDateTime> tsClient) {
        this.tsClient = tsClient;
    }

    public JSONObject queryTimeSeriesWithinBound(String dataIRI) {
        List<String> dataIRIs = Collections.singletonList(dataIRI);
        OffsetDateTime currentTime = OffsetDateTime.now();
        OffsetDateTime oneHourAgo = currentTime.minusHours(1);

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ssZ");
        LOGGER.info("Querying the time series data from " + oneHourAgo.format(formatter) + " to " + currentTime.format(formatter));

        TimeSeries<OffsetDateTime> result = tsClient.getTimeSeriesWithinBounds(dataIRIs, oneHourAgo, currentTime);
        LOGGER.info(result.getValues(dataIRI).size() + " data values are received");

        JSONObject jsonResult = new JSONObject();
        jsonResult.put("times", result.getTimes());
        jsonResult.put("values", result.getValuesAsDouble(dataIRI));
        return jsonResult;
    }
}
