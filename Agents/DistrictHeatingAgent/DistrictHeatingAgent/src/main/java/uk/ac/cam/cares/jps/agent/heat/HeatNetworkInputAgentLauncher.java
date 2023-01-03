package uk.ac.cam.cares.jps.agent.heat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import java.time.OffsetDateTime;
import java.text.ParseException;
import javax.servlet.annotation.WebServlet;
import org.springframework.stereotype.Controller;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import org.json.JSONObject;
import com.ibm.icu.text.SimpleDateFormat;

/**
 * Class with a main method that is the entry point of the compiled war and puts all components together to retrieve
 * @author Hansong Xue
 */

@Controller
@WebServlet(urlPatterns = {"/performheatupdate"})
public class HeatNetworkInputAgentLauncher extends JPSAgent {

    private static final long serialVersionUID = 1L;

    // Logger for reporting info/errors
    private static final Logger LOGGER = LogManager.getLogger(HeatNetworkInputAgentLauncher.class);

    // Logging / error messages
    private static final String INITIALIZE_ERROR_MSG = "Could not initialize time series.";
    private static final String TSCLIENT_ERROR_MSG = "Could not construct the time series client needed by the input agent!";
    private static final String DATAINSTANTIATION = "Could not update static data";
    private static final String UPDATETSDATA = "Could not update time series data";

    // Data format string pattern for converting the time format for TS client
    private static final SimpleDateFormat inSDF = new SimpleDateFormat("dd/MM/yyyyhh:mm:ss a");
    private static final SimpleDateFormat outSDF = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");


    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {

        JSONObject jsonMessage = new JSONObject();
        String TsData = System.getenv("TSPATHTEST");
        String propertiesFile = System.getenv("CLIENTPROPERTY");
        String[] numericValue_date = ReadCol(0,TsData,",");
        Map<String, List<?>> data_TS = new HashMap<String, List<?>>();
        List<String> Date_Utc = new ArrayList<String>();
        for (int i = 1; i < numericValue_date.length; i++) {
            Date_Utc.add(formatDate(numericValue_date[i]));
        }
        data_TS.put("obsTimeUtc", Date_Utc);

        int column_length = 0;
        try {
            column_length = ColNum(TsData, ",");
        } catch (java.io.IOException e) {
            e.printStackTrace();
        }


        for (int i = 1; i < column_length; i++) {
            String[] numericValue = ReadCol(i, TsData, ",");
            data_TS.put(numericValue[0], Arrays.asList(Arrays.copyOfRange(numericValue, 1, numericValue.length)));
        }

        HeatNetworkInputAgent agent = new HeatNetworkInputAgent();

        try {
            agent.dataInstantiation();
            jsonMessage.accumulate("Result", "Static data has been updated.");
        } catch (JPSRuntimeException e) {
            LOGGER.error(DATAINSTANTIATION, e);
            throw new JPSRuntimeException(DATAINSTANTIATION, e);
        }

        TimeSeriesClient<OffsetDateTime> tsClient;
        try {
            tsClient = new TimeSeriesClient<>(OffsetDateTime.class, propertiesFile);
            agent.setTsClient(tsClient);
        } catch (IOException | JPSRuntimeException e) {
            LOGGER.error(TSCLIENT_ERROR_MSG, e);
            throw new JPSRuntimeException(TSCLIENT_ERROR_MSG, e);
        }
        LOGGER.info("Time series client object initialized.");

        try {
            agent.initializeTimeSeriesIfNotExist();
            jsonMessage.accumulate("Result", "Timeseries client initialized.");
        } catch (JPSRuntimeException e) {
            LOGGER.error(INITIALIZE_ERROR_MSG, e);
            throw new JPSRuntimeException(INITIALIZE_ERROR_MSG, e);
        }

        try {
            agent.updateTSData(data_TS);
            jsonMessage.accumulate("Result", "Timeseries Data has been updated.");
        } catch (JPSRuntimeException e) {
            LOGGER.error(UPDATETSDATA, e);
            throw new JPSRuntimeException(UPDATETSDATA, e);
        }
        return jsonMessage;
    }

    // Extract the column from a TS data csv file
    public static String[] ReadCol(int col, String filepath, String delimiter) {
        String currentLine;
        String[] data;
        ArrayList<String> colData = new ArrayList<String>();

        try {
            FileReader fr = new FileReader(filepath);
            BufferedReader br = new BufferedReader(fr);
            while ((currentLine = br.readLine()) != null) {
                data = currentLine.split(delimiter);
                colData.add(data[col]);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
        return colData.toArray(new String[0]);
    }

    // Find the column length of an input csv file
    public static int ColNum(String filepath, String delimiter) throws java.io.IOException {
        FileReader fr_col;
        String[] currentLine;
        fr_col = new FileReader(filepath);
        BufferedReader br_col = new BufferedReader(fr_col);
        currentLine = br_col.readLine().split(",");
        int col_length = currentLine.length;
        return col_length;
    }

    // Convert the TS time data into a suitable format for TS client
    public static String formatDate(String inDate) {
        String outDate = "";
        if (inDate != null) {
            try {
                Date date = inSDF.parse(inDate);
                outDate = outSDF.format(date);
            } catch (ParseException ex) {
            }
        }
        String front = outDate.substring(0, 10);
        String back = outDate.substring(11);
        return front + "T" + back;
    }
}
