package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import uk.ac.cam.cares.jps.agent.dashboard.utils.StringHelper;

import java.util.*;
import java.util.stream.Stream;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about the variable syntax specific to Grafana dashboard.
 *
 * @author qhouyee
 */
public class TemplatingModel {
    private final StringBuilder VARIABLES_SYNTAX = new StringBuilder();

    /**
     * Constructor that process customisable options for the templating variable in Grafana's JSON model.
     *
     * @param databaseConnectionMap A map linking each database to its connection ID.
     * @param timeSeries            A map of all assets and rooms mapped to their time series.
     */
    public TemplatingModel(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> timeSeries) {
        // Initialise a queue to store these template variables
        Queue<TemplateVariable> variableQueue = new ArrayDeque<>();
        // For each asset type or rooms available
        for (String item : timeSeries.keySet()) {
            // Retrieve the map of measures to their time series metadata
            Map<String, List<String[]>> measures = timeSeries.get(item);
            // Within the map, there is a list containing only the asset or room names
            String nestedKey = item.equals(StringHelper.ROOM_KEY) ? StringHelper.ROOM_KEY : StringHelper.ASSET_KEY; // The key name will vary depending on if it is a room or asset
            // Retrieve this list and convert it into one array to be processed for
            // creating the custom variable for filtering individual assets in one asset type group or rooms, and add into the queue
            String[] arrayItems = measures.get(nestedKey).stream().flatMap(Stream::of).distinct().toArray(String[]::new);
            CustomVariable variable = new CustomVariable(item, arrayItems, 0);
            variableQueue.offer(variable);
            // For each of the measures, create a postgres variable that is tied to their asset type or room custom variable
            for (String measure : measures.keySet()) {
                // Take note to exclude the assets and rooms keys as they are not required
                if (!measure.equals(StringHelper.ASSET_KEY) && !measure.equals(StringHelper.ROOM_KEY)) {
                    // Retrieve the relevant database and database ID from the first item
                    // Assumes that each measure of a specific asset type belongs to only one database
                    String database = measures.get(measure).get(0)[3];
                    PostgresVariable postgresVariable = new PostgresVariable(measure, item, databaseConnectionMap.get(database), measures.get(measure));
                    variableQueue.offer(postgresVariable);
                }
            }
        }
        // While there are still items in the queue,
        while (!variableQueue.isEmpty()) {
            // Append a comma before that variable if it is not the first variable
            if (this.VARIABLES_SYNTAX.length() != 0) this.VARIABLES_SYNTAX.append(",");
            // Retrieve the variable and remove it from the queue
            TemplateVariable currentVar = variableQueue.poll();
            // Construct its syntax and append it
            this.VARIABLES_SYNTAX.append(currentVar.construct());
        }
    }

    /**
     * Construct the JSON model as a String.
     *
     * @return The JSON model syntax as a String.
     */
    public String construct() {
        StringBuilder builder = new StringBuilder();
        // Enable templating in the dashboard
        builder.append("{\"enable\": true,")
                // List of all variables
                .append("\"list\": [").append(this.VARIABLES_SYNTAX).append("]")
                .append("}");
        return builder.toString();
    }
}
