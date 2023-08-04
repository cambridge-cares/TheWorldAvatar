package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

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
     * @param assets                A map of all assets mapped to their asset types.
     */
    public TemplatingModel(Map<String, String> databaseConnectionMap, Map<String, Map<String, List<String[]>>> assets) {
        // Initialise a queue to store these template variables
        Queue<TemplateVariable> variableQueue = new ArrayDeque<>();
        // Create the first custom variable for filtering asset types ONLY IF >1 asset type
        if (assets.keySet().size() > 1) {
            // Initialise an array for all the available types
            String[] assetTypes = assets.keySet().toArray(new String[0]);
            // Initialise a custom variable
            CustomVariable variable = new CustomVariable(assetTypes, 0);
            variableQueue.offer(variable);
        }
        // For each asset type available
        for (String assetType : assets.keySet()) {
            // Retrieve the map of measures to their asset and time series metadata
            Map<String, List<String[]>> measures = assets.get(assetType);
            // Within the map, there is a list containing only the asset names
            // Retrieve this list and convert it into one array to be processed for
            // creating the custom variable for filtering individual assets in one asset type group, and add into the queue
            String[] individualAssets = measures.get("assets").stream().flatMap(Stream::of).distinct().toArray(String[]::new);
            CustomVariable variable = new CustomVariable(assetType, individualAssets, 0);
            variableQueue.offer(variable);
            // For each of the measures, create a postgres variable that is tied to their asset type custom variable
            for (String measure : measures.keySet()) {
                // Take note to exclude the assets key as that is not required
                if (!measure.equals("assets")) {
                    // Retrieve the relevant database and database ID from the first item
                    // Assumes that each measure of a specific asset type belongs to only one database
                    String database = measures.get(measure).get(0)[3];
                    PostgresVariable postgresVariable = new PostgresVariable(measure, assetType, databaseConnectionMap.get(database), measures.get(measure));
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
