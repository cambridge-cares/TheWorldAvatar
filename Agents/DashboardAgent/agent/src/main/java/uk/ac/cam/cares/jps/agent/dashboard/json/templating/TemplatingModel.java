package uk.ac.cam.cares.jps.agent.dashboard.json.templating;

import java.util.*;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information
 * about the variable syntax specific to Grafana dashboard.
 *
 * @author qhouyee
 */
public class TemplatingModel {
    private final StringBuilder VARIABLES_SYNTAX = new StringBuilder();

    /**
     *  Constructor that process customisable options for the templating variable in Grafana's JSON model.
     *
     * @param assets       A map of all assets mapped to their asset types.
     */
    public TemplatingModel(Map<String, List<String>> assets) {
        // Initialise a queue to store these custom variables
        Queue<CustomVariable> variableQueue = new ArrayDeque<>();
        // Create the first custom variable for filtering asset types ONLY IF >1 asset type
        if (assets.keySet().size()>1){
            // Initialise an array for all the available types
            String[] assetTypes = assets.keySet().toArray(new String[0]);
            // Initialise a custom variable
            CustomVariable variable = new CustomVariable(assetTypes, 0);
            variableQueue.offer(variable);
        }
        // For each asset type available
        for (String assetType: assets.keySet()) {
            // Create custom variables for all of their individual assets
            String[] individualAssets = assets.get(assetType).toArray(new String[0]);
            CustomVariable variable = new CustomVariable(assetType, individualAssets, 0);
            // Append it to the queue
            variableQueue.offer(variable);
        }
        // While there are still items in the queue,
        while(!variableQueue.isEmpty()) {
            // Append a comma before that variable if it is not the first variable
            if(this.VARIABLES_SYNTAX.length()!=0) this.VARIABLES_SYNTAX.append(",");
            // Retrieve the variable and remove it from the queue
            CustomVariable currentVar = variableQueue.poll();
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
