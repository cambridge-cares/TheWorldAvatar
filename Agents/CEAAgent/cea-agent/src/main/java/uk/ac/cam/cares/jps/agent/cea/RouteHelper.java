package uk.ac.cam.cares.jps.agent.cea;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.config.JPSConstants;

import java.util.Arrays;
import java.util.List;

public class RouteHelper  {

    /**
     * Sets store client to the query and update endpoint of route, so that the time series client queries and updates from the same endpoint as route
     * @param route access agent route
     */
    public static List<String> getRouteEndpoints(String route) {
        JSONObject queryResult = AccessAgentCaller.getEndpoints(route);

        String queryEndpoint = queryResult.getString(JPSConstants.QUERY_ENDPOINT);
        String updateEndpoint = queryResult.getString(JPSConstants.UPDATE_ENDPOINT);

        return Arrays.asList(queryEndpoint, updateEndpoint);
    }

    /**
     * Checks if route is enabled to support quads
     * @param route endpoint to check
     * @return  true if quads is enabled in route, false if not
     */
    public static boolean checkQuadsEnabled(String route){
        WhereBuilder wb = new WhereBuilder()
                .addGraph("?g","?s", "?p", "?o");
        SelectBuilder sb = new SelectBuilder()
                .addVar("?g")
                .addWhere(wb)
                .setLimit(1);

        // check if query with graph works for route
        try{
            AccessAgentCaller.queryStore(route, sb.build().toString());
            return true;
        }
        catch (Exception e){
            return false;
        }
    }

    /**
     * Run basic SPARQL query to check if route is queryable
     * @param route endpoint to check
     * @return true if route is queryable, false if not
     */
    public static boolean checkEndpoint(String route){
        WhereBuilder wb = new WhereBuilder()
                .addWhere("?s", "?p", "?o");
        SelectBuilder sb = new SelectBuilder()
                .addVar("?s")
                .addWhere(wb)
                .setLimit(1);

        try{
            AccessAgentCaller.queryStore(route, sb.build().toString());
            return true;
        }
        catch (Exception e){
            return false;
        }
    }
}
