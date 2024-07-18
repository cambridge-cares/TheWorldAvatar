package uk.ac.cam.cares.jps.agent.bmsupdate;

import org.apache.jena.query.ParameterizedSparqlString;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.Literal;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.model.impl.SimpleValueFactory;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.util.Date;
import java.util.TimeZone;

public class BMSUpdateAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSUpdateAgent.class);

    private final String STR_OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private final Prefix P_OM = SparqlBuilder.prefix("om", iri(STR_OM));

    private final String STR_ONTOBMS = "https://www.theworldavatar.com/kg/ontobms/";
    private final Prefix P_ONTOBMS = SparqlBuilder.prefix("ontobms", iri(STR_ONTOBMS));

    final String FAIL_TO_GET_TEMP = "Fail to get temperature!";
    final String FAIL_TO_UPDATE_TEMP = "Fail to update temperature in knowledge graph!";
    final String FAIL_TO_TOGGLE = "Fail to trigger ESPHomeAgent to toggle device status!";
    final String FAIL_TO_PULL_DATA = "Fail to trigger ESPHomeUpdateAgent to pull data!";
    final String FAIL_TO_GET_BACNETDEVICEID = "Fail to get Bacnet Device ID!";
    final String FAIL_TO_GET_BACNETOBJECTID = "Fail to get Bacnet Object ID!";
    final String FAIL_TO_GET_LATESTDATA = "Unable to get latest timeseries data for the following IRI: ";
    final String FAIL_TO_UPDATE_NEW_VALUE = "Unable to update new numerical value for the following IRI: ";
    final String FAIL_TO_INSERT_TRIPLES = "Unable to insert the following triples: ";
    final String FAIL_TO_DELETE_TRIPLES = "Unable to delete the following triples: ";

    /**
     * Change the temperature of a fan's set point in the knowledge graph.
     *
     * @param dataIRI The iri of the set point
     * @param newTemperature The temperature
     * @param rsClient The knowledge graph client
     */
    public void setTemperatureInKg(String dataIRI, double newTemperature, RemoteStoreClient rsClient) {
        ModifyQuery modifyDataQuery = Queries.MODIFY();
        Variable temperatureVar = SparqlBuilder.var("temperature");
        TriplePattern deleteTemperature = GraphPatterns.tp(iri(dataIRI), P_OM.iri("hasNumericalValue"), temperatureVar);
        modifyDataQuery.prefix(P_OM).delete(deleteTemperature);
        modifyDataQuery.where(deleteTemperature);

        ValueFactory factory = SimpleValueFactory.getInstance();
        Literal newTemperatureLiteral = factory.createLiteral(newTemperature);
        TriplePattern insertTemperature = GraphPatterns.tp(iri(dataIRI), P_OM.iri("hasNumericalValue"), newTemperatureLiteral);
        modifyDataQuery.insert(insertTemperature);

        try {
            LOGGER.info("sending sparql request to: " + rsClient.getUpdateEndpoint());
            LOGGER.info("execute modify: " + modifyDataQuery.getQueryString());
            rsClient.executeUpdate(modifyDataQuery.getQueryString());
        } catch (Exception e) {
            LOGGER.error(FAIL_TO_UPDATE_TEMP);
            throw new JPSRuntimeException(FAIL_TO_UPDATE_TEMP);
        }
    }

    /**
     * Get the temperature of the current set point in the knowledge graph.
     *
     * @param dataIRI The iri of the set point
     * @param rsClient The knowledge graph client
     * @return
     */
    public double getTemperatureInKg(String dataIRI, RemoteStoreClient rsClient) {
        SelectQuery selectQuery = Queries.SELECT();
        Variable temperatureVar = SparqlBuilder.var("temperature");
        TriplePattern getTemperature = GraphPatterns.tp(iri(dataIRI), P_OM.iri("hasNumericalValue"), temperatureVar);
        selectQuery.prefix(P_OM).select(temperatureVar).where(getTemperature);
        try {
            LOGGER.info("getting original temperature...");
            LOGGER.info("execute query: " + selectQuery.getQueryString());
            JSONArray results = new JSONArray(rsClient.execute(selectQuery.getQueryString()));
            return results.getJSONObject(0).getDouble("temperature");
        } catch (Exception e) {
            LOGGER.error(FAIL_TO_GET_TEMP);
            throw new JPSRuntimeException(FAIL_TO_GET_TEMP);
        }
    }

    /**
     * Call ESPHomeAgent to compare the current temperature and the set point in the knowledge graph and turn on/off the fan.
     *
     * @param esphomeAgentToggle The url to ESPHomeAgent's toggle function.
     * @return The status of the fan after toggling.
     */
    public String toggleFan(String esphomeAgentToggle) {
        JSONObject queryJo = new JSONObject();
        queryJo.put("timeseriesDataClientProperties", "CLIENT_PROPERTIES");
        queryJo.put("esphomeStatusClientProperties", "ESPHOME_CLIENT_PROPERTIES");
        queryJo.put("esphomeAPIProperties", "API_PROPERTIES");

        try {
            LOGGER.info("toggle device status with ESPHomeAgent at " + esphomeAgentToggle);
            String response = AgentCaller.executePost(esphomeAgentToggle, queryJo.toString());
            return parseFanStatusMessage(response);
        } catch (Exception e) {
            LOGGER.error(FAIL_TO_TOGGLE);
            throw new JPSRuntimeException(FAIL_TO_TOGGLE);
        }
    }

    /**
     * Trigger the ESPHomeUpdateAgent to get fan status timeseries data from the ESPHome API to local postgres database.
     *
     * @param esphomeUpdateAgentRetrieve The url to ESPHomeUpdateAgent's retrieve function.
     */
    public void updateStatusInDb(String esphomeUpdateAgentRetrieve) {
        JSONObject queryJo = new JSONObject();
        queryJo.put("agentProperties", "ESPHOME_UPDATE_AGENTPROPERTIES");
        queryJo.put("apiProperties", "ESPHOME_UPDATE_APIPROPERTIES");
        queryJo.put("clientProperties", "ESPHOME_UPDATE_CLIENTPROPERTIES");

        try {
            LOGGER.info("trigger espHomeUpdateAgent at " + esphomeUpdateAgentRetrieve);
            AgentCaller.executePost(esphomeUpdateAgentRetrieve, queryJo.toString());
        } catch (Exception e) {
            LOGGER.error(FAIL_TO_PULL_DATA);
            throw new JPSRuntimeException(FAIL_TO_PULL_DATA);
        }

    }

    /**
     * Helper function that parse the ESPHomeAgent's return message to fan status message.
     *
     * @param response Response from ESPHomeAgent toggle
     * @return Fan status message.
     */
    private String parseFanStatusMessage(String response) {
        String statusString = new JSONObject(response).getJSONArray("message").getString(0).toLowerCase();
        if (statusString.contains("on state") || statusString.contains("turn on")) {
            return "The fan is in the ON state.";
        } else if (statusString.contains("off state") || statusString.contains("turn off")) {
            return "The fan is in the OFF state.";
        }
        LOGGER.warn("Unknown fan state, cannot parse the response");
        return "";
    }

    /**
     * Get the Bacnet Device ID of the data IRI
     *
     * @param dataIRI The data iri
     * @param rsClient The knowledge graph client
     * @return Bacnet Device ID of data IRI
     */
    public String getDeviceId(String dataIRI, RemoteStoreClient rsClient) {
        SelectQuery selectQuery = Queries.SELECT();
        Variable deviceIdVar = SparqlBuilder.var("deviceIdVar");
        TriplePattern getBacnetDeviceId = GraphPatterns.tp(iri(dataIRI), P_ONTOBMS.iri("hasBacnetDeviceID"), deviceIdVar);
        selectQuery.prefix(P_ONTOBMS).select(deviceIdVar).where(getBacnetDeviceId);
        try {
            LOGGER.info("getting Bacnet Device ID of " + dataIRI +" ...");
            LOGGER.info("execute query: " + selectQuery.getQueryString());
            JSONArray results = new JSONArray(rsClient.execute(selectQuery.getQueryString()));
            return results.getJSONObject(0).getString("deviceIdVar");
        } catch (Exception e) {
            LOGGER.error(FAIL_TO_GET_BACNETDEVICEID);
            throw new JPSRuntimeException(FAIL_TO_GET_BACNETDEVICEID);
        }
    }

    /**
     * Get the Bacnet Object ID of the data IRI
     *
     * @param dataIRI The data iri
     * @param rsClient The knowledge graph client
     * @return Bacnet Object ID of data IRI
     */
    public String getObjectId(String dataIRI, RemoteStoreClient rsClient) {
        SelectQuery selectQuery = Queries.SELECT();
        Variable objectIdVar = SparqlBuilder.var("objectIdVar");
        TriplePattern getBacnetObjectId = GraphPatterns.tp(iri(dataIRI), P_ONTOBMS.iri("hasBacnetObjectID"), objectIdVar);
        selectQuery.prefix(P_ONTOBMS).select(objectIdVar).where(getBacnetObjectId);
        try {
            LOGGER.info("getting Bacnet Object ID of " + dataIRI +" ...");
            LOGGER.info("execute query: " + selectQuery.getQueryString());
            JSONArray results = new JSONArray(rsClient.execute(selectQuery.getQueryString()));
            return results.getJSONObject(0).getString("objectIdVar");
        } catch (Exception e) {
            LOGGER.error(FAIL_TO_GET_BACNETOBJECTID);
            throw new JPSRuntimeException(FAIL_TO_GET_BACNETOBJECTID);
        }
    }

    private boolean check(String dataIRI, RemoteStoreClient rsClient, RemoteRDBStoreClient RDBClient, String triggerValue) {
        TimeSeries<OffsetDateTime> timeseries;
        TimeSeriesClient<OffsetDateTime> tsClient = new TimeSeriesClient<>(rsClient ,OffsetDateTime.class);
        try (Connection conn = RDBClient.getConnection()) {
            timeseries = tsClient.getLatestData(dataIRI, conn);
        } catch (Exception e) {
            throw new JPSRuntimeException(FAIL_TO_GET_LATESTDATA + dataIRI);
        }
        String latestValue = timeseries.getValuesAsString(dataIRI).get(timeseries.getValuesAsString(dataIRI).size() - 1);
        OffsetDateTime latestTimeStamp = timeseries.getTimes().get(timeseries.getTimes().size() - 1);
        Date date = new java.util.Date(latestTimeStamp.toEpochSecond()*1000);
        SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss a z");
        sdf.setTimeZone(TimeZone.getDefault());
        Object ts = sdf.format(date);
        LOGGER.info("The latest value for " + dataIRI +" is " + latestValue + " and the corresponding timestamp is " + ts.toString());
        if (latestValue.equals(triggerValue)) {
            return true;
        } else {
            return false;
        }
    }

    private String insert(RemoteStoreClient rsClient,String insertString) {
        ParameterizedSparqlString queryStr = new ParameterizedSparqlString();
        queryStr.append("INSERT DATA ");
        queryStr.append("{ ");
        queryStr.append(insertString);
        queryStr.append(" }");
        LOGGER.info("The triples to insert is " + queryStr.toString());
        try {
            rsClient.executeUpdate(queryStr.asUpdate().toString());
            LOGGER.info("Executed the following to the knowledge graph: " + queryStr.asUpdate().toString());
            return "Executed the following to the knowledge graph: " + queryStr.asUpdate().toString();
        } catch (Exception e) {
            throw new JPSRuntimeException(FAIL_TO_INSERT_TRIPLES + queryStr.asUpdate().toString());
        }
    }

    private String delete(RemoteStoreClient rsClient,String deleteString) {
        ParameterizedSparqlString queryStr = new ParameterizedSparqlString();
        queryStr.append("DELETE DATA ");
        queryStr.append("{ ");
        queryStr.append(deleteString);
        queryStr.append(" }");
        LOGGER.info("The triples to delete is " + queryStr.toString());
        try {
            rsClient.executeUpdate(queryStr.asUpdate().toString());
            LOGGER.info("Executed the following to the knowledge graph: " + queryStr.asUpdate().toString());
            return "Executed the following to the knowledge graph: " + queryStr.asUpdate().toString();
        } catch (Exception e) {
            throw new JPSRuntimeException(FAIL_TO_DELETE_TRIPLES + queryStr.asUpdate().toString());
        }
    }

    /**
     * Get latest timeseries value for the data IRI and compared it with the trigger value,
     * if they are equal, run a sparql update to insert a set of triples and delete a set of triples
     * @param dataIRI The data iri
     * @param rsClient The knowledge graph client
     * @param RDBClient The remote RDB client
     * @param triggerValue The value to compare with the latest timeseries value
     * @param insertString The string to insert via sparql update
     * @param deleteString The string to delete via sparql update
     * @return 
     */
    public JSONObject checkInsertAndDelete(String dataIRI, RemoteStoreClient rsClient, RemoteRDBStoreClient RDBClient, String triggerValue, String insertString, String deleteString) {
        JSONObject result = new JSONObject();
        boolean check = check(dataIRI, rsClient, RDBClient, triggerValue);
        if (check == true) {
            result.accumulate("message", insert(rsClient, insertString));
            result.accumulate("message", delete(rsClient, deleteString));
        } else {
            result.accumulate("message", "The latest timeseries value of " + dataIRI + " is not equivalent to the trigger value " + triggerValue);
        }
        return result;
    }

    /**
     * Get latest timeseries value for the data IRI and compared it with the trigger value, if they are equal, run a sparql update to insert a set of triples
     *
     * @param dataIRI The data iri
     * @param rsClient The knowledge graph client
     * @param RDBClient The remote RDB client
     * @param triggerValue The value to compare with the latest timeseries value
     * @param insertString The string to insert via sparql update
     * @return 
     */
    public JSONObject checkInsert(String dataIRI, RemoteStoreClient rsClient, RemoteRDBStoreClient RDBClient, String triggerValue, String insertString) {
        JSONObject result = new JSONObject();
        boolean check = check(dataIRI, rsClient, RDBClient, triggerValue);
        if (check == true) {
            result.accumulate("message", insert(rsClient, insertString));
        } else {
            result.accumulate("message", "The latest timeseries value of " + dataIRI + " is not equivalent to the trigger value " + triggerValue);
        }
        return result;
    }

    /**
     * Get latest timeseries value for the data IRI and compared it with the trigger value, if they are equal, run a sparql update to delete a set of triples
     *
     * @param dataIRI The data iri
     * @param rsClient The knowledge graph client
     * @param RDBClient The remote RDB client
     * @param triggerValue The value to compare with the latest timeseries value
     * @param deleteString The string to insert via sparql update
     * @return 
     */
    public JSONObject checkDelete(String dataIRI, RemoteStoreClient rsClient, RemoteRDBStoreClient RDBClient, String triggerValue, String deleteString) {
        JSONObject result = new JSONObject();
        boolean check = check(dataIRI, rsClient, RDBClient, triggerValue);
        if (check == true) {
            result.accumulate("message", delete(rsClient, deleteString));
        } else {
            result.accumulate("message", "The latest timeseries value of " + dataIRI + " is not equivalent to the trigger value " + triggerValue);
        }
        return result;
    }

    /**
     * Change the numerical value of a om:Measure or om:Point in the knowledge graph.
     *
     * @param dataIRI The iri of the om:Measure or om:Point
     * @param newValue the new value to insert
     * @param rsClient The knowledge graph client
     */
    public JSONObject setNumericalValue(String dataIRI, double newValue, RemoteStoreClient rsClient) {
        JSONObject result = new JSONObject();
        ModifyQuery modifyDataQuery = Queries.MODIFY();
        Variable numValue = SparqlBuilder.var("numValue");
        TriplePattern deleteExistingNumValue = GraphPatterns.tp(iri(dataIRI), P_OM.iri("hasNumericalValue"), numValue);
        modifyDataQuery.prefix(P_OM).delete(deleteExistingNumValue);
        modifyDataQuery.where(deleteExistingNumValue);

        ValueFactory factory = SimpleValueFactory.getInstance();
        Literal newValueLiteral = factory.createLiteral(newValue);
        TriplePattern insertNewNumValue = GraphPatterns.tp(iri(dataIRI), P_OM.iri("hasNumericalValue"), newValueLiteral);
        modifyDataQuery.insert(insertNewNumValue);

        try {
            LOGGER.info("sending sparql request to: " + rsClient.getUpdateEndpoint());
            LOGGER.info("execute modify: " + modifyDataQuery.getQueryString());
            rsClient.executeUpdate(modifyDataQuery.getQueryString());
            result.put("message", "Sucessfully executed the following query string: " + modifyDataQuery.getQueryString());
        } catch (Exception e) {
            LOGGER.error(FAIL_TO_UPDATE_NEW_VALUE + dataIRI);
            throw new JPSRuntimeException(FAIL_TO_UPDATE_NEW_VALUE + dataIRI);
        }
        return result;
    }
}
