package uk.ac.cam.cares.jps.agent.bmsupdate;

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
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

public class BMSUpdateAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSUpdateAgent.class);

    private final String STR_OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private final Prefix P_OM = SparqlBuilder.prefix("om", iri(STR_OM));

    final String FAIL_TO_GET_TEMP = "Fail to get temperature";
    final String FAIL_TO_UPDATE_TEMP = "Fail to update temperature in knowledge graph";
    final String FAIL_TO_TOGGLE = "Fail to trigger ESPHomeAgent to toggle device status";
    final String FAIL_TO_PULL_DATA = "Fail to trigger ESPHomeUpdateAgent to pull data";

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
}
