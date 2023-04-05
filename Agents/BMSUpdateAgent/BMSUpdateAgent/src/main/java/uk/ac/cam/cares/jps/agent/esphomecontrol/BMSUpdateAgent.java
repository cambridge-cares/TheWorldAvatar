package uk.ac.cam.cares.jps.agent.esphomecontrol;

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
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

public class BMSUpdateAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSUpdateAgent.class);

    private final String STR_OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private final Prefix P_OM = SparqlBuilder.prefix("om", iri(STR_OM));

    private final String esphomeAgentToggle;
    private final String esphomeUpdateAgentRetrieve;

    public BMSUpdateAgent(String esphomeAgentToggle, String esphomeUpdateAgentRetrieve) {
        this.esphomeAgentToggle = esphomeAgentToggle;
        this.esphomeUpdateAgentRetrieve = esphomeUpdateAgentRetrieve;
    }

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

        LOGGER.info("execute modify: " + modifyDataQuery.getQueryString());
        rsClient.executeUpdate(modifyDataQuery.getQueryString());
    }

    public String toggleFan() {
        JSONObject queryJo = new JSONObject();
        queryJo.put("timeseriesDataClientProperties", "CLIENT_PROPERTIES");
        queryJo.put("esphomeStatusClientProperties", "ESPHOME_CLIENT_PROPERTIES");
        queryJo.put("esphomeAPIProperties", "API_PROPERTIES");

        String response = AgentCaller.executePost(esphomeAgentToggle, queryJo.toString());
        return new JSONObject(response).getJSONArray("message").getString(0);
    }

    public void updateStatusInDb() {
        JSONObject queryJo = new JSONObject();
        queryJo.put("agentProperties", "ESPHOME_UPDATE_AGENTPROPERTIES");
        queryJo.put("apiProperties", "ESPHOME_UPDATE_APIPROPERTIES");
        queryJo.put("clientProperties", "ESPHOME_UPDATE_CLIENTPROPERTIES");

        AgentCaller.executePost(esphomeUpdateAgentRetrieve, queryJo.toString());
    }
}
