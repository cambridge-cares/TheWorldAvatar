package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.List;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

public class BMSQueryAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSQueryAgent.class);
    RemoteStoreClient rsClient;
    List<String> kgUrls;


    public void setRSClient(RemoteStoreClient rsClient, List<String> kgUrls) {
        this.rsClient = rsClient;
        this.kgUrls = kgUrls;
    }

    public JSONObject queryEquipmentInstance(String classIRIStr) {
        SelectQuery query = Queries.SELECT();
        Variable instance = SparqlBuilder.var("dataIRI");
        Variable label = SparqlBuilder.var("label");

        Iri classIRI = iri(classIRIStr);

        TriplePattern findInstanceOfAClass = GraphPatterns.tp(instance, RDF.TYPE, classIRI);
        TriplePattern findLabelForInstance = GraphPatterns.tp(instance, RDFS.LABEL, label);
        query.select(instance, label).where(findInstanceOfAClass, findLabelForInstance);

        JSONArray jsonResult;
        try {
            LOGGER.info("Sending federated request...");
            jsonResult = rsClient.executeFederatedQuery(kgUrls, query.getQueryString());
        } catch (Exception e) {
            LOGGER.error("Fail to run federated query to get equipment instances of type: " + classIRIStr);
            throw new JPSRuntimeException("Unable to get equipment instances of type: " + classIRIStr);
        }

        JSONObject result = new JSONObject();
        result.put("Equipments", parseLabel(jsonResult));
        LOGGER.info("Getting result:" + result);

        return result;
    }

    // CAUTION: the method assume @ is only for language specification in label
    private JSONArray parseLabel(JSONArray response) {
        for (int i = 0; i < response.length(); i++) {
            JSONObject jo = response.getJSONObject(i);
            String label = jo.getString("label");
            if (label.contains("@")) {
                label = label.split("@")[0];
            }
            if (label.charAt(0) == '"') {
                label = label.substring(1);
            }
            if (label.charAt(label.length()-1) == '"') {
                label = label.substring(0, label.length()-1);
            }
            jo.put("label", label);
        }
        return response;
    }

}
