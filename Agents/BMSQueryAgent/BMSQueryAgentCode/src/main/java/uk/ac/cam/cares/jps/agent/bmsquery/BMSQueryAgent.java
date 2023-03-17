package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.IRI;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.apicatalog.rdf.Rdf.createIRI;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

public class BMSQueryAgent {

    private static final Logger LOGGER = LogManager.getLogger(BMSQueryAgent.class);
    TimeSeriesClient<OffsetDateTime> tsClient;
    RemoteStoreClient rsClient;

    private static final String BMS_STRING = "http://www.theworldavatar.com/BMS/CaresLab#";
    private static final Prefix P_BMS = SparqlBuilder.prefix("bms",iri(BMS_STRING));

    public void setTSClient(TimeSeriesClient<OffsetDateTime> tsClient) {
        this.tsClient = tsClient;
    }

    public void setRSClient(RemoteStoreClient rsClient) {
        this.rsClient = rsClient;
    }

    public JSONObject queryTimeSeriesWithinBound(String dataIRI) {
        List<String> dataIRIs = Collections.singletonList(dataIRI);
        OffsetDateTime currentTime = OffsetDateTime.parse("2023-03-02T09:05:56Z");
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

    public JSONObject queryEquipmentInstance(String classIRIStr) {
        SelectQuery query = Queries.SELECT();
        Variable instance = query.var();

        Iri classIRI = iri(classIRIStr);

        query.prefix(P_BMS).select(instance).where(instance.has(RDF.TYPE, classIRI));
        JSONArray jsonResult = rsClient.executeQuery(query.getQueryString());

        return parseQueryResult(jsonResult);
    }

    private JSONObject parseQueryResult(JSONArray queryResult) {
        ArrayList<String> dataIris = new ArrayList<>();
        LOGGER.error(queryResult.toString());
        for (int i = 0; i < queryResult.length(); i++) {
            dataIris.add(queryResult.getJSONObject(i).getString("x0"));
        }

        JSONObject result = new JSONObject();
        result.put("Equipments", dataIris);
        return result;
    }
}
