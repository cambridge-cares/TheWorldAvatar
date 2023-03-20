package uk.ac.cam.cares.jps.agent.bmsquery;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
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
        Variable instance = SparqlBuilder.var("dataIRI");
        Variable label = SparqlBuilder.var("label");

        Iri classIRI = iri(classIRIStr);

        TriplePattern findInstanceOfAClass = GraphPatterns.tp(instance, RDF.TYPE, classIRI);
        TriplePattern findLabelForInstance = GraphPatterns.tp(instance, RDFS.LABEL, label);
        query.prefix(P_BMS).select(instance, label).where(findInstanceOfAClass, findLabelForInstance);
        JSONArray jsonResult = rsClient.executeQuery(query.getQueryString());
        JSONObject result = new JSONObject();
        result.put("Equipments", jsonResult);

        return result;
    }

}
