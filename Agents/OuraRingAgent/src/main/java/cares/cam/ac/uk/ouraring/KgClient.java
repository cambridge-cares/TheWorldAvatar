package cares.cam.ac.uk.ouraring;

import java.io.IOException;
import java.nio.file.Path;
import java.sql.Connection;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.constraint.propertypath.PropertyPath;
import org.eclipse.rdf4j.sparqlbuilder.constraint.propertypath.builder.PropertyPathBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.springframework.core.io.ClassPathResource;

import com.cmclinnovations.stack.clients.blazegraph.BlazegraphClient;
import com.cmclinnovations.stack.clients.ontop.OntopClient;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;

import cares.cam.ac.uk.ouraring.data.User;
import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesRDBClientWithReducedTables;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class KgClient {
    private static final Logger LOGGER = LogManager.getLogger(KgClient.class);

    RemoteStoreClient remoteStoreClient;
    RemoteStoreClient ontopClient;
    RemoteRDBStoreClient remoteRDBStoreClient;
    TimeSeriesClient<Instant> timeSeriesClient;
    TimeSeriesRDBClientWithReducedTables<Instant> timeSeriesRDBClientWithReducedTables = new TimeSeriesRDBClientWithReducedTables<>(
            Instant.class);

    static final String NAMESPACE = "https://www.theworldavatar.com/kg/ouraring/";

    static final Prefix PREFIX_OURA = SparqlBuilder.prefix("oura", Rdf.iri(NAMESPACE));
    static final Prefix PREFIX_OM = SparqlBuilder.prefix("om",
            Rdf.iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));

    static final Iri HAS_OURARING = PREFIX_OURA.iri("hasOuraRing");
    static final Iri MEASURES = PREFIX_OURA.iri("measures");
    static final Iri HAS_SOURCE = PREFIX_OURA.iri("hasSource"); // heart rate source
    static final Iri HAS_BPM = PREFIX_OURA.iri("hasBPM"); // heart rate value
    static final Iri HAS_VALUE = PREFIX_OM.iri("hasValue");

    static final Iri HAS_LAST_UPDATE = PREFIX_OURA.iri("hasLastUpdate");

    public KgClient() {
        remoteStoreClient = BlazegraphClient.getInstance().getRemoteStoreClient(Config.NAMESPACE);
        ontopClient = new RemoteStoreClient(OntopClient.getInstance().readEndpointConfig().getUrl());
        remoteRDBStoreClient = PostGISClient.getInstance().getRemoteStoreClient();
        timeSeriesClient = new TimeSeriesClient<>(remoteStoreClient, timeSeriesRDBClientWithReducedTables);
    }

    void setUserIri(List<User> users) {
        SelectQuery query = Queries.SELECT();
        Variable user = query.var();
        Variable userId = query.var();

        ValuesPattern valuesPattern = new ValuesPattern(userId,
                users.stream().map(u -> Rdf.literalOf(u.getId())).collect(Collectors.toList()));

        query.where(user.has(RDFS.LABEL, userId), valuesPattern);
        JSONArray queryResult = ontopClient.executeQuery(query.getQueryString());

        Map<String, User> idToUserMap = new HashMap<>();
        users.forEach(u -> idToUserMap.put(u.getId(), u));

        for (int i = 0; i < queryResult.length(); i++) {
            String idString = queryResult.getJSONObject(i).getString(userId.getVarName());
            idToUserMap.get(idString).setIri(queryResult.getJSONObject(i).getString(user.getVarName()));
        }
    }

    void initialiseOntop(List<User> users) {
        // checking one user is sufficient
        SelectQuery query = Queries.SELECT();
        Variable ouraring = query.var();

        query.where(Rdf.iri(users.get(0).getIri()).has(HAS_OURARING, ouraring)).prefix(PREFIX_OURA);

        JSONArray queryResult = ontopClient.executeQuery(query.getQueryString());

        if (queryResult.isEmpty()) {
            Path obdaFile = null;
            try {
                obdaFile = new ClassPathResource("oura.obda").getFile().toPath();
            } catch (IOException e) {
                LOGGER.error("Could not retrieve oura.obda file.");
                throw new RuntimeException("Could not retrieve ontop.obda file.", e);
            }
            OntopClient.getInstance().updateOBDA(obdaFile);
        }
    }

    void setHeartRateIris(List<User> users) {
        // get bpm measure IRI and source IRI (indicate whether user is resting etc.)
        SelectQuery query = Queries.SELECT();
        Variable userVar = query.var();
        Variable userIdVar = query.var();
        Variable heartRate = query.var();
        Variable bpmVar = query.var();
        Variable sourceVar = query.var();

        ValuesPattern valuesPattern = new ValuesPattern(userIdVar,
                users.stream().map(u -> Rdf.literalOf(u.getId())).collect(Collectors.toList()));

        PropertyPath heartRatePath = PropertyPathBuilder.of(HAS_OURARING).then(MEASURES).build();
        PropertyPath bpmPath = PropertyPathBuilder.of(HAS_BPM).then(HAS_VALUE).build();

        query.where(userVar.has(heartRatePath, heartRate),
                heartRate.has(bpmPath, bpmVar).andHas(HAS_SOURCE, sourceVar), valuesPattern)
                .prefix(PREFIX_OURA, PREFIX_OM);

        JSONArray queryResult = ontopClient.executeQuery(query.getQueryString());

        Map<String, User> idToUserMap = new HashMap<>();
        users.forEach(u -> idToUserMap.put(u.getId(), u));

        for (int i = 0; i < queryResult.length(); i++) {
            String userIdString = queryResult.getJSONObject(i).getString(userIdVar.getVarName());
            User user = idToUserMap.get(userIdString);
            user.getHeartRateData().setBpmIri(queryResult.getJSONObject(i).getString(bpmVar.getVarName()));
            user.getHeartRateData().setSourceIri(queryResult.getJSONObject(i).getString(sourceVar.getVarName()));
        }
    }

    void initialiseTimeSeries(List<User> users, Connection conn) {
        List<List<String>> dataIRIList = new ArrayList<>();
        List<List<Class<?>>> dataClassList = new ArrayList<>();

        users.forEach(user -> {
            if (!timeSeriesRDBClientWithReducedTables.checkDataHasTimeSeries(user.getHeartRateData().getBpmIri(),
                    conn)) {
                List<String> dataIRI = new ArrayList<>();
                dataIRI.add(user.getHeartRateData().getBpmIri());
                dataIRI.add(user.getHeartRateData().getSourceIri());

                List<Class<?>> dataClass = List.of(Integer.class, String.class);

                dataIRIList.add(dataIRI);
                dataClassList.add(dataClass);
            }
        });

        if (!dataIRIList.isEmpty()) {
            timeSeriesClient.bulkInitTimeSeries(dataIRIList, dataClassList, null, conn);
        }
    }

    void addTimeSeriesData(List<User> users, Connection conn) {
        List<TimeSeries<Instant>> timeSeriesList = new ArrayList<>();

        users.forEach(user -> {
            TimeSeries<Instant> timeseries = user.getHeartRateData().getHeartRateTimeSeries();
            if (!timeseries.getTimes().isEmpty()) {
                timeSeriesList.add(timeseries);
            }
        });

        if (!timeSeriesList.isEmpty()) {
            timeSeriesClient.bulkaddTimeSeriesData(timeSeriesList, conn);
        }
    }

    void setLastUpdate(List<User> users, Connection conn) {
        users.forEach(user -> {
            if (timeSeriesRDBClientWithReducedTables.checkDataHasTimeSeries(user.getHeartRateData().getBpmIri(),
                    conn)) {
                TimeSeries<Instant> tsLatest = timeSeriesClient.getLatestData(user.getHeartRateData().getBpmIri(),
                        conn);
                if (!tsLatest.getTimes().isEmpty()) {
                    user.getHeartRateData().setLastUpdate(tsLatest.getTimes().get(0));
                }
            }
        });
    }
}
