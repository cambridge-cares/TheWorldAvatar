package com.cmclinnovations.dispersion;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.derivation.Derivation;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

/**
 * sends sparql queries
 */
public class QueryClient {
    private StoreClientInterface storeClient;
    private DerivationClient derivationClient;
    private TimeSeriesClient<Long> tsClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);

    static final String PREFIX = "http://www.theworldavatar.com/kg/dispersion/";
    static final Prefix P_DISP = SparqlBuilder.prefix("disp",iri(PREFIX));
    private static final Prefix P_OM = SparqlBuilder.prefix("om",iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));

    // classes
    private static final Iri MEASURE = P_OM.iri("Measure");
    private static final Iri SCOPE = P_DISP.iri("Scope");
    private static final Iri SIMULATION_TIME = P_DISP.iri("SimulationTime");
    private static final Iri NX = P_DISP.iri("nx");
    private static final Iri NY = P_DISP.iri("ny");
    private static final Iri REPORTING_STATION = iri("https://www.theworldavatar.com/kg/ontoems/ReportingStation");
    private static final Iri DISPERSION_MATRIX = P_DISP.iri("DispersionMatrix");
    private static final Iri DISPERSION_LAYER = P_DISP.iri("DispersionLayer");
    private static final Iri SHIPS_LAYER = P_DISP.iri("ShipsLayer");

    // properties
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");

    public QueryClient(RemoteStoreClient storeClient, RemoteRDBStoreClient remoteRDBStoreClient, TimeSeriesClient<Long> tsClient) {
        this.storeClient = storeClient;
        this.derivationClient = new DerivationClient(storeClient, PREFIX);
        this.remoteRDBStoreClient = remoteRDBStoreClient;
        this.tsClient = tsClient;
    }

    void initialiseAgent() {
        Iri service = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Service");
        Iri operation = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation");
        Iri hasOperation = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation");
        Iri hasHttpUrl = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl");
        Iri hasInput = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasInput");
        Iri hasMandatoryPart = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasMandatoryPart");
        Iri hasType = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasType");
        
        Iri operationIri = iri(PREFIX + UUID.randomUUID());
        Iri inputIri = iri(PREFIX + UUID.randomUUID());
        Iri partIri = iri(PREFIX + UUID.randomUUID());

        ModifyQuery modify = Queries.MODIFY();

        modify.insert(iri(Config.AERMOD_AGENT_IRI).isA(service).andHas(hasOperation, operationIri));
		modify.insert(operationIri.isA(operation).andHas(hasHttpUrl, iri(Config.AERMOD_AGENT_URL)).andHas(hasInput, inputIri));
        modify.insert(inputIri.has(hasMandatoryPart, partIri));
        modify.insert(partIri.has(hasType, SIMULATION_TIME)).insert(partIri.has(hasType, NX)).insert(partIri.has(hasType, NY)).insert(partIri.has(hasType, SCOPE))
        .insert(partIri.has(hasType, REPORTING_STATION)).prefix(P_DISP);

        storeClient.executeUpdate(modify.getQueryString());
    }

    String initialiseScopeDerivation(String scopeIri, String weatherStation, int nx, int ny) {
        ModifyQuery modify = Queries.MODIFY();
        modify.insert(iri(scopeIri).isA(SCOPE));

        // sim time (input)
        String simTime = PREFIX + UUID.randomUUID();
        String simTimeMeasure = PREFIX + UUID.randomUUID();
        modify.insert(iri(simTime).isA(SIMULATION_TIME).andHas(HAS_VALUE, iri(simTimeMeasure)));
        modify.insert(iri(simTimeMeasure).isA(MEASURE).andHas(HAS_NUMERICALVALUE, 0));

        // nx (input)
        String nxIri = PREFIX + UUID.randomUUID();
        String nxMeasureIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(nxIri).isA(NX).andHas(HAS_VALUE, iri(nxMeasureIri)));
        modify.insert(iri(nxMeasureIri).isA(MEASURE).andHas(HAS_NUMERICALVALUE, nx));

        // ny (input)
        String nyIri = PREFIX + UUID.randomUUID();
        String nyMeasureIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(nyIri).isA(NY).andHas(HAS_VALUE, iri(nyMeasureIri)));
        modify.insert(iri(nyMeasureIri).isA(MEASURE).andHas(HAS_NUMERICALVALUE, ny));

        // outputs (DispersionMatrix, DispersionLayer, ShipsLayer) as time series
        String matrixIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(matrixIri).isA(DISPERSION_MATRIX));

        String dispLayerIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(dispLayerIri).isA(DISPERSION_LAYER));

        String shipsLayerIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(shipsLayerIri).isA(SHIPS_LAYER));

        modify.prefix(P_DISP,P_OM);
        storeClient.executeUpdate(modify.getQueryString());

        // initialise time series for dispersion matrix
        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            tsClient.initTimeSeries(List.of(matrixIri,dispLayerIri,shipsLayerIri), List.of(String.class, String.class, String.class), null, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Closing connection failed when initialising time series for dispersion matrix");
        }

        List<String> inputs = new ArrayList<>();
        inputs.add(weatherStation);
        inputs.add(simTime);
        inputs.add(scopeIri);
        inputs.add(nxIri);
        inputs.add(nyIri);

        String derivation = derivationClient.createDerivationWithTimeSeries(List.of(matrixIri, dispLayerIri, shipsLayerIri), Config.AERMOD_AGENT_IRI, inputs);
        
        // timestamp for pure inputs
        derivationClient.addTimeInstance(inputs);
        return derivation;
    }

    /**
     * updates all instances of simulation time in the KG, since ships got updates, everything else should be out-of-date
     * @param newValue
     */
    void updateSimulationTime(long newValue) {
        // replace old value
        ModifyQuery modify = Queries.MODIFY();

        SelectQuery query = Queries.SELECT();
        Variable simTime = query.var();
        Variable simTimeMeasure = query.var();
        Variable oldValue = query.var();
        GraphPattern gp = GraphPatterns.and(simTime.isA(SIMULATION_TIME).andHas(HAS_VALUE, simTimeMeasure),
        simTimeMeasure.has(HAS_NUMERICALVALUE, oldValue));

        modify.insert(simTimeMeasure.has(HAS_NUMERICALVALUE, newValue)).delete(simTimeMeasure.has(HAS_NUMERICALVALUE, oldValue)).where(gp).prefix(P_DISP,P_OM);

        storeClient.executeUpdate(modify.getQueryString());

        // get IRI of sim time
        query.where(simTime.isA(SIMULATION_TIME)).prefix(P_DISP);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        List<String> simTimes = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            simTimes.add(queryResult.getJSONObject(i).getString(simTime.getQueryString().substring(1)));
        }
        // update derivation timestamp of simulation time to trigger update
        derivationClient.updateTimestamps(simTimes);
    }
}
