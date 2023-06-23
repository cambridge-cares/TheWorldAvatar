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
import java.util.Collections;
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

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX));
    private static final Prefix P_OM = SparqlBuilder.prefix("om",
            iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));

    // classes
    private static final Iri MEASURE = P_OM.iri("Measure");
    private static final Iri SCOPE = P_DISP.iri("Scope");
    private static final Iri SIMULATION_TIME = P_DISP.iri("SimulationTime");
    private static final Iri NX = P_DISP.iri("nx");
    private static final Iri NY = P_DISP.iri("ny");
    private static final Iri REPORTING_STATION = iri("https://www.theworldavatar.com/kg/ontoems/ReportingStation");
    private static final Iri DISPERSION_OUTPUT = P_DISP.iri("DispersionOutput");
    private static final Iri DISPERSION_MATRIX = P_DISP.iri("DispersionMatrix");
    private static final Iri DISPERSION_LAYER = P_DISP.iri("DispersionLayer");
    private static final Iri DISPERSION_RASTER = P_DISP.iri("DispersionRaster");
    private static final Iri SHIPS_LAYER = P_DISP.iri("ShipsLayer");
    private static final Iri CITIES_NAMESPACE = P_DISP.iri("OntoCityGMLNamespace");
    private static final Iri AERMAP_OUTPUT = P_DISP.iri("AermapOutput");
    private static final Iri EMISSION = P_DISP.iri("Emission");
    // Pollutants
    private static final Iri POLLUTANT_ID = P_DISP.iri("PollutantID");
    private static final Iri NO_X = P_DISP.iri("NOx");
    private static final Iri UHC = P_DISP.iri("uHC");
    private static final Iri CO = P_DISP.iri("CO");
    private static final Iri SO2 = P_DISP.iri("SO2");
    private static final Iri PM10 = P_DISP.iri("PM10");
    private static final Iri PM25 = P_DISP.iri("PM2.5");
    private static final Iri CO2 = P_DISP.iri("CO2");

    // properties
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    private static final Iri HAS_NAME = P_DISP.iri("hasName");
    private static final Iri HAS_DISPERSION_MATRIX = P_DISP.iri("hasDispersionMatrix");
    private static final Iri HAS_DISPERSION_LAYER = P_DISP.iri("hasDispersionLayer");
    private static final Iri HAS_DISPERSION_RASTER = P_DISP.iri("hasDispersionRaster");
    private static final Iri HAS_POLLUTANT_ID = P_DISP.iri("hasPollutantID");

    public QueryClient(RemoteStoreClient storeClient, RemoteRDBStoreClient remoteRDBStoreClient,
            TimeSeriesClient<Long> tsClient) {
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
        modify.insert(operationIri.isA(operation).andHas(hasHttpUrl, iri(Config.AERMOD_AGENT_URL)).andHas(hasInput,
                inputIri));
        modify.insert(inputIri.has(hasMandatoryPart, partIri));

        modify.insert(partIri.has(hasType, CITIES_NAMESPACE));
        modify.insert(partIri.has(hasType, SIMULATION_TIME)).insert(partIri.has(hasType, NX))
                .insert(partIri.has(hasType, NY)).insert(partIri.has(hasType, SCOPE))
                .insert(partIri.has(hasType, REPORTING_STATION)).prefix(P_DISP);

        storeClient.executeUpdate(modify.getQueryString());
    }

    String initialiseScopeDerivation(String scopeIri, String weatherStation, int nx, int ny, String citiesNamespace) {
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

        // cities namespace (optional input)
        String citiesNamespaceIri = PREFIX + UUID.randomUUID();
        if (citiesNamespace != null) {
            modify.insert(iri(citiesNamespaceIri).isA(CITIES_NAMESPACE).andHas(HAS_NAME, citiesNamespace));
        }

        // outputs (DispersionOutput, ShipsLayer, AERMAPOutput) as time series
        // Create columns for one DispersionOutput per pollutant
        List<Iri> pollutantsList = Arrays.asList(CO, CO2, NO_X, PM25, PM10, SO2, UHC);
        List<String> tsList = new ArrayList<>();
        List<String> derivationList = new ArrayList<>();

        pollutantsList.stream().forEach(p -> {
            String dispOutputIri = PREFIX + UUID.randomUUID();
            String pollutantIdIri = PREFIX + UUID.randomUUID();
            modify.insert(iri(dispOutputIri).isA(DISPERSION_OUTPUT).andHas(HAS_POLLUTANT_ID, iri(pollutantIdIri)));
            modify.insert(iri(pollutantIdIri).isA(p));
            derivationList.add(dispOutputIri);

            String dispLayerIri = PREFIX + UUID.randomUUID();
            String dispMatrixIri = PREFIX + UUID.randomUUID();
            String dispRasterIri = PREFIX + UUID.randomUUID();
            modify.insert(iri(dispLayerIri).isA(DISPERSION_LAYER));
            modify.insert(iri(dispOutputIri).has(HAS_DISPERSION_LAYER, iri(dispLayerIri)));
            modify.insert(iri(dispMatrixIri).isA(DISPERSION_MATRIX));
            modify.insert(iri(dispOutputIri).has(HAS_DISPERSION_MATRIX, iri(dispMatrixIri)));
            modify.insert(iri(dispRasterIri).isA(DISPERSION_RASTER));
            modify.insert(iri(dispOutputIri).has(HAS_DISPERSION_RASTER, iri(dispRasterIri)));

            tsList.add(dispLayerIri);
            tsList.add(dispMatrixIri);
            tsList.add(dispRasterIri);

        });

        String shipsLayerIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(shipsLayerIri).isA(SHIPS_LAYER));

        String aermapOutputIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(aermapOutputIri).isA(AERMAP_OUTPUT));

        modify.prefix(P_DISP, P_OM);
        storeClient.executeUpdate(modify.getQueryString());

        // initialise time series for dispersion matrix

        tsList.add(shipsLayerIri);
        tsList.add(aermapOutputIri);
        List<Class<?>> dataClass = Collections.nCopies(tsList.size(), String.class);

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            tsClient.initTimeSeries(tsList, dataClass, null, conn);
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
        if (citiesNamespace != null)
            inputs.add(citiesNamespaceIri);
        derivationList.add(shipsLayerIri);
        derivationList.add(aermapOutputIri);

        String derivation = derivationClient.createDerivationWithTimeSeries(
                derivationList, Config.AERMOD_AGENT_IRI, inputs);

        // timestamp for pure inputs
        derivationClient.addTimeInstance(inputs);
        return derivation;
    }

    /**
     * updates all instances of simulation time in the KG, since ships got updates,
     * everything else should be out-of-date
     * 
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

        modify.insert(simTimeMeasure.has(HAS_NUMERICALVALUE, newValue))
                .delete(simTimeMeasure.has(HAS_NUMERICALVALUE, oldValue)).where(gp).prefix(P_DISP, P_OM);

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
