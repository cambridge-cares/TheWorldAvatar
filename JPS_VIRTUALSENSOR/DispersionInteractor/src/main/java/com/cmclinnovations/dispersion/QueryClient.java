package com.cmclinnovations.dispersion;

import org.eclipse.rdf4j.model.vocabulary.GEO;
import org.eclipse.rdf4j.model.vocabulary.GEOF;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.model.vocabulary.TIME;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Operand;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.QueryElement;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.geom.Polygon;
import org.postgis.Point;
import org.apache.jena.geosparql.implementation.parsers.wkt.WKTReader;

import com.cmclinnovations.dispersion.sparqlbuilder.GeoSPARQL;
import com.cmclinnovations.dispersion.sparqlbuilder.ServiceEndpoint;
import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * sends sparql queries
 */
public class QueryClient {
    private StoreClientInterface storeClient;
    private DerivationClient derivationClient;
    private TimeSeriesClient<Long> tsClient;
    private TimeSeriesClient<Instant> tsClientInstant;
    private RemoteRDBStoreClient remoteRDBStoreClient;
    private String ontopUrl;
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX));
    private static final Prefix P_OM = SparqlBuilder.prefix("om",
            iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
    public static final String ONTO_EMS = "https://www.theworldavatar.com/kg/ontoems/";
    public static final Prefix P_EMS = SparqlBuilder.prefix("ontoems", iri(ONTO_EMS));
    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.NAMESPACE));
    private static final Prefix P_GEOF = SparqlBuilder.prefix("geof", iri(GEOF.NAMESPACE));
    private static final Prefix P_TIME = SparqlBuilder.prefix("time", iri(TIME.NAMESPACE));

    // classes
    private static final Iri MEASURE = P_OM.iri("Measure");
    private static final Iri SCOPE = P_DISP.iri("Scope");
    private static final Iri SIMULATION_TIME = P_DISP.iri("SimulationTime");
    private static final Iri NX = P_DISP.iri("nx");
    private static final Iri NY = P_DISP.iri("ny");
    private static final Iri Z = P_DISP.iri("z");
    private static final Iri REPORTING_STATION = iri("https://www.theworldavatar.com/kg/ontoems/ReportingStation");
    private static final Iri DISPERSION_OUTPUT = P_DISP.iri("DispersionOutput");
    private static final Iri DISPERSION_MATRIX = P_DISP.iri("DispersionMatrix");
    private static final Iri DISPERSION_RASTER = P_DISP.iri("DispersionRaster");
    private static final Iri DISPERSION_COLOUR_BAR = P_DISP.iri("DispersionColourBar");
    private static final Iri SHIPS_LAYER = P_DISP.iri("ShipsLayer");
    private static final Iri BUILDINGS_LAYER = P_DISP.iri("BuildingsLayer");
    private static final Iri CITIES_NAMESPACE = P_DISP.iri("OntoCityGMLNamespace");
    private static final Iri STATIC_POINT_SOURCE_LAYER = P_DISP.iri("StaticPointSourceLayer");
    private static final Iri ELEVATION_LAYER = P_DISP.iri("Elevation");

    private static final String NO_X = PREFIX + "NOx";
    private static final String UHC = PREFIX + "uHC";
    private static final String CO = PREFIX + "CO";
    private static final String SO2 = PREFIX + "SO2";
    private static final String PM10 = PREFIX + "PM10";
    private static final String PM25 = PREFIX + "PM2.5";
    private static final String CO2 = PREFIX + "CO2";

    // Pollutant concentrations and units
    private static final Iri NO_X_CONC = P_EMS.iri("NitrogenOxidesConcentration");
    private static final Iri UHC_CONC = P_EMS.iri("UhcConcentration");// Currently not in ONTOEMS
    private static final Iri CO_CONC = P_EMS.iri("CarbonMonoxideConcentration");
    private static final Iri CO2_CONC = P_EMS.iri("CarbonDioxideConcentration");
    private static final Iri SO2_CONC = P_EMS.iri("SulfurDioxideConcentration");
    private static final Iri PM10_CONC = P_EMS.iri("PM10Concentration");
    private static final Iri PM25_CONC = P_EMS.iri("PM2.5Concentration");
    private static final Iri UNIT_POLLUTANT_CONC = P_OM.iri("microgramPerCubicmetre");

    // properties
    private static final Iri HAS_GEOMETRY = P_GEO.iri("hasGeometry");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    private static final Iri HAS_NAME = P_DISP.iri("hasName");
    private static final Iri HAS_DISPERSION_MATRIX = P_DISP.iri("hasDispersionMatrix");
    private static final Iri HAS_DISPERSION_RASTER = P_DISP.iri("hasDispersionRaster");
    private static final Iri HAS_DISPERSION_COLOUR_BAR = P_DISP.iri("hasDispersionColourBar");
    private static final Iri HAS_POLLUTANT_ID = P_DISP.iri("hasPollutantID");
    private static final Iri AS_WKT = iri("http://www.opengis.net/ont/geosparql#asWKT");
    private static final Iri REPORTS = P_EMS.iri("reports");
    private static final Iri HAS_UNIT = P_OM.iri("hasUnit");
    private static final Iri HAS_HEIGHT = P_DISP.iri("hasHeight");

    public QueryClient(RemoteStoreClient storeClient, RemoteRDBStoreClient remoteRDBStoreClient,
            TimeSeriesClient<Long> tsClient, TimeSeriesClient<Instant> tsClientInstant) {
        this.storeClient = storeClient;
        this.derivationClient = new DerivationClient(storeClient, PREFIX);
        this.remoteRDBStoreClient = remoteRDBStoreClient;
        this.tsClient = tsClient;
        this.tsClientInstant = tsClientInstant;

    }

    public QueryClient(RemoteStoreClient storeClient, RemoteRDBStoreClient remoteRDBStoreClient,
            TimeSeriesClient<Long> tsClient) {
        this.storeClient = storeClient;
        this.derivationClient = new DerivationClient(storeClient, PREFIX);
        this.remoteRDBStoreClient = remoteRDBStoreClient;
        this.tsClient = tsClient;
    }

    public void setOntopUrl(String ontopUrl) {
        this.ontopUrl = ontopUrl;
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
                .insert(partIri.has(hasType, REPORTING_STATION))
                .insert(partIri.has(hasType, Z)).prefix(P_DISP);

        storeClient.executeUpdate(modify.getQueryString());
    }

    String initialiseScopeDerivation(String scopeIri, String scopeLabel, String weatherStation, int nx, int ny,
            String citiesNamespace, List<Integer> zList, String simulationTimeIri) {
        ModifyQuery modify = Queries.MODIFY();
        modify.insert(iri(scopeIri).isA(SCOPE).andHas(iri(RDFS.LABEL), scopeLabel));

        // sim time (input)
        String simTime = null;
        if (simulationTimeIri == null) {
            simTime = PREFIX + UUID.randomUUID();
            String simTimePosition = PREFIX + UUID.randomUUID();
            modify.insert(iri(simTime).isA(SIMULATION_TIME).andHas(iri(TIME.IN_TIME_POSITION), iri(simTimePosition)));
            modify.insert(iri(simTimePosition).isA(iri(TIME.TIME_POSITION)).andHas(iri(TIME.NUMERIC_POSITION), 0)
                    .andHas(iri(TIME.HAS_TRS), "https://dbpedia.org/page/Unix_time"));
        } else {
            simTime = simulationTimeIri;
        }

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

        // z (input)
        List<String> zIriList = new ArrayList<>();
        zList.stream().forEach(zValue -> {
            String zIri = PREFIX + UUID.randomUUID();
            zIriList.add(zIri);
            String zMeasureIri = PREFIX + UUID.randomUUID();
            modify.insert(iri(zIri).isA(Z).andHas(HAS_VALUE, iri(zMeasureIri)));
            modify.insert(iri(zMeasureIri).isA(MEASURE).andHas(HAS_NUMERICALVALUE, zValue));
        });

        // cities namespace (optional input)
        String citiesNamespaceIri = PREFIX + UUID.randomUUID();
        if (citiesNamespace != null) {
            modify.insert(iri(citiesNamespaceIri).isA(CITIES_NAMESPACE).andHas(HAS_NAME, citiesNamespace));
        }

        // outputs (DispersionOutput, ShipsLayer, StaticPointSourceLayer) as time series
        // Create columns for one DispersionOutput per pollutant
        List<Iri> pollutantsList = List.of(CO, CO2, NO_X, PM25, PM10, SO2, UHC).stream().map(Rdf::iri)
                .collect(Collectors.toList());
        List<String> tsList = new ArrayList<>();
        List<Class<?>> dataClass = new ArrayList<>();
        List<String> entityList = new ArrayList<>();

        pollutantsList.stream().forEach(p -> zIriList.stream().forEach(z -> {
            String dispOutputIri = PREFIX + UUID.randomUUID();
            String pollutantIdIri = PREFIX + UUID.randomUUID();
            modify.insert(iri(dispOutputIri).isA(DISPERSION_OUTPUT).andHas(HAS_POLLUTANT_ID, iri(pollutantIdIri))
                    .andHas(HAS_HEIGHT, iri(z)));
            modify.insert(iri(pollutantIdIri).isA(p));
            entityList.add(dispOutputIri);

            String dispMatrixIri = PREFIX + UUID.randomUUID();
            String dispRasterIri = PREFIX + UUID.randomUUID();
            String dispColourBar = PREFIX + UUID.randomUUID();

            modify.insert(iri(dispMatrixIri).isA(DISPERSION_MATRIX));
            modify.insert(iri(dispOutputIri).has(HAS_DISPERSION_MATRIX, iri(dispMatrixIri)));
            modify.insert(iri(dispRasterIri).isA(DISPERSION_RASTER));
            modify.insert(iri(dispOutputIri).has(HAS_DISPERSION_RASTER, iri(dispRasterIri)));
            modify.insert(iri(dispColourBar).isA(DISPERSION_COLOUR_BAR));
            modify.insert(iri(dispOutputIri).has(HAS_DISPERSION_COLOUR_BAR, iri(dispColourBar)));

            tsList.add(dispMatrixIri);
            tsList.add(dispRasterIri);
            tsList.add(dispColourBar);
            dataClass.add(String.class);
            dataClass.add(String.class);
            dataClass.add(String.class);
        }));

        String shipsLayerIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(shipsLayerIri).isA(SHIPS_LAYER));

        String buildingsLayerIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(buildingsLayerIri).isA(BUILDINGS_LAYER));

        String staticPointSourceLayerIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(staticPointSourceLayerIri).isA(STATIC_POINT_SOURCE_LAYER));

        String elevationLayerIri = PREFIX + UUID.randomUUID();
        modify.insert(iri(elevationLayerIri).isA(ELEVATION_LAYER));

        modify.prefix(P_DISP, P_OM);
        storeClient.executeUpdate(modify.getQueryString());

        entityList.add(shipsLayerIri);
        entityList.add(buildingsLayerIri);
        entityList.add(staticPointSourceLayerIri);
        entityList.add(elevationLayerIri);

        // initialise time series for dispersion matrix
        tsList.add(shipsLayerIri);
        tsList.add(staticPointSourceLayerIri);
        tsList.add(buildingsLayerIri);
        tsList.add(elevationLayerIri);
        dataClass.add(Boolean.class);
        dataClass.add(Boolean.class);
        dataClass.add(Boolean.class);
        dataClass.add(Boolean.class);

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
        zIriList.forEach(inputs::add);
        if (citiesNamespace != null)
            inputs.add(citiesNamespaceIri);

        String derivation = derivationClient.createDerivationWithTimeSeries(
                entityList, Config.AERMOD_AGENT_IRI, inputs);

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
        GraphPattern gp = GraphPatterns.and(
                simTime.isA(SIMULATION_TIME).andHas(iri(TIME.IN_TIME_POSITION), simTimeMeasure),
                simTimeMeasure.has(iri(TIME.NUMERIC_POSITION), oldValue));

        modify.insert(simTimeMeasure.has(iri(TIME.NUMERIC_POSITION), newValue))
                .delete(simTimeMeasure.has(iri(TIME.NUMERIC_POSITION), oldValue)).where(gp).prefix(P_DISP, P_OM);

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

    void updateSimulationTime(String derivation, long newValue) {
        Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
        ModifyQuery modify = Queries.MODIFY();

        SelectQuery query = Queries.SELECT();
        Variable simTime = query.var();
        Variable simTimeMeasure = query.var();
        Variable oldValue = query.var();
        GraphPattern gp = GraphPatterns.and(iri(derivation).has(isDerivedFrom, simTime),
                simTime.isA(SIMULATION_TIME).andHas(iri(TIME.IN_TIME_POSITION), simTimeMeasure),
                simTimeMeasure.has(iri(TIME.NUMERIC_POSITION), oldValue));

        modify.insert(simTimeMeasure.has(iri(TIME.NUMERIC_POSITION), newValue))
                .delete(simTimeMeasure.has(iri(TIME.NUMERIC_POSITION), oldValue)).where(gp).prefix(P_DISP, P_OM);

        storeClient.executeUpdate(modify.getQueryString());

        // get IRI of sim time
        query.where(iri(derivation).has(isDerivedFrom, simTime), simTime.isA(SIMULATION_TIME)).prefix(P_DISP);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        List<String> simTimes = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            simTimes.add(queryResult.getJSONObject(i).getString(simTime.getQueryString().substring(1)));
        }
        // update derivation timestamp of simulation time to trigger update
        derivationClient.updateTimestamps(simTimes);
    }

    void initialiseVirtualSensorAgent() {

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
        // This next line needs to be added the ontodispersion TBox.
        String virtualSensorUpdateIri = Config.VIRTUAL_SENSOR_AGENT_IRI;
        modify.insert(iri(virtualSensorUpdateIri).isA(service).andHas(hasOperation, operationIri));
        modify.insert(operationIri.isA(operation).andHas(hasHttpUrl, iri(Config.VIRTUAL_SENSOR_AGENT_URL))
                .andHas(hasInput, inputIri));
        modify.insert(inputIri.has(hasMandatoryPart, partIri));
        modify.insert(partIri.has(hasType, DISPERSION_OUTPUT));
        modify.prefix(P_DISP, P_OM, P_EMS);
        storeClient.executeUpdate(modify.getQueryString());
    }

    List<String> getScopesIncludingPoint(Point vsLocation) {
        SelectQuery query = Queries.SELECT();
        Variable scopeIri = query.var();
        Variable scopeWkt = query.var();

        QueryElement location = Rdf.literalOfType(
                "<http://www.opengis.net/def/crs/OGC/1.3/CRS84> " + vsLocation.toString(),
                iri(org.eclipse.rdf4j.model.vocabulary.GEO.WKT_LITERAL));

        Operand sfWithin = GeoSPARQL.sfWithin(location, scopeWkt);
        GraphPattern gp = GraphPatterns.and(scopeIri.has(PropertyPaths.path(HAS_GEOMETRY, AS_WKT), scopeWkt))
                .filter(Expressions.and(sfWithin));
        ServiceEndpoint serviceEndpoint = new ServiceEndpoint(new EndpointConfig().getOntopUrl());
        GraphPattern gp2 = serviceEndpoint.service(gp);
        GraphPattern gp3 = GraphPatterns.and(scopeIri.isA(SCOPE));

        query.where(gp3, gp2).select(scopeIri).prefix(P_GEO, P_GEOF, P_DISP);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        List<String> scopeIriList = new ArrayList<>();

        for (int i = 0; i < queryResult.length(); i++) {
            scopeIriList.add(queryResult.getJSONObject(i).getString(scopeIri.getQueryString().substring(1)));
        }

        return scopeIriList;

    }

    void initialiseVirtualSensors(List<String> scopeIriList, Point virtualSensorLocation,
            List<String> pollutants, Connection conn) {
        // Get data IRIs of dispersion outputs belonging to the input derivation
        Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
        Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");
        String virtualSensorUpdateIri = Config.VIRTUAL_SENSOR_AGENT_IRI;

        String sparqlEndpoint = new EndpointConfig().getKgurl();
        ModifyQuery modify = Queries.MODIFY();

        Map<String, Iri> pollutantToConcMap = new HashMap<>();
        pollutantToConcMap.put(CO2, CO2_CONC);
        pollutantToConcMap.put(CO, CO_CONC);
        pollutantToConcMap.put(NO_X, NO_X_CONC);
        pollutantToConcMap.put(PM25, PM25_CONC);
        pollutantToConcMap.put(PM10, PM10_CONC);
        pollutantToConcMap.put(SO2, SO2_CONC);
        pollutantToConcMap.put(UHC, UHC_CONC);

        String scopeIri = scopeIriList.get(0);

        SelectQuery query = Queries.SELECT();
        Variable entity = query.var();
        Variable derivation = query.var();
        Variable heightVar = query.var();
        query.where(iri(scopeIri).isA(SCOPE), derivation.has(isDerivedFrom, iri(scopeIri)),
                entity.isA(DISPERSION_OUTPUT).andHas(belongsTo, derivation)
                        .andHas(PropertyPaths.path(HAS_HEIGHT, HAS_VALUE, HAS_NUMERICALVALUE), heightVar))
                .prefix(P_DISP, P_OM)
                .select(entity, heightVar).distinct();
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        // one output per pollutant
        Map<Double, List<String>> heightToDispOutputMap = new HashMap<>();
        for (int j = 0; j < queryResult.length(); j++) {
            double height = queryResult.getJSONObject(j).getDouble(heightVar.getQueryString().substring(1));
            String dispOutput = queryResult.getJSONObject(j).getString(entity.getQueryString().substring(1));
            heightToDispOutputMap.computeIfAbsent(height, k -> new ArrayList<>());
            heightToDispOutputMap.get(height).add(dispOutput);
        }

        // get dispersion output at lowest height
        double minHeight = heightToDispOutputMap.keySet().stream().mapToDouble(d -> d).min().getAsDouble();
        List<String> inputs = heightToDispOutputMap.get(minHeight);

        // output (OntoEMS reporting station)
        String mainUuid = "virtualsensor_" + UUID.randomUUID();
        String stationIri = ONTO_EMS + mainUuid;
        Iri station = iri(stationIri);

        // Update triples for station in blazegraph
        modify.insert(station.isA(REPORTING_STATION));

        List<String> dataListForTimeSeries = new ArrayList<>();

        // triples to insert for each station
        pollutants.forEach(pollutant -> {
            Iri quantity = P_EMS.iri("quantity_" + UUID.randomUUID());
            String measure = ONTO_EMS + "measure_" + UUID.randomUUID();
            modify.insert(station.has(REPORTS, quantity));
            modify.insert(quantity.isA(pollutantToConcMap.get(pollutant)).andHas(HAS_VALUE, iri(measure)));
            modify.insert(iri(measure).isA(MEASURE).andHas(HAS_UNIT, UNIT_POLLUTANT_CONC));
            dataListForTimeSeries.add(measure);
        });

        // Create timeseries of pollutant concentrations for OntoEMS station
        List<Class<?>> classListForTimeSeries = Collections.nCopies(dataListForTimeSeries.size(), Double.class);
        tsClientInstant.initTimeSeries(dataListForTimeSeries, classListForTimeSeries, null, conn);

        // Create derivation for each virtual sensor
        derivationClient.createDerivationWithTimeSeries(
                List.of(stationIri), virtualSensorUpdateIri, inputs);

        // Create POSTGIS and GeoServer feature for this OntoEMS station
        JSONObject geometry = new JSONObject();
        geometry.put("type", "Point");
        geometry.put("coordinates", new JSONArray(List.of(virtualSensorLocation.x, virtualSensorLocation.y)));
        JSONObject feature = new JSONObject();
        feature.put("type", "Feature");
        feature.put("geometry", geometry);
        feature.put("iri", stationIri);
        feature.put("main_uuid", mainUuid);
        feature.put("name", String.format("Virtual sensor at %.2f m", minHeight));
        feature.put("endpoint", sparqlEndpoint);
        feature.put("geom_uuid", UUID.randomUUID());

        modify.prefix(P_DISP, P_OM, P_EMS);
        storeClient.executeUpdate(modify.getQueryString());

        LOGGER.info("Uploading virtual sensors GeoJSON to PostGIS");
        GDALClient gdalclient = GDALClient.getInstance();
        gdalclient.uploadVectorStringToPostGIS(Config.DATABASE, Config.SENSORS_TABLE_NAME, feature.toString(),
                new Ogr2OgrOptions(), true);

        LOGGER.info("Creating virtual sensors layer in Geoserver");
        GeoServerClient geoserverclient = GeoServerClient.getInstance();
        geoserverclient.createWorkspace(Config.GEOSERVER_WORKSPACE);
        geoserverclient.createPostGISLayer(Config.GEOSERVER_WORKSPACE, Config.DATABASE, Config.SENSORS_TABLE_NAME,
                new GeoServerVectorSettings());
    }

    public List<String> getVirtualSensorDerivations(String dispersionDerivation) {
        List<String> derivationList = new ArrayList<>();

        Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
        Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");

        SelectQuery query = Queries.SELECT();

        Variable derivation = query.var();
        Variable dispOutput = query.var();
        Variable station = query.var();

        query.where(dispOutput.has(belongsTo, iri(dispersionDerivation)),
                derivation.has(isDerivedFrom, dispOutput), dispOutput.isA(DISPERSION_OUTPUT),
                station.isA(REPORTING_STATION).andHas(belongsTo, derivation)).select(derivation).distinct()
                .prefix(P_DISP, P_OM, P_EMS);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        for (int i = 0; i < queryResult.length(); i++) {
            String derivationIri = queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1));
            derivationList.add(derivationIri);
        }

        return derivationList;
    }

    public List<DispersionSimulation> getDispersionSimulations() {
        Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
        Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");

        SelectQuery query = Queries.SELECT();

        Variable derivation = query.var();
        Variable scope = query.var();
        Variable dispersionOutput = query.var();
        Variable scopelabelVar = query.var();
        Variable pollutantType = query.var();
        Variable pollutantLabel = query.var();
        Variable dispRasterVar = query.var();
        Variable weatherStationVar = query.var();
        Variable scopeWktVar = query.var();
        Variable weatherStationWktVar = query.var();
        Variable zVar = query.var();
        Variable zValueVar = query.var();

        ServiceEndpoint ontopEndpoint = new ServiceEndpoint(ontopUrl);

        query.where(derivation.has(isDerivedFrom, scope), scope.isA(SCOPE).andHas(iri(RDFS.LABEL), scopelabelVar),
                derivation.has(isDerivedFrom, weatherStationVar), weatherStationVar.isA(REPORTING_STATION),
                dispersionOutput.isA(DISPERSION_OUTPUT).andHas(belongsTo, derivation)
                        .andHas(HAS_DISPERSION_RASTER, dispRasterVar)
                        .andHas(PropertyPaths.path(HAS_POLLUTANT_ID, iri(RDF.TYPE)), pollutantType)
                        .andHas(HAS_HEIGHT, zVar),
                zVar.has(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE), zValueVar),
                pollutantType.has(RDFS.LABEL, pollutantLabel).optional(),
                ontopEndpoint.service(scope.has(PropertyPaths.path(HAS_GEOMETRY, iri(GEO.AS_WKT)), scopeWktVar),
                        weatherStationVar.has(PropertyPaths.path(HAS_GEOMETRY, iri(GEO.AS_WKT)), weatherStationWktVar)))
                .prefix(P_DISP, P_GEO, P_OM);

        Map<String, DispersionSimulation> iriToDispSimMap = new HashMap<>();
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        for (int i = 0; i < queryResult.length(); i++) {
            String derivationIri = queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1));

            iriToDispSimMap.computeIfAbsent(derivationIri, DispersionSimulation::new);

            String scopeLabel = queryResult.getJSONObject(i).getString(scopelabelVar.getQueryString().substring(1));
            String pol = queryResult.getJSONObject(i).getString(pollutantType.getQueryString().substring(1));
            String dispRaster = queryResult.getJSONObject(i).getString(dispRasterVar.getQueryString().substring(1));

            String zIri = queryResult.getJSONObject(i).getString(zVar.getQueryString().substring(1));
            int zValue = queryResult.getJSONObject(i).getInt(zValueVar.getQueryString().substring(1));

            DispersionSimulation dispersionSimulation = iriToDispSimMap.get(derivationIri);
            dispersionSimulation.setScopeLabel(scopeLabel);
            dispersionSimulation.addZValue(zValue, zIri);

            // if pollutant type does not have the label attached (requires uploading of
            // tbox, use IRI as the user facing label)
            String polLabel;
            if (queryResult.getJSONObject(i).has(pollutantLabel.getQueryString().substring(1))) {
                polLabel = queryResult.getJSONObject(i).getString(pollutantLabel.getQueryString().substring(1));
            } else {
                polLabel = pol;
            }

            dispersionSimulation.setPollutantLabelAndDispRaster(pol, polLabel, dispRaster);

            String wktLiteral = queryResult.getJSONObject(i).getString(scopeWktVar.getQueryString().substring(1));
            org.locationtech.jts.geom.Geometry scopePolygon = WKTReader.extract(wktLiteral).getGeometry();
            scopePolygon.setSRID(4326);

            dispersionSimulation.setScopePolygon((Polygon) scopePolygon);

            String weatherStation = queryResult.getJSONObject(i)
                    .getString(weatherStationVar.getQueryString().substring(1));
            dispersionSimulation.setWeatherStationIri(weatherStation);

            String weatherWkt = queryResult.getJSONObject(i)
                    .getString(weatherStationWktVar.getQueryString().substring(1));
            org.locationtech.jts.geom.Geometry weatherLocation = WKTReader.extract(weatherWkt).getGeometry();
            dispersionSimulation.setWeatherStationLocation((org.locationtech.jts.geom.Point) weatherLocation);
        }

        return new ArrayList<>(iriToDispSimMap.values());
    }

    /**
     * by checking for null in the time series columns
     * 
     * @param dispersionSimulations
     */
    public void removeNonExistentPollutantsAndSetSimTimes(List<DispersionSimulation> dispersionSimulations,
            Connection conn) {
        dispersionSimulations.forEach(dispersionSimulation -> {
            List<String> pollutants = dispersionSimulation.getPollutants();
            List<String> dispersionLayers = pollutants.stream().map(dispersionSimulation::getDispersionRaster)
                    .collect(Collectors.toList());

            Long latestTime = tsClient.getMaxTime(dispersionLayers.get(0), conn);

            pollutants.forEach(pollutant -> {
                String dispersionRaster = dispersionSimulation.getDispersionRaster(pollutant);
                if (latestTime == null) {
                    dispersionSimulation.removePollutant(pollutant);
                } else {
                    TimeSeries<Long> latestTimeSeries = tsClient.getTimeSeriesWithinBounds(dispersionLayers, latestTime,
                            latestTime, conn);
                    if (latestTimeSeries.getValues(dispersionRaster).get(0) == null) {
                        dispersionSimulation.removePollutant(pollutant);
                    }
                }
            });

            // set timesteps
            List<Long> simulationTimes;
            if (latestTime != null) {
                String dispersionRaster = dispersionSimulation
                        .getDispersionRaster(dispersionSimulation.getPollutants().get(0));
                simulationTimes = tsClient.getTimeSeries(List.of(dispersionRaster), conn).getTimes();
            } else {
                simulationTimes = new ArrayList<>();
            }

            dispersionSimulation.setTimesteps(simulationTimes);
        });
    }

    /**
     * index in output - 0) dispersion, 1) ships, 2) buildings, 3) static point
     * source
     * 
     * @param pollutant
     * @param timeStep
     * @param derivation
     * @return
     */
    public List<Boolean> getLayers(long timeStep, String derivation, Connection conn) {
        Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");

        SelectQuery query = Queries.SELECT();

        Variable shipLayer = query.var();
        Variable buildingsLayer = query.var();
        Variable staticPointLayer = query.var();
        Variable elevationLayer = query.var();

        query.where(shipLayer.has(belongsTo, iri(derivation)).andIsA(SHIPS_LAYER),
                buildingsLayer.has(belongsTo, iri(derivation)).andIsA(BUILDINGS_LAYER),
                staticPointLayer.has(belongsTo, iri(derivation)).andIsA(STATIC_POINT_SOURCE_LAYER),
                elevationLayer.has(belongsTo, iri(derivation)).andIsA(ELEVATION_LAYER))
                .select(shipLayer, buildingsLayer, staticPointLayer, elevationLayer)
                .prefix(P_DISP);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        String shipLayerIri = queryResult.getJSONObject(0).getString(shipLayer.getQueryString().substring(1));
        String buildingsLayerIri = queryResult.getJSONObject(0).getString(buildingsLayer.getQueryString().substring(1));
        String staticPointLayerIri = queryResult.getJSONObject(0)
                .getString(staticPointLayer.getQueryString().substring(1));
        String elevationLayerIri = queryResult.getJSONObject(0).getString(elevationLayer.getQueryString().substring(1));

        TimeSeries<Long> queriedTimeSeries = tsClient.getTimeSeriesWithinBounds(
                List.of(shipLayerIri, buildingsLayerIri, staticPointLayerIri, elevationLayerIri),
                timeStep, timeStep, conn);

        boolean hasShips = (Boolean) queriedTimeSeries.getValues(shipLayerIri).get(0);
        boolean hasBuildings = (Boolean) queriedTimeSeries.getValues(buildingsLayerIri).get(0);
        boolean hasStaticSources = (Boolean) queriedTimeSeries.getValues(staticPointLayerIri).get(0);
        boolean hasElevation = (Boolean) queriedTimeSeries.getValues(elevationLayerIri).get(0);

        List<Boolean> layers = new ArrayList<>();
        layers.add(hasShips);
        layers.add(hasBuildings);
        layers.add(hasStaticSources);
        layers.add(hasElevation);

        return layers;
    }

    public String getColourBarURL(String pollutant, long timeStep, String derivation, String zIri) {
        Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");

        SelectQuery query = Queries.SELECT();

        Variable dispOutput = query.var();
        Variable dispColourBar = query.var();

        query.where(dispOutput.has(belongsTo, iri(derivation)),
                dispOutput.has(PropertyPaths.path(HAS_POLLUTANT_ID, iri(RDF.TYPE)), iri(pollutant))
                        .andHas(HAS_DISPERSION_COLOUR_BAR, dispColourBar)
                        .andHas(HAS_HEIGHT, iri(zIri)))
                .select(dispColourBar).prefix(P_DISP);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        String dispColourBarIri = queryResult.getJSONObject(0).getString(dispColourBar.getQueryString().substring(1));

        String dispColourBarUrl = null;

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            TimeSeries<Long> queriedTimeSeries = tsClient.getTimeSeriesWithinBounds(List.of(dispColourBarIri),
                    timeStep, timeStep, conn);
            dispColourBarUrl = queriedTimeSeries.getValuesAsString(dispColourBarIri).get(0);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Closing connection failed when retrieving the dispersion layer name");
        }

        return dispColourBarUrl;
    }

    String getDerivationWithScope(String scope) {
        Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
        SelectQuery query = Queries.SELECT();

        Variable derivationVar = query.var();

        query.where(derivationVar.has(isDerivedFrom, iri(scope)));

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        return queryResult.getJSONObject(0).getString(derivationVar.getQueryString().substring(1));
    }
}
