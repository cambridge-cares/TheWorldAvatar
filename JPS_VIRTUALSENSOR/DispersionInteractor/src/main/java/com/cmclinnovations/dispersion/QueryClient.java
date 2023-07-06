package com.cmclinnovations.dispersion;

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
import org.postgis.Point;

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
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import it.unibz.inf.ontop.model.vocabulary.GEO;
import it.unibz.inf.ontop.model.vocabulary.GEOF;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
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
    private TimeSeriesClient<Instant> tsClientInstant;
    private RemoteRDBStoreClient remoteRDBStoreClient;
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX));
    private static final Prefix P_OM = SparqlBuilder.prefix("om",
            iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
    public static final String ONTO_EMS = "https://www.theworldavatar.com/kg/ontoems/";
    public static final Prefix P_EMS = SparqlBuilder.prefix("ontoems", iri(ONTO_EMS));
    private static final Prefix P_GEO = SparqlBuilder.prefix("geo", iri(GEO.PREFIX));
    private static final Prefix P_GEOF = SparqlBuilder.prefix("geof", iri(GEOF.PREFIX));

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
    private static final Iri GEOM = iri("http://www.opengis.net/ont/geosparql#Geometry");

    private static final Iri NO_X = P_DISP.iri("NOx");
    private static final Iri UHC = P_DISP.iri("uHC");
    private static final Iri CO = P_DISP.iri("CO");
    private static final Iri SO2 = P_DISP.iri("SO2");
    private static final Iri PM10 = P_DISP.iri("PM10");
    private static final Iri PM25 = P_DISP.iri("PM2.5");
    private static final Iri CO2 = P_DISP.iri("CO2");
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
    private static final Iri HAS_DISPERSION_LAYER = P_DISP.iri("hasDispersionLayer");
    private static final Iri HAS_DISPERSION_RASTER = P_DISP.iri("hasDispersionRaster");
    private static final Iri HAS_POLLUTANT_ID = P_DISP.iri("hasPollutantID");
    private static final Iri AS_WKT = iri("http://www.opengis.net/ont/geosparql#asWKT");
    private static final Iri OBSERVATION_LOCATION = P_EMS.iri("hasObservationLocation");
    private static final Iri REPORTS = P_EMS.iri("reports");
    private static final Iri HAS_UNIT = P_OM.iri("hasUnit");

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
        String virtualSensorUpdateUrl = "http://ship-stack-virtual-sensor-agent:8080/VirtualSensorAgent/";
        modify.insert(iri(virtualSensorUpdateIri).isA(service).andHas(hasOperation, operationIri));
        modify.insert(operationIri.isA(operation).andHas(hasHttpUrl, iri(virtualSensorUpdateUrl)).andHas(hasInput,
                inputIri));
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
                "<http://www.opengis.net/def/crs/EPSG/0/4326> " + vsLocation.toString(),
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

    void initializeVirtualSensors(List<String> scopeIriList, List<Point> virtualSensorLocations) {
        // Get data IRIs of dispersion outputs belonging to the input derivation
        Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
        Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");
        String virtualSensorUpdateIri = Config.VIRTUAL_SENSOR_AGENT_IRI;

        // create a JSONObject that represents a GeoJSON Feature Collection
        JSONObject featureCollection = new JSONObject();
        featureCollection.put("type", "FeatureCollection");
        JSONArray features = new JSONArray();

        String sparqlEndpoint = new EndpointConfig().getKgurl();
        ModifyQuery modify = Queries.MODIFY();

        try (Connection conn = remoteRDBStoreClient.getConnection()) {

            for (int i = 0; i < virtualSensorLocations.size(); i++) {

                String scopeIri = scopeIriList.get(i);
                Point location = virtualSensorLocations.get(i);

                SelectQuery query = Queries.SELECT();
                Variable entity = query.var();
                Variable derivation = query.var();
                query.where(iri(scopeIri).isA(SCOPE), derivation.has(isDerivedFrom, iri(scopeIri)),
                        entity.isA(DISPERSION_OUTPUT).andHas(belongsTo, derivation))
                        .prefix(P_DISP)
                        .select(entity).distinct();
                JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

                List<String> dispersionOutputs = new ArrayList<>();

                for (int j = 0; j < queryResult.length(); j++) {
                    String dispOutput = queryResult.getJSONObject(j).getString(entity.getQueryString().substring(1));
                    dispersionOutputs.add(dispOutput);
                }

                List<String> inputs = new ArrayList<>(dispersionOutputs);

                // output (OntoEMS reporting station)
                String stationIri = ONTO_EMS + "virtualsensor_" + UUID.randomUUID();
                Iri station = iri(stationIri);

                // Update triples for station in blazegraph
                String locationIri = PREFIX + UUID.randomUUID();
                modify.insert(station.isA(REPORTING_STATION));

                List<String> dataListForTimeSeries = new ArrayList<>();

                // triples to insert for each station
                // NOx
                Iri quantity = P_EMS.iri("quantity_" + UUID.randomUUID());
                String measure = ONTO_EMS + "measure_" + UUID.randomUUID();
                modify.insert(station.has(REPORTS, quantity));
                modify.insert(quantity.isA(NO_X_CONC).andHas(HAS_VALUE, iri(measure)));
                modify.insert(iri(measure).isA(MEASURE).andHas(HAS_UNIT, UNIT_POLLUTANT_CONC));
                dataListForTimeSeries.add(measure);

                // UHC
                quantity = P_EMS.iri("quantity_" + UUID.randomUUID());
                measure = ONTO_EMS + "measure_" + UUID.randomUUID();
                modify.insert(station.has(REPORTS, quantity));
                modify.insert(quantity.isA(UHC_CONC).andHas(HAS_VALUE, iri(measure)));
                modify.insert(iri(measure).isA(MEASURE).andHas(HAS_UNIT, UNIT_POLLUTANT_CONC));
                dataListForTimeSeries.add(measure);

                // CO
                quantity = P_EMS.iri("quantity_" + UUID.randomUUID());
                measure = ONTO_EMS + "measure_" + UUID.randomUUID();
                modify.insert(station.has(REPORTS, quantity));
                modify.insert(quantity.isA(CO_CONC).andHas(HAS_VALUE, iri(measure)));
                modify.insert(iri(measure).isA(MEASURE).andHas(HAS_UNIT, UNIT_POLLUTANT_CONC));
                dataListForTimeSeries.add(measure);

                // CO2
                quantity = P_EMS.iri("quantity_" + UUID.randomUUID());
                measure = ONTO_EMS + "measure_" + UUID.randomUUID();
                modify.insert(station.has(REPORTS, quantity));
                modify.insert(quantity.isA(CO2_CONC).andHas(HAS_VALUE, iri(measure)));
                modify.insert(iri(measure).isA(MEASURE).andHas(HAS_UNIT, UNIT_POLLUTANT_CONC));
                dataListForTimeSeries.add(measure);

                // SO2
                quantity = P_EMS.iri("quantity_" + UUID.randomUUID());
                measure = ONTO_EMS + "measure_" + UUID.randomUUID();
                modify.insert(station.has(REPORTS, quantity));
                modify.insert(quantity.isA(SO2_CONC).andHas(HAS_VALUE, iri(measure)));
                modify.insert(iri(measure).isA(MEASURE).andHas(HAS_UNIT, UNIT_POLLUTANT_CONC));
                dataListForTimeSeries.add(measure);

                // PM10
                quantity = P_EMS.iri("quantity_" + UUID.randomUUID());
                measure = ONTO_EMS + "measure_" + UUID.randomUUID();
                modify.insert(station.has(REPORTS, quantity));
                modify.insert(quantity.isA(PM10_CONC).andHas(HAS_VALUE, iri(measure)));
                modify.insert(iri(measure).isA(MEASURE).andHas(HAS_UNIT, UNIT_POLLUTANT_CONC));
                dataListForTimeSeries.add(measure);

                // PM2.5
                quantity = P_EMS.iri("quantity_" + UUID.randomUUID());
                measure = ONTO_EMS + "measure_" + UUID.randomUUID();
                modify.insert(station.has(REPORTS, quantity));
                modify.insert(quantity.isA(PM25_CONC).andHas(HAS_VALUE, iri(measure)));
                modify.insert(iri(measure).isA(MEASURE).andHas(HAS_UNIT, UNIT_POLLUTANT_CONC));
                dataListForTimeSeries.add(measure);

                // Create timeseries of pollutant concentrations for OntoEMS station
                List<Class<?>> classListForTimeSeries = Collections.nCopies(dataListForTimeSeries.size(), Double.class);
                tsClientInstant.initTimeSeries(dataListForTimeSeries, classListForTimeSeries,
                        null, conn);

                // Create derivation for each virtual sensor
                String sensorDerivation = derivationClient.createDerivationWithTimeSeries(
                        List.of(stationIri), virtualSensorUpdateIri, inputs);
                derivationClient.addTimeInstance(inputs);

                // Create POSTGIS and GeoServer feature for this OntoEMS station
                JSONObject geometry = new JSONObject();
                geometry.put("type", "Point");
                List<Double> coordinate = Arrays.asList(location.x, location.y);
                geometry.put("coordinates", new JSONArray(coordinate));
                JSONObject feature = new JSONObject();
                feature.put("type", "Feature");
                feature.put("geometry", geometry);
                feature.put("iri", stationIri);
                feature.put("name", "VirtualSensor_" + (i + 1));
                feature.put("endpoint", sparqlEndpoint);
                feature.put("scope_iri", scopeIri);
                feature.put("geom_iri", locationIri);
                features.put(feature);

            }
        } catch (SQLException e) {
            LOGGER.error("SQL exception when updating virtual sensor data.");
            LOGGER.error(e.getMessage());
        }

        modify.prefix(P_DISP, P_OM, P_EMS);
        storeClient.executeUpdate(modify.getQueryString());
        // Upload virtual sensor layer to POSTGIS and GeoServer for visualization
        // purposes

        featureCollection.put("features", features);

        LOGGER.info("Uploading virtual sensors GeoJSON to PostGIS");
        GDALClient gdalclient = GDALClient.getInstance();
        gdalclient.uploadVectorStringToPostGIS(Config.DATABASE, Config.SENSORS_TABLE_NAME, featureCollection.toString(),
                new Ogr2OgrOptions(), true);
        // GeoServer layer creation is handled in the agent that creates the data.json
        // file. That agent will query for sensors that lie within the scope for which
        // the visualization is being created.
        LOGGER.info("Creating virtual sensors layer in Geoserver");
        GeoServerClient geoserverclient = GeoServerClient.getInstance();
        geoserverclient.createWorkspace(Config.GEOSERVER_WORKSPACE);
        geoserverclient.createPostGISLayer(Config.GEOSERVER_WORKSPACE, Config.DATABASE, Config.SENSORS_TABLE_NAME,
                new GeoServerVectorSettings());
    }

    public List<String> getVirtualSensorDerivations(String dispersionDerivation) {
        List<String> derivationList = new ArrayList<>();
        List<String> dispOutputList = new ArrayList<>();

        Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
        Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");

        SelectQuery query = Queries.SELECT();

        Variable derivation = query.var();
        Variable dispOutput = query.var();
        Variable station = query.var();

        query.where(dispOutput.has(belongsTo, iri(dispersionDerivation)),
                derivation.has(isDerivedFrom, dispOutput), dispOutput.isA(DISPERSION_OUTPUT),
                station.isA(REPORTING_STATION).andHas(belongsTo, derivation)).select(derivation, dispOutput)
                .prefix(P_DISP, P_OM, P_EMS);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        for (int i = 0; i < queryResult.length(); i++) {
            String derivationIri = queryResult.getJSONObject(i).getString(derivation.getQueryString().substring(1));
            derivationList.add(derivationIri);
            String dispOutputIri = queryResult.getJSONObject(i).getString(dispOutput.getQueryString().substring(1));
            dispOutputList.add(dispOutputIri);
        }

        // update derivation timestamp of dispersion output to trigger update of virtual
        // sensor derivations contained in derivationList
        derivationClient.updateTimestamps(dispOutputList);

        return derivationList;

    }

}
