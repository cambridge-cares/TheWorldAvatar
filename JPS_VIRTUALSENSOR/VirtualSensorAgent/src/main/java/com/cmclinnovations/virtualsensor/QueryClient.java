package com.cmclinnovations.virtualsensor;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.derivation.Derivation;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.postgis.Point;

import com.cmclinnovations.ship.EnvConfig;

/**
 * sends sparql queries
 */
public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);
    private StoreClientInterface storeClient;
    private TimeSeriesClient<Long> tsClient;
    private DerivationClient derivationClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX));
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri(OM_STRING));

    public static final String ONTO_EMS = "https://www.theworldavatar.com/kg/ontoems/";
    public static final Prefix P_EMS = SparqlBuilder.prefix("ontoems", iri(ONTO_EMS));

    // classes
    // as Iri classes for sparql updates sent directly from here
    private static final Iri SHIP = P_DISP.iri("Ship");
    private static final String SPEED_STRING = PREFIX + "Speed";
    private static final Iri SPEED = iri(SPEED_STRING);
    private static final String COURSE_STRING = PREFIX + "CourseOverGround";
    private static final Iri COURSE_OVER_GROUND = iri(COURSE_STRING);
    private static final String MMSI_STRING = PREFIX + "MMSI";
    private static final Iri MMSI = iri(MMSI_STRING);
    private static final String LOCATION_STRING = PREFIX + "Location";
    private static final Iri LOCATION = iri(LOCATION_STRING);
    private static final Iri SHIP_TYPE = P_DISP.iri("ShipType");
    private static final Iri MEASURE = P_DISP.iri("Measure");
    private static final Iri REPORTING_STATION = P_EMS.iri("ReportingStation");
    private static final Iri DISPERSION_OUTPUT = P_DISP.iri("DispersionOutput");
    private static final Iri DISPERSION_MATRIX = P_DISP.iri("DispersionMatrix");
    private static final Iri GEOM = iri("http://www.opengis.net/ont/geosparql#Geometry");

    // properties
    private static final Iri HAS_PROPERTY = P_DISP.iri("hasProperty");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");
    private static final Iri REPORTS = P_EMS.iri("reports");
    private static final Iri isDerivedFrom = iri(DerivationSparql.derivednamespace + "isDerivedFrom");
    private static final Iri belongsTo = iri(DerivationSparql.derivednamespace + "belongsTo");
    private static final Iri HAS_DISPERSION_MATRIX = P_DISP.iri("hasDispersionMatrix");
    private static final Iri HAS_POLLUTANT_ID = P_DISP.iri("hasPollutantID");
    private static final Iri HAS_WKT = iri("http://www.opengis.net/ont/geosparql#asWKT");

    public QueryClient(StoreClientInterface storeClient, TimeSeriesClient<Long> tsClient,
            DerivationClient derivationClient, RemoteRDBStoreClient remoteRDBStoreClient) {
        this.storeClient = storeClient;
        this.tsClient = tsClient;
        this.derivationClient = derivationClient;
        this.remoteRDBStoreClient = remoteRDBStoreClient;
    }

    public Long getLatestStationTime(String derivation) {
        SelectQuery query = Queries.SELECT();
        Variable station = query.var();
        Variable pollutantConcentration = query.var();
        Variable concValue = query.var();

        // Only need to consider one result because all pollutants are stored in a
        // single time series.
        // Number of rows in the time series table should the same for all pollutants.

        query.where(station.isA(REPORTING_STATION).andHas(belongsTo, iri(derivation)),
                station.has(REPORTS, pollutantConcentration),
                pollutantConcentration.isA(MEASURE).andHas(HAS_VALUE, concValue)).prefix(P_DISP, P_OM, P_EMS)
                .select(concValue).limit(1);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        String pollutantConcIri = null;
        for (int i = 0; i < queryResult.length(); i++) {
            pollutantConcIri = queryResult.getJSONObject(i).getString(concValue.getQueryString().substring(1));
        }

        Long latestTime = 0L;

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            latestTime = tsClient.getMaxTime(pollutantConcIri, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return latestTime;

    }

    public Map<String, String> getDispersionMatrixIris(String derivation) {
        // Get data IRIs of dispersion derivations
        SelectQuery query = Queries.SELECT();
        Variable dispersionOutput = query.var();
        Variable pollutantIri = query.var();
        Variable pollutant = query.var();
        Variable dispMatrix = query.var();

        // dispMatrix is the timeseries data IRI of the fileServer URL of the AERMOD
        // concentration output (averageConcentration.dat).
        // There is exactly one such data IRI for each pollutant ID.

        query.where(iri(derivation).has(isDerivedFrom, dispersionOutput),
                dispersionOutput.isA(DISPERSION_OUTPUT).andHas(HAS_POLLUTANT_ID, pollutantIri)
                        .andHas(HAS_DISPERSION_MATRIX, dispMatrix),
                pollutantIri.isA(pollutant),
                dispMatrix.isA(DISPERSION_MATRIX)).prefix(P_DISP, P_OM, P_EMS)
                .select(pollutant, dispMatrix);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        Map<String, String> pollutantToDispMatrix = new HashMap<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String dispMatrixIri = queryResult.getJSONObject(i).getString(dispMatrix.getQueryString().substring(1));
            String pollutantId = queryResult.getJSONObject(i).getString(pollutant.getQueryString().substring(1));
            pollutantToDispMatrix.put(pollutantId, dispMatrixIri);
        }
        return pollutantToDispMatrix;

    }

    public Point getStationLocation(String derivation) {
        SelectQuery query = Queries.SELECT();
        Variable locationWkt = query.var();
        Variable locationIri = query.var();

        query.where(iri(derivation).has(isDerivedFrom, locationIri), locationIri.isA(GEOM).andHas(HAS_WKT, locationWkt))
                .select(locationWkt);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        String locationString = queryResult.getJSONObject(0).getString(locationWkt.getQueryString().substring(1));

        Point location = null;
        try {
            location = new Point(locationString);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return location;

    }

    public void updateStation(Long latestTime, Map<String, String> pollutantToDispMatrix) {

        List<String> dispMatrixIriList = new ArrayList<>(pollutantToDispMatrix.values());
        TimeSeries<Long> dispMatrixTimeSeries = null;

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            dispMatrixTimeSeries = tsClient
                    .getTimeSeriesWithinBounds(dispMatrixIriList, latestTime, null, conn);
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            return;
        }
        for (int i = 0; i < dispMatrixIriList.size(); i++) {
            String dispMatrixIri = dispMatrixIriList.get(i);
            List<String> dispersionMatrixUrls = dispMatrixTimeSeries.getValuesAsString(dispMatrixIri);

        }

    }

    /**
     * executes get request from python-service to postprocess
     */
    public JSONObject getInterpolatedValue(String endPoint, String outputFileURL, int srid, double height) {
        URI httpGet;
        try {
            URIBuilder builder = new URIBuilder(endPoint);
            builder.setParameter("dispersionMatrix", outputFileURL);
            builder.setParameter("srid", String.valueOf(srid));
            builder.setParameter("height", String.valueOf(height));

            httpGet = builder.build();
        } catch (URISyntaxException e) {
            LOGGER.error("Failed at building URL");
            throw new RuntimeException(e);
        }

        try (CloseableHttpClient httpClient = HttpClients.createDefault();
                CloseableHttpResponse httpResponse = httpClient.execute(new HttpGet(httpGet))) {
            String result = EntityUtils.toString(httpResponse.getEntity());
            return new JSONObject(result);
        } catch (IOException e) {
            LOGGER.error("Failed at making connection with python service");
            throw new RuntimeException(e);
        } catch (JSONException e) {
            LOGGER.error("Failed to parse result from python service for aermod geojson");
            LOGGER.error(outputFileURL);
            throw new RuntimeException(e);
        }
    }

    /**
     * returns false if there's nothing in the KG
     * 
     * @return
     */
    boolean initialised() {
        boolean result = false;
        SelectQuery query = Queries.SELECT();
        Variable ship = query.var();
        query.where(ship.isA(SHIP)).prefix(P_DISP).limit(1);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        if (!queryResult.isEmpty()) {
            result = true;
        }
        return result;
    }

    /**
     * first query existing instances with MMSI values, then initialise if do not
     * exist
     * 
     * @param ships
     */
    List<Ship> initialiseShipsIfNotExist(List<Ship> ships) {
        SelectQuery query = Queries.SELECT();

        Variable mmsi = query.var();
        Variable mmsiValue = query.var();
        ValuesPattern<Integer> vp = new ValuesPattern<>(mmsiValue,
                ships.stream().map(Ship::getMmsi).collect(Collectors.toList()), Integer.class);

        GraphPattern gp = mmsi.isA(MMSI).andHas(PropertyPaths.path(HAS_VALUE, HAS_NUMERICALVALUE), mmsiValue);

        query.where(gp, vp).prefix(P_OM, P_DISP).select(mmsiValue);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        List<Integer> initialisedShipMMSI = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            initialisedShipMMSI.add(queryResult.getJSONObject(i).getInt(mmsiValue.getQueryString().substring(1)));
        }

        List<Ship> newShipsToInitialise = new ArrayList<>();
        for (Ship ship : ships) {
            if (!initialisedShipMMSI.contains(ship.getMmsi())) {
                newShipsToInitialise.add(ship);
            }
        }
        createShip(newShipsToInitialise);
        return newShipsToInitialise;
    }

    /**
     * called by initialiseShipsIfNotExist, adds triples and initialises the time
     * series tables
     * 
     * @param ships
     */
    private void createShip(List<Ship> ships) {
        // for bulk time series initialisation
        List<List<String>> dataIRIs = new ArrayList<>();
        List<List<Class<?>>> dataClasses = new ArrayList<>();
        List<String> timeUnit = new ArrayList<>();

        if (!ships.isEmpty()) {
            // ship IRI list to add derivation timestamps
            List<String> shipIriList = new ArrayList<>();
            // triples
            ModifyQuery modify = Queries.MODIFY();
            for (Ship ship : ships) {
                List<String> dataWithTimeSeries = new ArrayList<>();
                List<Class<?>> classes = new ArrayList<>();

                String shipName = "Ship" + ship.getMmsi();
                Iri shipIri = P_DISP.iri(shipName);
                String shipIriString = PREFIX + shipName;
                shipIriList.add(shipIriString);
                ship.setIri(shipIriString);
                modify.insert(shipIri.isA(SHIP));

                // mmsi
                Iri mmsiProperty = P_DISP.iri(shipName + "MMSI");
                Iri mmsiMeasure = P_DISP.iri(shipName + "MMSIMeasure");

                modify.insert(shipIri.has(HAS_PROPERTY, mmsiProperty));
                modify.insert(mmsiProperty.isA(MMSI).andHas(HAS_VALUE, mmsiMeasure));
                modify.insert(mmsiMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getMmsi()));

                // ship type (integer)
                Iri shipTypeProperty = P_DISP.iri(shipName + "ShipType");
                Iri shipTypeMeasure = P_DISP.iri(shipName + "ShipTypeMeasure");

                modify.insert(shipIri.has(HAS_PROPERTY, shipTypeProperty));
                modify.insert(shipTypeProperty.isA(SHIP_TYPE).andHas(HAS_VALUE, shipTypeMeasure));
                modify.insert(shipTypeMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getShipType()));

                // Location time series
                Iri locationProperty = P_DISP.iri(shipName + "Location");
                String locationMeasure = PREFIX + shipName + "LocationMeasure";
                modify.insert(shipIri.has(HAS_PROPERTY, locationProperty));
                modify.insert(locationProperty.isA(LOCATION).andHas(HAS_VALUE, iri(locationMeasure)));
                modify.insert(iri(locationMeasure).isA(MEASURE));

                dataWithTimeSeries.add(locationMeasure);
                classes.add(Point.class);

                // speed time series
                Iri speedProperty = P_DISP.iri(shipName + "Speed");
                String speedMeasure = PREFIX + shipName + "SpeedMeasure";
                modify.insert(shipIri.has(HAS_PROPERTY, speedProperty));
                modify.insert(speedProperty.isA(SPEED).andHas(HAS_VALUE, iri(speedMeasure)));
                modify.insert(iri(speedMeasure).isA(MEASURE));

                dataWithTimeSeries.add(speedMeasure);
                classes.add(Integer.class);

                // course time series
                Iri courseProperty = P_DISP.iri(shipName + "Course");
                String courseMeasure = PREFIX + shipName + "CourseMeasure";
                modify.insert(shipIri.has(HAS_PROPERTY, courseProperty));
                modify.insert(courseProperty.isA(COURSE_OVER_GROUND).andHas(HAS_VALUE, iri(courseMeasure)));
                modify.insert(iri(courseMeasure).isA(MEASURE));

                dataWithTimeSeries.add(courseMeasure);
                classes.add(Integer.class);

                dataIRIs.add(dataWithTimeSeries);
                dataClasses.add(classes);
                timeUnit.add("Unix timestamp");
            }
            modify.prefix(P_OM, P_DISP);
            storeClient.executeUpdate(modify.getQueryString());

            // add timestamps for derivation framework
            derivationClient.addTimeInstance(shipIriList);

            // time series in rdb, 4326 is the srid
            try (Connection conn = remoteRDBStoreClient.getConnection()) {
                tsClient.bulkInitTimeSeries(dataIRIs, dataClasses, timeUnit, 4326, conn);
            } catch (SQLException e) {
                LOGGER.error(e.getMessage());
            }
        }
    }

    /**
     * get ship IRI from kg based on MMSI
     * 
     * @param ships
     */
    void setShipIRIs(List<Ship> ships) {
        SelectQuery query = Queries.SELECT();

        Variable ship = query.var();
        Variable mmsiValue = query.var();
        Variable property = query.var();
        Variable measure = query.var();

        ValuesPattern<Integer> vpMmsi = new ValuesPattern<>(mmsiValue,
                ships.stream().map(Ship::getMmsi).collect(Collectors.toList()), Integer.class);

        GraphPattern gp = GraphPatterns.and(ship.has(HAS_PROPERTY, property),
                property.isA(MMSI).andHas(HAS_VALUE, measure), measure.has(HAS_NUMERICALVALUE, mmsiValue));

        query.prefix(P_OM, P_DISP).where(gp, vpMmsi).select(ship, mmsiValue).distinct();

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        // look up map used later to assign ship Iris
        Map<Integer, Ship> mmsiToShipMap = new HashMap<>();
        ships.stream().forEach(s -> mmsiToShipMap.put(s.getMmsi(), s));

        for (int i = 0; i < queryResult.length(); i++) {
            String shipIri = queryResult.getJSONObject(i).getString(ship.getQueryString().substring(1));

            int mmsi = queryResult.getJSONObject(i).getInt(mmsiValue.getQueryString().substring(1));
            // obtain ship object from map and set IRI
            mmsiToShipMap.get(mmsi).setIri(shipIri);
        }
    }

    void setMeasureIri(List<Ship> ships) {
        SelectQuery query = Queries.SELECT();

        Variable ship = query.var();
        Variable property = query.var();
        Variable propertyType = query.var();
        Variable measure = query.var();

        ValuesPattern<Iri> shipValues = new ValuesPattern<>(ship,
                ships.stream().map(s -> iri(s.getIri())).collect(Collectors.toList()), Iri.class);

        GraphPattern gp = GraphPatterns.and(ship.has(HAS_PROPERTY, property),
                property.isA(propertyType).andHas(HAS_VALUE, measure));

        query.prefix(P_OM, P_DISP).where(gp, shipValues).select(ship, property, propertyType, measure).distinct();

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        // look up map to obtain ship object
        Map<String, Ship> shipIriToShipMap = new HashMap<>();
        ships.stream().forEach(s -> shipIriToShipMap.put(s.getIri(), s));

        for (int i = 0; i < queryResult.length(); i++) {
            String shipIri = queryResult.getJSONObject(i).getString(ship.getQueryString().substring(1));
            String propertyTypeIri = queryResult.getJSONObject(i).getString(propertyType.getQueryString().substring(1));

            Ship shipObject = shipIriToShipMap.get(shipIri);

            switch (propertyTypeIri) {
                case LOCATION_STRING:
                    shipObject.setLocationMeasureIri(
                            queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1)));
                    break;
                case SPEED_STRING:
                    shipObject.setSpeedMeasureIri(
                            queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1)));
                    break;
                case COURSE_STRING:
                    shipObject.setCourseMeasureIri(
                            queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1)));
                    break;
                default:
                    break;
            }
        }
    }

    void updateTimeSeriesData(List<Ship> ships) {
        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            ships.stream().forEach(ship -> {
                List<String> dataIRIs = Arrays.asList(ship.getCourseMeasureIri(), ship.getSpeedMeasureIri(),
                        ship.getLocationMeasureIri());

                // order of dataIRIs is course, speed, location, as defined in the previous loop
                List<List<?>> values = new ArrayList<>();
                values.add(Arrays.asList(ship.getCourse()));
                values.add(Arrays.asList(ship.getSpeed()));
                values.add(Arrays.asList(ship.getLocation()));

                List<Long> time = Arrays.asList(ship.getTimestamp().getEpochSecond());

                TimeSeries<Long> ts = new TimeSeries<>(time, dataIRIs, values);
                tsClient.addTimeSeriesData(ts, conn);
            });
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        derivationClient.updateTimestamps(ships.stream().map(Ship::getIri).collect(Collectors.toList()));
    }

    /**
     * adds the OntoAgent instance
     */
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

        modify.insert(iri(EnvConfig.EMISSIONS_AGENT_IRI).isA(service).andHas(hasOperation, operationIri));
        modify.insert(operationIri.isA(operation).andHas(hasHttpUrl, iri(EnvConfig.EMISSIONS_AGENT_URL))
                .andHas(hasInput, inputIri));
        modify.insert(inputIri.has(hasMandatoryPart, partIri));
        modify.insert(partIri.has(hasType, SHIP)).prefix(P_DISP);

        storeClient.executeUpdate(modify.getQueryString());
    }

    /**
     * 
     * @param ships
     */
    void createNewDerivations(List<Ship> ships) {
        if (Boolean.parseBoolean(EnvConfig.PARALLELISE_CALCULATIONS)) {
            CompletableFuture<Derivation> getAsync = null;

            for (Ship ship : ships) {
                getAsync = CompletableFuture.supplyAsync(() -> {
                    Derivation derivation = null;
                    try {
                        derivation = derivationClient.createSyncDerivationForNewInfo(EnvConfig.EMISSIONS_AGENT_IRI,
                                Arrays.asList(ship.getIri()), DerivationSparql.ONTODERIVATION_DERIVATION);
                    } catch (Exception e) {
                        LOGGER.error(e.getMessage());
                        LOGGER.error("Failed to create new derivation for {}", ship.getIri());
                    }
                    return derivation;
                });
            }

            if (getAsync != null) {
                getAsync.join();
            }
        } else {
            for (Ship ship : ships) {
                try {
                    derivationClient.createSyncDerivationForNewInfo(EnvConfig.EMISSIONS_AGENT_IRI,
                            Arrays.asList(ship.getIri()), DerivationSparql.ONTODERIVATION_DERIVATION);
                } catch (Exception e) {
                    LOGGER.error(e.getMessage());
                    LOGGER.error("Failed to create new derivation for {}", ship.getIri());
                }
            }
        }
    }
}
