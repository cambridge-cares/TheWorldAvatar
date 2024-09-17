package com.cmclinnovations.ship;

import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.postgis.Point;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.sql.Connection;
import java.sql.SQLException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.postgis.Point;

/**
 * sends sparql queries
 */
public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);
    private StoreClientInterface storeClient;
    private TimeSeriesClient<Instant> tsClient;
    private DerivationClient derivationClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    static final String PREFIX = "https://www.theworldavatar.com/kg/ontodispersion/";
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp", iri(PREFIX));
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final Prefix P_OM = SparqlBuilder.prefix("om", iri(OM_STRING));
    private static final Prefix P_DERIVATION = SparqlBuilder.prefix("der",
            iri("https://www.theworldavatar.com/kg/ontoderivation/"));

    // classes
    // as Iri classes for sparql updates sent directly from here
    private static final Iri SHIP = P_DISP.iri("Ship");
    private static final Iri SIMULATION_TIME = P_DISP.iri("SimulationTime");
    private static final String SPEED_STRING = PREFIX + "Speed";
    private static final Iri SPEED = iri(SPEED_STRING);
    private static final String COURSE_STRING = PREFIX + "CourseOverGround";
    private static final Iri COURSE_OVER_GROUND = iri(COURSE_STRING);
    private static final String MMSI_STRING = PREFIX + "MMSI";
    private static final Iri MMSI = iri(MMSI_STRING);
    private static final String LOCATION_STRING = PREFIX + "Location";
    private static final Iri LOCATION = iri(LOCATION_STRING);
    private static final String LATITUDE_STRING = PREFIX + "Latitude";
    private static final Iri LATITUDE = iri(LATITUDE_STRING);
    private static final String LONGITUDE_STRING = PREFIX + "Longitude";
    private static final Iri LONGITUDE = iri(LONGITUDE_STRING);
    private static final Iri SHIP_TYPE = P_DISP.iri("ShipType");
    private static final Iri DRAUGHT = P_DISP.iri("Draught");
    private static final Iri DIMENSION = P_DISP.iri("Dimension");
    private static final Iri IMO_NUMBER = P_DISP.iri("IMONumber");
    private static final Iri CALLSIGN = P_DISP.iri("CallSign");
    private static final Iri MEASURE = P_OM.iri("Measure");

    // properties
    private static final Iri HAS_PROPERTY = P_DISP.iri("hasProperty");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");

    public QueryClient(StoreClientInterface storeClient, TimeSeriesClient<Instant> tsClient,
            DerivationClient derivationClient, RemoteRDBStoreClient remoteRDBStoreClient) {
        this.storeClient = storeClient;
        this.tsClient = tsClient;
        this.derivationClient = derivationClient;
        this.remoteRDBStoreClient = remoteRDBStoreClient;
    }

    public RemoteRDBStoreClient getRemoteRDBStoreClient() {
        return remoteRDBStoreClient;
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

        List<Integer> initialisedShipMMSI = findIfShipsExist(ships);

        List<Ship> newShipsToInitialise = new ArrayList<>();
        for (Ship ship : ships) {
            if (!initialisedShipMMSI.contains(ship.getMmsi())) {
                newShipsToInitialise.add(ship);
            }
        }
        createShip(newShipsToInitialise);
        return newShipsToInitialise;
    }

    List<Integer> findIfShipsExist(List<Ship> ships) {
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
        return initialisedShipMMSI;
    }

    boolean shipExists() {
        SelectQuery query = Queries.SELECT();

        Variable ship = query.var();

        GraphPattern gp = ship.isA(SHIP);

        query.where(gp).prefix(P_DISP).limit(1);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        return queryResult.length() > 0;
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

                modify.insert(shipIri.has(iri(RDFS.LABEL), ship.getShipName()));

                modify.insert(shipIri.has(HAS_PROPERTY, mmsiProperty));
                modify.insert(mmsiProperty.isA(MMSI).andHas(HAS_VALUE, mmsiMeasure));
                modify.insert(mmsiMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getMmsi()));

                // ship type (integer)
                Iri shipTypeProperty = P_DISP.iri(shipName + "ShipType");
                Iri shipTypeMeasure = P_DISP.iri(shipName + "ShipTypeMeasure");

                modify.insert(shipIri.has(HAS_PROPERTY, shipTypeProperty));
                modify.insert(shipTypeProperty.isA(SHIP_TYPE).andHas(HAS_VALUE, shipTypeMeasure));
                modify.insert(shipTypeMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getShipType()));

                // draught
                if (ship.getDraught() > 0) {
                    Iri draughtProperty = P_DISP.iri(shipName + "Draught");
                    Iri draughtMeasure = P_DISP.iri(shipName + "DraughtMeasure");
                    modify.insert(shipIri.has(HAS_PROPERTY, draughtProperty));
                    modify.insert(draughtProperty.isA(DRAUGHT).andHas(HAS_VALUE, draughtMeasure));
                    modify.insert(draughtMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getDraught()));
                }

                // dimension
                if (!ship.getDimension().isEmpty()) {
                    Iri dimensionProperty = P_DISP.iri(shipName + "Dimension");
                    Iri dimensionMeasure = P_DISP.iri(shipName + "DimensionMeasure");
                    modify.insert(shipIri.has(HAS_PROPERTY, dimensionProperty));
                    modify.insert(dimensionProperty.isA(DIMENSION).andHas(HAS_VALUE, dimensionMeasure));
                    modify.insert(
                            dimensionMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getDimension().toString()));
                }

                // IMO number
                if (ship.getImoNumber() != 0) {
                    Iri imoProperty = P_DISP.iri(shipName + "IMO");
                    Iri imoMeasure = P_DISP.iri(shipName + "IMOMeasure");
                    modify.insert(shipIri.has(HAS_PROPERTY, imoProperty));
                    modify.insert(imoProperty.isA(IMO_NUMBER).andHas(HAS_VALUE, imoMeasure));
                    modify.insert(imoMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getImoNumber()));
                }

                // callsign
                if (!ship.getCallSign().isEmpty()) {
                    Iri callSignProperty = P_DISP.iri(shipName + "CallSign");
                    Iri callSignMeasure = P_DISP.iri(shipName + "CallSignMeasure");
                    modify.insert(shipIri.has(HAS_PROPERTY, callSignProperty));
                    modify.insert(callSignProperty.isA(CALLSIGN).andHas(HAS_VALUE, callSignMeasure));
                    modify.insert(callSignMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getCallSign()));
                }

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
                classes.add(Double.class);

                // course time series
                Iri courseProperty = P_DISP.iri(shipName + "Course");
                String courseMeasure = PREFIX + shipName + "CourseMeasure";
                modify.insert(shipIri.has(HAS_PROPERTY, courseProperty));
                modify.insert(courseProperty.isA(COURSE_OVER_GROUND).andHas(HAS_VALUE, iri(courseMeasure)));
                modify.insert(iri(courseMeasure).isA(MEASURE));

                dataWithTimeSeries.add(courseMeasure);
                classes.add(Double.class);

                // lat time series
                Iri latProperty = P_DISP.iri(shipName + "Lat");
                String latMeasure = PREFIX + shipName + "LatMeasure";
                modify.insert(shipIri.has(HAS_PROPERTY, latProperty));
                modify.insert(latProperty.isA(LATITUDE).andHas(HAS_VALUE, iri(latMeasure)));
                modify.insert(iri(latMeasure).isA(MEASURE));

                dataWithTimeSeries.add(latMeasure);
                classes.add(Double.class);

                // lon time series
                Iri lonProperty = P_DISP.iri(shipName + "Lon");
                String lonMeasure = PREFIX + shipName + "LonMeasure";
                modify.insert(shipIri.has(HAS_PROPERTY, lonProperty));
                modify.insert(lonProperty.isA(LONGITUDE).andHas(HAS_VALUE, iri(lonMeasure)));
                modify.insert(iri(lonMeasure).isA(MEASURE));

                dataWithTimeSeries.add(lonMeasure);
                classes.add(Double.class);

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
                case LATITUDE_STRING:
                    shipObject.setLatMeasureIri(
                            queryResult.getJSONObject(i).getString(measure.getQueryString().substring(1)));
                    break;
                case LONGITUDE_STRING:
                    shipObject.setLonMeasureIri(
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
                        ship.getLocationMeasureIri(), ship.getLatMeasureIri(), ship.getLonMeasureIri());

                // order of dataIRIs is course, speed, location, as defined in the previous loop
                List<List<?>> values = new ArrayList<>();
                List<Instant> time;
                if (ship.hasTimeSeries() && !ship.getTimestampList().isEmpty()) {
                    // data from aisstream.io, only save last data point to avoid having too much
                    // data
                    int tsSize = ship.getTimestampList().size();
                    time = Arrays.asList(ship.getTimestampList().get(tsSize - 1));
                    values.add(Arrays.asList(ship.getCogList().get(tsSize - 1)));
                    values.add(Arrays.asList(ship.getSpeedList().get(tsSize - 1)));
                    values.add(Arrays.asList(ship.getLocationList().get(tsSize - 1)));
                    values.add(Arrays.asList(ship.getLatList().get(tsSize - 1)));
                    values.add(Arrays.asList(ship.getLonList().get(tsSize - 1)));
                } else {
                    time = Arrays.asList(ship.getTimestamp());
                    values.add(Arrays.asList(ship.getCourse()));
                    values.add(Arrays.asList(ship.getSpeed()));
                    values.add(Arrays.asList(ship.getLocation()));
                    values.add(Arrays.asList(ship.getLat()));
                    values.add(Arrays.asList(ship.getLon()));
                }
                TimeSeries<Instant> ts = new TimeSeries<>(time, dataIRIs, values);
                tsClient.addTimeSeriesData(ts, conn);
            });
        } catch (SQLException e) {
            LOGGER.error("Error adding time series for ship");
            LOGGER.error(e.getMessage());
        }

        derivationClient.updateTimestamps(ships.stream().map(Ship::getIri).collect(Collectors.toList()));
    }

    void bulkUpdateTimeSeriesData(List<Ship> ships) {
        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            for (Ship ship : ships) {
                List<String> dataIRIs = Arrays.asList(ship.getCourseMeasureIri(), ship.getSpeedMeasureIri(),
                        ship.getLocationMeasureIri(), ship.getLatMeasureIri(), ship.getLonMeasureIri());
                List<List<?>> values = new ArrayList<>();
                // query relational database for data
                String sqlQuery = String.format(
                        "SELECT to_char(\"BaseDateTime\" at time zone 'UTC', 'YYYY-MM-DD\"T\"HH24:MI:SS') AS \"DATE\", " +
                                "\"LAT\", \"LON\", \"SOG\", \"COG\" FROM \"ship\" WHERE \"MMSI\" = %s",
                        ship.getMmsi());
                JSONArray tsData = remoteRDBStoreClient.executeQuery(sqlQuery);
                List<Instant> time = IntStream
                        .range(0, tsData.length()).mapToObj(i -> LocalDateTime
                                .parse(tsData.getJSONObject(i).getString("DATE")).toInstant(ZoneOffset.UTC))
                        .collect(Collectors.toList());
                values.add(IntStream.range(0, tsData.length()).mapToObj(i -> tsData.getJSONObject(i).getDouble("COG"))
                        .collect(Collectors.toList())); // course
                values.add(IntStream.range(0, tsData.length()).mapToObj(i -> tsData.getJSONObject(i).getDouble("SOG"))
                        .collect(Collectors.toList())); // speed
                List<Double> listLat = IntStream.range(0, tsData.length())
                        .mapToObj(i -> tsData.getJSONObject(i).getDouble("LAT"))
                        .collect(Collectors.toList());
                List<Double> listLon = IntStream.range(0, tsData.length())
                        .mapToObj(i -> tsData.getJSONObject(i).getDouble("LON"))
                        .collect(Collectors.toList());
                // construct location
                List<Point> listLocation = new ArrayList<>();
                for (int i = 0; i < tsData.length(); i++) {
                    Point point = new Point(listLon.get(i), listLat.get(i));
                    point.setSrid(4326);
                    listLocation.add(point);
                }
                values.add(listLocation); // location
                values.add(listLat); // lat
                values.add(listLon); // lon
                // add data to time series
                TimeSeries<Instant> ts = new TimeSeries<>(time, dataIRIs, values);
                tsClient.addTimeSeriesData(ts, conn);
            }
        } catch (SQLException e) {
            LOGGER.error("Error adding time series for ship");
            LOGGER.error(e.getMessage());
        }

        derivationClient.updateTimestamps(ships.stream().map(Ship::getIri).collect(Collectors.toList()));
    }

    /**
     * adds the OntoAgent instance for emissions agent and ship data agent
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
        modify.insert(partIri.has(hasType, SHIP).andHas(hasType, SIMULATION_TIME)).prefix(P_DISP);

        // ship data agent
        Iri operationIri2 = iri(PREFIX + "ship_data_operation");
        Iri inputIri2 = iri(PREFIX + "ship_data_input");
        Iri partIri2 = iri(PREFIX + "ship_data_mandatory_part");

        modify.insert(iri(EnvConfig.SHIP_DATA_AGENT_IRI).isA(service).andHas(hasOperation, operationIri2));
        modify.insert(operationIri2.isA(operation).andHas(hasHttpUrl, iri(EnvConfig.SHIP_DATA_AGENT_URL))
                .andHas(hasInput, inputIri2));
        modify.insert(inputIri2.has(hasMandatoryPart, partIri2));
        modify.insert(partIri2.has(hasType, SHIP));

        storeClient.executeUpdate(modify.getQueryString());
    }

    /**
     * 
     * @param ships
     */
    void createNewDerivations(List<Ship> ships) {
        if (Boolean.parseBoolean(EnvConfig.PARALLELISE_CALCULATIONS)) {
            ships.parallelStream()
                    .forEach(ship -> {
                        derivationClient.createSyncDerivationForNewInfo(EnvConfig.EMISSIONS_AGENT_IRI,
                                Arrays.asList(ship.getIri()), DerivationSparql.ONTODERIVATION_DERIVATION);
                        derivationClient.createSyncDerivationForNewInfo(EnvConfig.SHIP_DATA_AGENT_IRI,
                                Arrays.asList(ship.getIri()), DerivationSparql.ONTODERIVATION_DERIVATION);
                    });
        } else {
            for (Ship ship : ships) {
                try {
                    derivationClient.createSyncDerivationForNewInfo(EnvConfig.EMISSIONS_AGENT_IRI,
                            Arrays.asList(ship.getIri()), DerivationSparql.ONTODERIVATION_DERIVATION);
                    derivationClient.createSyncDerivationForNewInfo(EnvConfig.SHIP_DATA_AGENT_IRI,
                            Arrays.asList(ship.getIri()), DerivationSparql.ONTODERIVATION_DERIVATION);
                } catch (Exception e) {
                    LOGGER.error(e.getMessage());
                    LOGGER.error("Failed to create new derivation for {}", ship.getIri());
                }
            }
        }
    }

    /**
     * update value of ship type if previous value is 0
     * 
     * @param ships
     */
    void updateShipType(List<Ship> shipsWithShipType) {
        Map<String, Ship> iriToShipMap = new HashMap<>();
        shipsWithShipType.forEach(s -> iriToShipMap.put(s.getIri(), s));

        SelectQuery query = Queries.SELECT();

        Variable shipVar = query.var();
        Variable shipPropertyVar = query.var();
        Variable shipPropertyValueVar = query.var();

        ValuesPattern<Iri> valuesPattern = new ValuesPattern<>(shipVar,
                shipsWithShipType.stream().map(s -> iri(s.getIri())).collect(Collectors.toList()), Iri.class);

        GraphPattern queryPattern = GraphPatterns.and(shipVar.has(HAS_PROPERTY, shipPropertyVar),
                shipPropertyVar.isA(SHIP_TYPE).andHas(HAS_VALUE, shipPropertyValueVar),
                shipPropertyValueVar.has(HAS_NUMERICALVALUE, 0));

        query.where(queryPattern, valuesPattern).prefix(P_DISP, P_OM);

        // only ships with previous value of shiptype = 0 will appear here
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        Map<String, Integer> valueIriToValueMap = new HashMap<>();
        List<String> shipIriList = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            String shipIri = queryResult.getJSONObject(i).getString(shipVar.getQueryString().substring(1));
            String valueIri = queryResult.getJSONObject(i)
                    .getString(shipPropertyValueVar.getQueryString().substring(1));

            valueIriToValueMap.put(valueIri, iriToShipMap.get(shipIri).getShipType());
            shipIriList.add(shipIri);
        }

        if (!valueIriToValueMap.isEmpty()) {
            ModifyQuery modify = Queries.MODIFY();
            valueIriToValueMap.entrySet().forEach(entry -> modify.delete(iri(entry.getKey()).has(HAS_NUMERICALVALUE, 0))
                    .insert(iri(entry.getKey()).has(HAS_NUMERICALVALUE, entry.getValue())));
            modify.prefix(P_OM, P_DISP);

            // this is designed for aisstream, ship type comes together in the same message
            // as the following data
            shipIriList.forEach(shipIri -> {
                Ship ship = iriToShipMap.get(shipIri);
                String shipName = "Ship" + ship.getMmsi();

                Iri draughtProperty = P_DISP.iri(shipName + "Draught");
                Iri draughtMeasure = P_DISP.iri(shipName + "DraughtMeasure");
                modify.insert(iri(shipIri).has(HAS_PROPERTY, draughtProperty));
                modify.insert(draughtProperty.isA(DRAUGHT).andHas(HAS_VALUE, draughtMeasure));
                modify.insert(draughtMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getDraught()));

                Iri dimensionProperty = P_DISP.iri(shipName + "Dimension");
                Iri dimensionMeasure = P_DISP.iri(shipName + "DimensionMeasure");
                modify.insert(iri(shipIri).has(HAS_PROPERTY, dimensionProperty));
                modify.insert(dimensionProperty.isA(DIMENSION).andHas(HAS_VALUE, dimensionMeasure));
                modify.insert(
                        dimensionMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getDimension().toString()));

                Iri imoProperty = P_DISP.iri(shipName + "IMO");
                Iri imoMeasure = P_DISP.iri(shipName + "IMOMeasure");
                modify.insert(iri(shipIri).has(HAS_PROPERTY, imoProperty));
                modify.insert(imoProperty.isA(IMO_NUMBER).andHas(HAS_VALUE, imoMeasure));
                modify.insert(imoMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getImoNumber()));

                Iri callSignProperty = P_DISP.iri(shipName + "CallSign");
                Iri callSignMeasure = P_DISP.iri(shipName + "CallSignMeasure");
                modify.insert(iri(shipIri).has(HAS_PROPERTY, callSignProperty));
                modify.insert(callSignProperty.isA(CALLSIGN).andHas(HAS_VALUE, callSignMeasure));
                modify.insert(callSignMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getCallSign()));
            });

            storeClient.executeUpdate(modify.getQueryString());
        }
    }

    List<String> cleanUpTimeSeries(long daysBefore) {
        SelectQuery query = Queries.SELECT();
        Variable shipVar = query.var();
        Variable measureVar = query.var();
        Variable timeseriesVar = query.var();

        GraphPattern queryPattern = GraphPatterns.and(
                shipVar.isA(SHIP).andHas(PropertyPaths.path(HAS_PROPERTY, HAS_VALUE), measureVar),
                measureVar.has(iri("https://www.theworldavatar.com/kg/ontotimeseries/hasTimeSeries"), timeseriesVar));

        query.where(queryPattern).prefix(P_OM, P_DISP);

        Map<String, List<String>> shipToMeasuresMap = new HashMap<>();
        Map<String, String> shipToTsMap = new HashMap<>();

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        for (int i = 0; i < queryResult.length(); i++) {
            String ship = queryResult.getJSONObject(i).getString(shipVar.getQueryString().substring(1));
            String measure = queryResult.getJSONObject(i).getString(measureVar.getQueryString().substring(1));
            String timeseries = queryResult.getJSONObject(i).getString(timeseriesVar.getQueryString().substring(1));

            shipToMeasuresMap.computeIfAbsent(ship, s -> new ArrayList<>());
            shipToMeasuresMap.get(ship).add(measure);
            shipToTsMap.put(ship, timeseries);
        }

        List<String> shipsToDeleteCompletely = new ArrayList<>();
        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            shipToMeasuresMap.entrySet().forEach(entry -> {
                String ship = entry.getKey();
                List<String> measures = entry.getValue();
                TimeSeries<Instant> tsLatest = tsClient.getLatestData(measures.get(0), conn);
                Instant earliestTimeTokeep = Instant.now().minus(daysBefore, ChronoUnit.DAYS);

                if (tsLatest.getTimes().isEmpty() || tsLatest.getTimes().get(0).isBefore(earliestTimeTokeep)) {
                    shipsToDeleteCompletely.add(entry.getKey());
                    tsClient.deleteTimeSeries(shipToTsMap.get(ship), conn);
                } else {
                    Instant earliestTime = tsClient.getOldestData(measures.get(0), conn).getTimes().get(0);

                    measures.forEach(
                            measure -> tsClient.deleteTimeSeriesHistory(measure, earliestTime, earliestTimeTokeep,
                                    conn));
                }
            });
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Error in cleanUpTimeSeries");
        }

        return shipsToDeleteCompletely;
    }

    void deleteShipDerivation(List<String> shipsToDeleteCompletely) {
        Prefix prefixTime = SparqlBuilder.prefix("time", iri("http://www.w3.org/2006/time#"));
        Prefix prefixDerivation = SparqlBuilder.prefix("derivation",
                iri("https://www.theworldavatar.com/kg/ontoderivation/"));

        Iri isDerivedFrom = prefixDerivation.iri("isDerivedFrom");
        Iri isDerivedUsing = prefixDerivation.iri("isDerivedUsing");
        Iri derivationType = prefixDerivation.iri("Derivation");
        Iri belongsTo = prefixDerivation.iri("belongsTo");

        Iri instantClass = prefixTime.iri("Instant");
        Iri timePositionClass = prefixTime.iri("TimePosition");
        Iri hasTRS = prefixTime.iri("hasTRS");
        Iri numericPosition = prefixTime.iri("numericPosition");
        Iri inTimePosition = prefixTime.iri("inTimePosition");
        Iri hasTime = prefixTime.iri("hasTime");

        Variable shipVar = SparqlBuilder.var("ship");
        Variable derivationVar = SparqlBuilder.var("derivation");
        Variable output = SparqlBuilder.var("output");
        Variable outputPredicate = SparqlBuilder.var("outputPredicate");
        Variable outputObject = SparqlBuilder.var("outputObject");
        Variable outputSubject = SparqlBuilder.var("outputSubject");
        Variable outputPredicate2 = SparqlBuilder.var("outputPredicate2");
        Variable time = SparqlBuilder.var("time");
        Variable timeUnixIri = SparqlBuilder.var("timeUnixIri");
        Variable timestamp = SparqlBuilder.var("timestamp");
        Variable trs = SparqlBuilder.var("trs");
        Variable agent = SparqlBuilder.var("agent");

        ValuesPattern<Iri> shipValues = new ValuesPattern<>(shipVar,
                shipsToDeleteCompletely.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);

        TriplePattern triplePattern1 = derivationVar.has(isDerivedFrom, shipVar).andIsA(derivationType)
                .andHas(isDerivedUsing, agent);

        // outputs, consider cases where output is subject/object of a triple
        TriplePattern triplePattern2 = output.has(belongsTo, derivationVar);
        TriplePattern triplePattern3 = output.has(outputPredicate, outputObject);
        TriplePattern triplePattern4 = outputSubject.has(outputPredicate2, output);

        // derivation timestamps
        TriplePattern timestampTp1 = derivationVar.has(hasTime, time);
        TriplePattern timeTpAll1 = time.isA(instantClass).andHas(inTimePosition, timeUnixIri);
        TriplePattern timeTpAll2 = timeUnixIri.isA(timePositionClass).andHas(numericPosition, timestamp).andHas(hasTRS,
                trs);

        GraphPattern graphPattern = GraphPatterns.and(triplePattern1, triplePattern2, triplePattern3.optional(),
                triplePattern4.optional(), timestampTp1, timeTpAll1, timeTpAll2, shipValues);

        ModifyQuery modify = Queries.MODIFY();
        modify.delete(triplePattern1, triplePattern2, triplePattern3, triplePattern4, timestampTp1, timeTpAll1,
                timeTpAll2).where(graphPattern).prefix(prefixTime, prefixDerivation);
        storeClient.executeUpdate(modify.getQueryString());
    }

    void deleteShips(List<String> shipsToDelete) {
        Variable label = SparqlBuilder.var("label");
        Variable ship = SparqlBuilder.var("ship");
        Variable property = SparqlBuilder.var("property");
        Variable propertyType = SparqlBuilder.var("propertyType");
        Variable measure = SparqlBuilder.var("measure");
        Variable measureType = SparqlBuilder.var("measureType");
        Variable value = SparqlBuilder.var("value");

        ValuesPattern<Iri> shipValues = new ValuesPattern<>(ship,
                shipsToDelete.stream().map(Rdf::iri).collect(Collectors.toList()), Iri.class);
        TriplePattern tp1 = ship.isA(SHIP).andHas(iri(RDFS.LABEL), label).andHas(HAS_PROPERTY, property);
        TriplePattern tp2 = property.isA(propertyType).andHas(HAS_VALUE, measure);
        TriplePattern tp3 = measure.isA(measureType);
        TriplePattern tp4 = measure.has(HAS_NUMERICALVALUE, value);

        ModifyQuery modify = Queries.MODIFY();
        modify.where(tp1, tp2, tp3, tp4.optional(), shipValues).delete(tp1, tp2, tp3, tp4).prefix(P_OM, P_DISP);

        storeClient.executeUpdate(modify.getQueryString());
        derivationClient.dropTimestampsOf(shipsToDelete);
    }
}
