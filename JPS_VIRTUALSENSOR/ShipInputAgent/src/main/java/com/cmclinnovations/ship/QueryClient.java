package com.cmclinnovations.ship;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.derivation.Derivation;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

import org.postgis.Point;

/**
 * sends sparql queries
 */
public class QueryClient {
    private static final Logger LOGGER = LogManager.getLogger(QueryClient.class);
    private StoreClientInterface storeClient;
    private TimeSeriesClient<Long> tsClient;
    private DerivationClient derivationClient;
    private RemoteRDBStoreClient remoteRDBStoreClient;

    static final String PREFIX = "http://www.theworldavatar.com/dispersion/";
    private static final Prefix P_DISP = SparqlBuilder.prefix("disp",iri(PREFIX));
    static final String OM_STRING = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final Prefix P_OM = SparqlBuilder.prefix("om",iri(OM_STRING));

    // classes
    // as Iri classes for sparql updates sent directly from here
    private static final Iri SHIP = P_DISP.iri("Ship");
    private static final Iri SPEED = P_DISP.iri("Speed");
    private static final Iri COURSE_OVER_GROUND = P_DISP.iri("CourseOverGround");
    private static final Iri MMSI = P_DISP.iri("MMSI");
    private static final Iri LOCATION = P_DISP.iri("Location");
    private static final Iri SHIP_TYPE = P_DISP.iri("ShipType");
    private static final Iri MEASURE = P_DISP.iri("Measure");

    // properties
    private static final Iri HAS_MMSI = P_DISP.iri("hasMMSI");
    private static final Iri HAS_SPEED = P_DISP.iri("hasSpeed");
    private static final Iri HAS_COURSE = P_DISP.iri("hasCourse");
    private static final Iri HAS_SHIPTYPE = P_DISP.iri("hasShipType");
    private static final Iri HAS_LOCATION = P_DISP.iri("hasLocation");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");

    public QueryClient(StoreClientInterface storeClient, TimeSeriesClient<Long> tsClient, DerivationClient derivationClient, RemoteRDBStoreClient remoteRDBStoreClient) {
        this.storeClient = storeClient;
        this.tsClient = tsClient;
        this.derivationClient = derivationClient;
        this.remoteRDBStoreClient = remoteRDBStoreClient;
    }

    /**
     * returns false if there's nothing in the KG
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
     * first query existing instances with MMSI values, then initialise if do not exist
     * @param ships
     */
    List<Ship> initialiseShipsIfNotExist(List<Ship> ships) {
        SelectQuery query = Queries.SELECT();

        Variable mmsi = query.var();
        Variable mmsiValue = query.var();
        ValuesPattern<Integer> vp = new ValuesPattern<>(mmsiValue, ships.stream().map(Ship::getMmsi).collect(Collectors.toList()), Integer.class);

        GraphPattern gp = mmsi.isA(MMSI).andHas(PropertyPaths.path(HAS_VALUE,HAS_NUMERICALVALUE),mmsiValue);

        query.where(gp,vp).prefix(P_OM,P_DISP).select(mmsiValue);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        List<Integer> initialisedShipMMSI = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i ++) {
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
     * called by initialiseShipsIfNotExist, adds triples and initialises the time series tables
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
                Iri mmsiProperty = P_DISP.iri(shipName +  "MMSI");
                Iri mmsiMeasure = P_DISP.iri(shipName + "MMSIMeasure");
                
                modify.insert(shipIri.has(HAS_MMSI,mmsiProperty));
                modify.insert(mmsiProperty.isA(MMSI).andHas(HAS_VALUE, mmsiMeasure));
                modify.insert(mmsiMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getMmsi()));

                // ship type (integer)
                Iri shipTypeProperty = P_DISP.iri(shipName +  "ShipType");
                Iri shipTypeMeasure = P_DISP.iri(shipName +  "ShipTypeMeasure");

                modify.insert(shipIri.has(HAS_SHIPTYPE, shipTypeProperty));
                modify.insert(shipTypeProperty.isA(SHIP_TYPE).andHas(HAS_VALUE, shipTypeMeasure));
                modify.insert(shipTypeMeasure.isA(MEASURE).andHas(HAS_NUMERICALVALUE, ship.getShipType()));

                // Location time series
                Iri locationProperty = P_DISP.iri(shipName + "Location");
                String locationMeasure = PREFIX + shipName + "LocationMeasure";
                modify.insert(shipIri.has(HAS_LOCATION, locationProperty));
                modify.insert(locationProperty.isA(LOCATION).andHas(HAS_VALUE, iri(locationMeasure)));
                modify.insert(iri(locationMeasure).isA(MEASURE));

                dataWithTimeSeries.add(locationMeasure);
                classes.add(Point.class);

                // speed time series
                Iri speedProperty = P_DISP.iri(shipName + "Speed");
                String speedMeasure = PREFIX + shipName + "SpeedMeasure";
                modify.insert(shipIri.has(HAS_SPEED, speedProperty));
                modify.insert(speedProperty.isA(SPEED).andHas(HAS_VALUE, iri(speedMeasure)));
                modify.insert(iri(speedMeasure).isA(MEASURE));

                dataWithTimeSeries.add(speedMeasure);
                classes.add(Integer.class);

                // course time series
                Iri courseProperty = P_DISP.iri(shipName + "Course");
                String courseMeasure = PREFIX + shipName + "CourseMeasure";
                modify.insert(shipIri.has(HAS_COURSE, courseProperty));
                modify.insert(courseProperty.isA(COURSE_OVER_GROUND).andHas(HAS_VALUE, iri(courseMeasure)));
                modify.insert(iri(courseMeasure).isA(MEASURE));

                dataWithTimeSeries.add(courseMeasure);
                classes.add(Integer.class);

                dataIRIs.add(dataWithTimeSeries);
                dataClasses.add(classes);
                timeUnit.add("Unix timestamp");
            }
            modify.prefix(P_OM,P_DISP);
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

    void setShipIRIs(List<Ship> ships) {
        SelectQuery query = Queries.SELECT();

        Variable ship = query.var();
        Variable mmsiValue = query.var();
        ValuesPattern<Integer> vp = new ValuesPattern<>(mmsiValue, ships.stream().map(Ship::getMmsi).collect(Collectors.toList()), Integer.class);
        GraphPattern gp = ship.has(PropertyPaths.path(HAS_MMSI,HAS_VALUE,HAS_NUMERICALVALUE), mmsiValue);

        query.prefix(P_OM,P_DISP).where(gp,vp);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        // look up map used later to assign ship Iris
        Map<Integer, Ship> mmsiToShipMap = new HashMap<>();
        ships.stream().forEach(s -> mmsiToShipMap.put(s.getMmsi(),s));

        for (int i = 0; i < queryResult.length(); i++) {
            int mmsi = queryResult.getJSONObject(i).getInt(mmsiValue.getQueryString().substring(1));
            String shipIri = queryResult.getJSONObject(i).getString(ship.getQueryString().substring(1));

            // obtain ship object from map and set IRI
            mmsiToShipMap.get(mmsi).setIri(shipIri);
        }
    }

    void updateTimeSeriesData(List<Ship> ships) {
        // step 1: query measure IRIs for each ship and group them
        // each list of measureIRI belongs to the same time series table
        Map<Ship, List<String>> shipToMeasureIRIMap = new HashMap<>();

        Map<String, Ship> iriToShipMap = new HashMap<>();
        ships.stream().forEach(s -> iriToShipMap.put(s.getIri(),s));

        SelectQuery query = Queries.SELECT();

        Variable shipVar = query.var();
        Variable course = query.var();
        Variable speed = query.var();
        Variable location = query.var();

        GraphPattern gp = shipVar.has(PropertyPaths.path(HAS_COURSE,HAS_VALUE), course)
            .andHas(PropertyPaths.path(HAS_SPEED,HAS_VALUE),speed)
            .andHas(PropertyPaths.path(HAS_LOCATION,HAS_VALUE),location);
        ValuesPattern<Iri> vp = new ValuesPattern<>(shipVar, ships.stream().map(s -> iri(s.getIri())).collect(Collectors.toList()), Iri.class);

        query.prefix(P_DISP,P_OM).where(gp,vp);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        for (int i = 0; i < queryResult.length(); i++) {
            String shipIri = queryResult.getJSONObject(i).getString(shipVar.getQueryString().substring(1));
            String courseMeasure = queryResult.getJSONObject(i).getString(course.getQueryString().substring(1));
            String speedMeasure = queryResult.getJSONObject(i).getString(speed.getQueryString().substring(1));
            String locationMeasure = queryResult.getJSONObject(i).getString(location.getQueryString().substring(1));

            shipToMeasureIRIMap.put(iriToShipMap.get(shipIri), Arrays.asList(courseMeasure,speedMeasure,locationMeasure));
        }

        // generate 1 time series object for each ship and upload to rdb
        Iterator<Ship> shipIterator = shipToMeasureIRIMap.keySet().iterator();

        try (Connection conn = remoteRDBStoreClient.getConnection()) {
            while(shipIterator.hasNext()) {
                Ship ship = shipIterator.next();
                List<String> dataIRIs = shipToMeasureIRIMap.get(ship);
    
                // order of dataIRIs is course, speed, location, as defined in the previous loop
                List<List<?>> values = new ArrayList<>();
                values.add(Arrays.asList(ship.getCourse()));
                values.add(Arrays.asList(ship.getSpeed()));
                values.add(Arrays.asList(ship.getLocation()));
    
                List<Long> time = Arrays.asList(ship.getTimestamp().getEpochSecond());
    
                TimeSeries<Long> ts = new TimeSeries<>(time,dataIRIs,values);
                tsClient.addTimeSeriesData(ts, conn);
            }
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
		modify.insert(operationIri.isA(operation).andHas(hasHttpUrl, iri(EnvConfig.EMISSIONS_AGENT_URL)).andHas(hasInput, inputIri));
        modify.insert(inputIri.has(hasMandatoryPart, partIri));
        modify.insert(partIri.has(hasType, SHIP)).prefix(P_DISP);

        storeClient.executeUpdate(modify.getQueryString());
    }

    /**
     * 
     * @param ships
     */
    void createNewDerivations(List<Ship> ships) {
        CompletableFuture<Derivation> getAsync = null;

        for (Ship ship : ships) {
            getAsync = CompletableFuture.supplyAsync(() -> {
                Derivation derivation = null;
                try {
                    derivation = derivationClient.createSyncDerivationForNewInfo(EnvConfig.EMISSIONS_AGENT_IRI, Arrays.asList(ship.getIri()), DerivationSparql.ONTODERIVATION_DERIVATION);
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
    }
}
