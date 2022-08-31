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

import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.postgis.Point;

/**
 * sends sparql queries
 */
public class QueryClient {
    private StoreClientInterface storeClient;
    private TimeSeriesClient<Long> tsClient;

    String prefix = "http://www.theworldavatar.com/dispersion/";
    Prefix p_disp = SparqlBuilder.prefix("disp",iri(prefix));
    Prefix p_om = SparqlBuilder.prefix("om",iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));

    // classes
    Iri Ship = p_disp.iri("Ship");
    Iri Speed = p_disp.iri("Speed");
    Iri CourseOverGround = p_disp.iri("CourseOverGround");
    Iri MMSI = p_disp.iri("MMSI");
    Iri Location = p_disp.iri("Location");
    Iri Measure = p_om.iri("Measure");

    // properties
    Iri hasMMSI = p_disp.iri("hasMMSI");
    Iri hasSpeed = p_disp.iri("hasSpeed");
    Iri hasCourse = p_disp.iri("hasCourse");
    Iri hasLocation = p_disp.iri("hasLocation");
    Iri hasValue = p_om.iri("hasValue");
    Iri hasNumericalValue = p_om.iri("hasNumericalValue");

    public QueryClient(StoreClientInterface storeClient, TimeSeriesClient<Long> tsClient) {
        this.storeClient = storeClient;
        this.tsClient = tsClient;
    }

    /**
     * first query existing instances with MMSI values, then initialise if do not exist
     * @param ships
     */
    void initialiseShipsIfNotExist(List<Ship> ships) {
        SelectQuery query = Queries.SELECT();

        Variable mmsi = query.var();
        Variable mmsi_value = query.var();
        ValuesPattern vp = new ValuesPattern(mmsi_value, ships.stream().map(s -> s.getMmsi()).collect(Collectors.toList()));

        GraphPattern gp = mmsi.isA(MMSI).andHas(PropertyPaths.path(hasValue,hasNumericalValue),mmsi_value);

        query.where(gp,vp).prefix(p_om,p_disp).select(mmsi_value);
        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());
        List<Integer> initialisedShipMMSI = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i ++) {
            initialisedShipMMSI.add(queryResult.getJSONObject(i).getInt(mmsi_value.getQueryString().substring(1)));
        }

        List<Ship> newShipsToInitialise = new ArrayList<>();
        for (Ship ship : ships) {
            if (!initialisedShipMMSI.contains(ship.getMmsi())) {
                newShipsToInitialise.add(ship);
            }
        }
        createShip(newShipsToInitialise);
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

        if (ships.size() > 0) {
            // triples
            ModifyQuery modify = Queries.MODIFY();
            for (Ship ship : ships) {
                List<String> dataWithTimeSeries = new ArrayList<>();
                List<Class<?>> classes = new ArrayList<>();

                String shipName = "Ship" + ship.getMmsi();
                Iri shipIri = p_disp.iri(shipName);
                modify.insert(shipIri.isA(Ship));

                // mmsi
                Iri mmsiProperty = p_disp.iri(shipName +  "MMSI");
                Iri mmsiMeasure = p_disp.iri(shipName + "MMSIMeasure");
                
                modify.insert(shipIri.has(hasMMSI,mmsiProperty));
                modify.insert(mmsiProperty.isA(MMSI).andHas(hasValue, mmsiMeasure));
                modify.insert(mmsiMeasure.isA(Measure).andHas(hasNumericalValue, ship.getMmsi()));

                // Location time series
                Iri locationProperty = p_disp.iri(shipName + "Location");
                String locationMeasure = prefix + shipName + "LocationMeasure";
                modify.insert(shipIri.has(hasLocation, locationProperty));
                modify.insert(locationProperty.isA(Location).andHas(hasValue, iri(locationMeasure)));
                modify.insert(iri(locationMeasure).isA(Measure));

                dataWithTimeSeries.add(locationMeasure);
                classes.add(Point.class);

                // speed time series
                Iri speedProperty = p_disp.iri(shipName + "Speed");
                String speedMeasure = prefix + shipName + "SpeedMeasure";
                modify.insert(shipIri.has(hasSpeed, speedProperty));
                modify.insert(speedProperty.isA(Speed).andHas(hasValue, iri(speedMeasure)));
                modify.insert(iri(speedMeasure).isA(Measure));

                dataWithTimeSeries.add(speedMeasure);
                classes.add(Integer.class);

                // course time series
                Iri courseProperty = p_disp.iri(shipName + "Course");
                String courseMeasure = prefix + shipName + "CourseMeasure";
                modify.insert(shipIri.has(hasCourse, courseProperty));
                modify.insert(courseProperty.isA(CourseOverGround).andHas(hasValue, iri(courseMeasure)));
                modify.insert(iri(courseMeasure).isA(Measure));

                dataWithTimeSeries.add(courseMeasure);
                classes.add(Integer.class);

                dataIRIs.add(dataWithTimeSeries);
                dataClasses.add(classes);
                timeUnit.add("Unix timestamp");
            }
            modify.prefix(p_om,p_disp);
            storeClient.executeUpdate(modify.getQueryString());

            // time series in rdb, 4326 is the srid
            tsClient.bulkInitTimeSeries(dataIRIs, dataClasses, timeUnit, 4326);
        }
    }

    void setShipIRIs(List<Ship> ships) {
        SelectQuery query = Queries.SELECT();

        Variable ship = query.var();
        Variable mmsi_value = query.var();
        ValuesPattern vp = new ValuesPattern(mmsi_value, ships.stream().map(s -> s.getMmsi()).collect(Collectors.toList()));
        GraphPattern gp = ship.has(PropertyPaths.path(hasMMSI,hasValue,hasNumericalValue), mmsi_value);

        query.prefix(p_om,p_disp).where(gp,vp);

        JSONArray queryResult = storeClient.executeQuery(query.getQueryString());

        // look up map used later to assign ship Iris
        Map<Integer, Ship> mmsiToShipMap = new HashMap<>();
        ships.stream().forEach(s -> mmsiToShipMap.put(s.getMmsi(),s));

        for (int i = 0; i < queryResult.length(); i++) {
            int mmsi = queryResult.getJSONObject(i).getInt(mmsi_value.getQueryString().substring(1));
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

        GraphPattern gp = shipVar.has(PropertyPaths.path(hasCourse,hasValue), course)
            .andHas(PropertyPaths.path(hasSpeed,hasValue),speed)
            .andHas(PropertyPaths.path(hasLocation,hasValue),location);
        ValuesPattern vp = new ValuesPattern(shipVar, ships.stream().map(s -> iri(s.getIri())).collect(Collectors.toList()));

        query.prefix(p_disp,p_om).where(gp,vp);

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
            tsClient.addTimeSeriesData(ts);
        }
    }
}
