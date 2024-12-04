package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import org.postgis.Point;

import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.Payload;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class LocationDataProcessor extends SensorDataProcessor {
    private String bearingIRI = null;
    private String speedIRI = null;
    private String altitudeIRI = null;
    private String pointIRI = null;
    private String sessionIRI = null;

    private final List<Double> bearingList = new ArrayList<>();
    private final List<Double> speedList = new ArrayList<>();
    private final List<Double> altitudeList = new ArrayList<>();
    private final List<Point> geomLocationList = new ArrayList<>();
    private final List<String> sessionIdList = new ArrayList<>();

    public LocationDataProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super(config, storeClient, smartphoneIRINode);
    }

    @Override
    public void addData(Payload data) {
        timeList.addAll(data.getLocationTs());
        bearingList.addAll(data.getBearings());
        speedList.addAll(data.getSpeeds());
        altitudeList.addAll(data.getAltitudes());
        geomLocationList.addAll(data.getGeomLocations());
        sessionIdList.addAll(data.getSessionIds());
    }

    @Override
    public TimeSeries<Long> getProcessedTimeSeries() {
        // todo: do processing of location data
        List<String> dataIRIList = Arrays.asList(bearingIRI, speedIRI, altitudeIRI, pointIRI, sessionIRI);
        List<List<?>> valueList = Arrays.asList(new ArrayList<>(bearingList),
                new ArrayList<>(speedList),
                new ArrayList<>(altitudeList),
                new ArrayList<>(geomLocationList),
                new ArrayList<>(sessionIdList));

        List<Long> epochlist = timeList.stream().map(t -> t.toInstant().toEpochMilli())
                .collect(Collectors.toList());
        TimeSeries<Long> ts = new TimeSeries<>(new ArrayList<>(epochlist), dataIRIList, valueList);

        clearData();
        return ts;
    }

    @Override
    public List<Class<?>> getDataClass() {
        List<Class<?>> dataClass = new ArrayList<>(Collections.nCopies(3, Double.class));
        dataClass.add(Point.class);
        dataClass.add(String.class);
        return dataClass;
    }

    @Override
    public List<String> getDataIRIs() {
        return List.of(bearingIRI, speedIRI, altitudeIRI, pointIRI, sessionIRI);
    }

    @Override
    void clearData() {
        timeList.clear();
        bearingList.clear();
        speedList.clear();
        altitudeList.clear();
        geomLocationList.clear();
        sessionIdList.clear();
    }

    @Override
    void getIrisFromKg() {
        Var bearing = Var.alloc("bearing");
        Var altitude = Var.alloc("altitude");
        Var speed = Var.alloc("speed");
        Var point = Var.alloc("point");
        Var session = Var.alloc("session");

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addPrefix("sf", SF)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?GPSDevice")
                .addWhere(smartphoneIRINode, "ontodevice:hasSessionID", "?session")
                .addWhere("?GPSDevice", "rdf:type", "ontodevice:GPSDevice")
                .addWhere("?GPSDevice", "ontodevice:measures", "?Bearing")
                .addWhere("?Bearing", "rdf:type", "ontodevice:Bearing")
                .addWhere("?Bearing", "om:hasValue", bearing)
                .addWhere("?GPSDevice", "ontodevice:measures", "?Altitude")
                .addWhere("?Altitude", "rdf:type", "ontodevice:Altitude")
                .addWhere("?Altitude", "om:hasValue", altitude)
                .addWhere("?GPSDevice", "ontodevice:measures", "?Speed")
                .addWhere("?Speed", "rdf:type", "om:Speed")
                .addWhere("?Speed", "om:hasValue", speed)
                .addWhere("?GPSDevice", "ontodevice:hasGeoLocation", point)
                .addWhere(point, "rdf:type", "sf:Point");

        SelectBuilder sb = new SelectBuilder()
                .addVar(bearing).addVar(altitude).addVar(speed).addVar(point).addVar(session).addWhere(wb);
        JSONArray queryResult;
        try {
            queryResult = storeClient.executeQuery(sb.buildString());
        } catch (Exception e) {
            // ontop does not accept queries before any mapping is added
            return;
        }
        if (queryResult.isEmpty()) {
            return;
        }
        bearingIRI = queryResult.getJSONObject(0).optString("bearing");
        altitudeIRI = queryResult.getJSONObject(0).optString("altitude");
        speedIRI = queryResult.getJSONObject(0).optString("speed");
        pointIRI = queryResult.getJSONObject(0).optString("point");
        sessionIRI = queryResult.getJSONObject(0).optString("session");
    }

    @Override
    public String getOntodeviceLabel() {
        return "GPSDevice";
    }
}
