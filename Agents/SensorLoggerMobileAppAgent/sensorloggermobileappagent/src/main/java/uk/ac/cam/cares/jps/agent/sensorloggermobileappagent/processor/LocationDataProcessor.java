package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import org.postgis.Point;

import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.AgentConfig;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.Payload;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model.SensorData;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.util.*;
import java.util.stream.Collectors;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class LocationDataProcessor extends SensorDataProcessor {

    private SensorData<Double> bearing;
    private SensorData<Double> speed;
    private SensorData<Double> altitude;
    private SensorData<Point> geomLocation;
    private SensorData<String> session;

    public LocationDataProcessor(AgentConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super("GPSDevice", config, storeClient, smartphoneIRINode);
    }

    @Override
    public void addData(Payload data) {
        timeList.addAll(data.getLocationTs());
        bearing.addData(data.getBearings());
        speed.addData(data.getSpeeds());
        altitude.addData(data.getAltitudes());
        geomLocation.addData(data.getGeomLocations());
        session.addData(data.getSessionIds());
    }

    @Override
    public TimeSeries<Long> getProcessedTimeSeries() {
        // todo: do processing of location data
        List<String> dataIRIList = getDataIRIs();
        List<List<?>> valueList = getValues().stream()
                .map(ArrayList::new)
                .collect(Collectors.toList());

        List<Long> epochlist = timeList.stream().map(t -> t.toInstant().toEpochMilli()).toList();
        TimeSeries<Long> ts = new TimeSeries<>(new ArrayList<>(epochlist), dataIRIList, valueList);

        clearData();
        return ts;
    }

    @Override
    void initSensorData() {
        bearing = new SensorData<>(Double.class);
        speed = new SensorData<>(Double.class);
        altitude = new SensorData<>(Double.class);
        geomLocation = new SensorData<>(Point.class);
        session = new SensorData<>(String.class);
        sensorData = List.of(bearing, speed, altitude, geomLocation, session);
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
        this.bearing.setIri(queryResult.getJSONObject(0).optString("bearing"));
        this.altitude.setIri(queryResult.getJSONObject(0).optString("altitude"));
        this.speed.setIri(queryResult.getJSONObject(0).optString("speed"));
        this.geomLocation.setIri(queryResult.getJSONObject(0).optString("point"));
        this.session.setIri(queryResult.getJSONObject(0).optString("session"));
    }

    @Override
    public String getOntodeviceLabel() {
        return "GPSDevice";
    }
}
