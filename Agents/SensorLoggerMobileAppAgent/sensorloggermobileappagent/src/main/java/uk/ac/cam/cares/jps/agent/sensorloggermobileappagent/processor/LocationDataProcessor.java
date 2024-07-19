package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.sparql.core.Var;
import org.json.JSONArray;
import org.postgis.Point;
import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.DownSampleConfig;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;

import java.time.OffsetDateTime;
import java.util.*;

import static uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.OntoConstants.*;

public class LocationDataProcessor extends SensorDataProcessor {
    private String bearingIRI;
    private String speedIRI;
    private String altitudeIRI;
    private String pointIRI;

    private final ArrayList<OffsetDateTime> timeList = new ArrayList<>();
    private final List<Double> bearingList = new ArrayList<>();
    private final List<Double> speedList = new ArrayList<>();
    private final List<Double> altitudeList = new ArrayList<>();
    private final List<Point> geomLocationList = new ArrayList<>();

    public LocationDataProcessor(DownSampleConfig config, RemoteStoreClient storeClient, Node smartphoneIRINode) {
        super(config, storeClient, smartphoneIRINode);
    }

    @Override
    public void addData(HashMap data) {
        timeList.addAll((List<OffsetDateTime>) data.get("location_tsList"));
        bearingList.addAll((List<Double>) data.get("bearingList"));
        speedList.addAll((List<Double>) data.get("speedList"));
        altitudeList.addAll((List<Double>) data.get("altitudeList"));
        geomLocationList.addAll((List<Point>) data.get("geomLocationList"));
    }

    @Override
    public TimeSeries getProcessedTimeSeries() throws Exception {
        // todo: do processing of location data
        List<String> dataIRIList = Arrays.asList(bearingIRI, speedIRI, altitudeIRI, pointIRI);
        List<List<?>> valueList = Arrays.asList(bearingList, speedList, altitudeList, geomLocationList);
        TimeSeries ts = new TimeSeries(timeList, dataIRIList, valueList);
        clearData();
        return ts;
    }

    @Override
    public void initIRIs() {
        getIrisFromKg();

        if ((bearingIRI == null || bearingIRI.isEmpty())
                && (speedIRI == null || speedIRI.isEmpty())
                && (altitudeIRI == null || altitudeIRI.isEmpty())
                && (pointIRI == null || pointIRI.isEmpty())) {
            bearingIRI = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_bearing_" + UUID.randomUUID();
            speedIRI = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_speed_" + UUID.randomUUID();
            altitudeIRI = "https://www.theworldavatar.com/kg/sensorloggerapp/measure_altitude_" + UUID.randomUUID();
            pointIRI = "https://www.theworldavatar.com/kg/sensorloggerapp/point_" + UUID.randomUUID();

            isIriInstantiationNeeded = true;
            isRbdInstantiationNeeded = true;
        }
    }

    @Override
    public List<Class> getDataClass() {
        List<Class> dataClass = Collections.nCopies(getDataIRIMap().size() - 1, Double.class);
        dataClass.add(Point.class);
        return dataClass;
    }

    @Override
    public Map<String, String> getDataIRIMap() {
        Map<String, String> iriHashMap = new HashMap<>();
        iriHashMap.put("bearing", bearingIRI);
        iriHashMap.put("speed", speedIRI);
        iriHashMap.put("altitude", altitudeIRI);
        iriHashMap.put("point", pointIRI);
        return iriHashMap;
    }

    @Override
    void clearData() {
        timeList.clear();
        bearingList.clear();
        speedList.clear();
        altitudeList.clear();
        geomLocationList.clear();
    }

    @Override
    void getIrisFromKg() {
        Var bearing = Var.alloc("bearing");
        Var altitude = Var.alloc("altitude");
        Var speed = Var.alloc("speed");
        Var point = Var.alloc("point");

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("slma", SLA)
                .addPrefix("saref", SAREF)
                .addPrefix("ontodevice", ONTODEVICE)
                .addPrefix("rdf", RDF)
                .addPrefix("om", OM)
                .addWhere(smartphoneIRINode, "saref:consistsOf", "?GPSDevice")
                .addWhere("?GPSDevice", "rdf:type", "ontodevice:GPSDevice")
                .addWhere("?GPSDevice", "ontodevice:measures", "?Bearing")
                .addWhere("?Bearing", "rdf:type", "slma:Bearing")
                .addWhere("?Bearing", "om:hasValue", bearing)
                .addWhere("?GPSDevice", "ontodevice:measures", "?Altitude")
                .addWhere("?Altitude", "rdf:type", "slma:Altitude")
                .addWhere("?Altitude", "om:hasValue", altitude)
                .addWhere("?GPSDevice", "ontodevice:measures", "?Speed")
                .addWhere("?Speed", "rdf:type", "om:Speed")
                .addWhere("?Speed", "om:hasValue", speed)
                .addWhere("?GPSDevice", "ontodevice:hasGeoLocation", point)
                .addWhere(point, "rdf:type", "sf:Point");

        SelectBuilder sb = new SelectBuilder()
                .addVar(bearing).addVar(altitude).addVar(speed).addVar(point).addWhere(wb);
        JSONArray queryResult = storeClient.executeQuery(sb.buildString());
        if (queryResult.isEmpty()) {
            return;
        }
        bearingIRI = queryResult.getJSONObject(0).optString("bearing");
        altitudeIRI = queryResult.getJSONObject(0).optString("altitude");
        speedIRI = queryResult.getJSONObject(0).optString("speed");
        pointIRI = queryResult.getJSONObject(0).optString("point");
    }
}
