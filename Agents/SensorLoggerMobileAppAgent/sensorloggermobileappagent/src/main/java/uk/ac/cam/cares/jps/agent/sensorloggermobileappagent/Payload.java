package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.json.JSONArray;
import org.postgis.Point;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

public class Payload {
    private final List<OffsetDateTime> accelTs = new ArrayList<>();
    private final List<Double> accelXs = new ArrayList<>();
    private final List<Double> accelYs = new ArrayList<>();
    private final List<Double> accelZs = new ArrayList<>();
    private final List <OffsetDateTime> magnetometerTs = new ArrayList<>();
    private final List <Double> magnetometerXs = new ArrayList<>();
    private final List <Double> magnetometerYs = new ArrayList<>();
    private final List <Double> magnetometerZs = new ArrayList<>();
    private final List <OffsetDateTime> gravityTs = new ArrayList<>();
    private final List <Double> gravityXs = new ArrayList<>();
    private final List <Double> gravityYs = new ArrayList<>();
    private final List <Double> gravityZs = new ArrayList<>();
    private final List <OffsetDateTime> locationTs = new ArrayList<>();
    private final List <Double> bearings = new ArrayList<>();
    private final List <Double> speeds = new ArrayList<>();
    private final List <Double> altitudes = new ArrayList<>();
    private final List <Point> geomLocations = new ArrayList<>();
    private final List <String> sessionIds = new ArrayList<>();
    private final List <OffsetDateTime> dBFSTs = new ArrayList<>();
    private final List <Double> dBFSs = new ArrayList<>();
    private final List <OffsetDateTime> lightValueTs = new ArrayList<>();
    private final List <Double> lightValues = new ArrayList<>();
    private final List <OffsetDateTime> brightnessTs = new ArrayList<>();
    private final List <Double> brightness = new ArrayList<>();
    private final List <OffsetDateTime> activityTs = new ArrayList<>();
    private final List <Integer> confidences = new ArrayList<>();
    private final List <String> activityTypes = new ArrayList<>();

    public Payload(JSONArray payload, String sessionId)
            throws JsonProcessingException {
        ObjectMapper mapper = new ObjectMapper();
        JsonNode payloadNode = mapper.readTree(payload.toString());

        for (JsonNode node : payloadNode) {

            JsonNode timeEPOCH = node.get("time");
            JsonNode sensor = node.get("name");
            JsonNode values = node.get("values");
            Instant instant = Instant.ofEpochSecond(timeEPOCH.longValue() / 1000000000,
                    timeEPOCH.longValue() % 1000000000);
            OffsetDateTime timestamp = OffsetDateTime.ofInstant(instant, ZoneOffset.UTC);

            switch (sensor.textValue()) {
                case "accelerometer" -> {
                    accelTs.add(timestamp);
                    accelXs.add(values.get("x").doubleValue());
                    accelYs.add(values.get("y").doubleValue());
                    accelZs.add(values.get("z").doubleValue());
                }
                case "magnetometer" -> {
                    magnetometerTs.add(timestamp);
                    magnetometerXs.add(values.get("x").doubleValue());
                    magnetometerYs.add(values.get("y").doubleValue());
                    magnetometerZs.add(values.get("z").doubleValue());
                }
                case "gravity" -> {
                    gravityTs.add(timestamp);
                    gravityXs.add(values.get("x").doubleValue());
                    gravityYs.add(values.get("y").doubleValue());
                    gravityZs.add(values.get("z").doubleValue());
                }
                case "location" -> {
                    locationTs.add(timestamp);
                    bearings.add(values.get("bearing").doubleValue());
                    speeds.add(values.get("speed").doubleValue());
                    altitudes.add(values.get("altitude").doubleValue());

                    // Parse latitude and longitude into geomLocations
                    double latitude = values.get("latitude").doubleValue();
                    double longitude = values.get("longitude").doubleValue();
                    Point point = new Point(longitude, latitude);
                    point.setSrid(4326);
                    geomLocations.add(point);
                    sessionIds.add(sessionId);
                }
                case "microphone" -> {
                    dBFSTs.add(timestamp);
                    dBFSs.add(values.get("dBFS").doubleValue());
                }
                case "light" -> {
                    lightValueTs.add(timestamp);
                    lightValues.add(values.get("lux").doubleValue());
                }
                case "brightness" -> {
                    brightnessTs.add(timestamp);
                    brightness.add(values.get("brightness").doubleValue());
                }
                case "activity" -> {
                    activityTs.add(timestamp);
                    confidences.add(values.get("confidence").intValue());
                    activityTypes.add(values.get("type").textValue());
                }
                default -> {
                }
            }
        }
    }

    public List<OffsetDateTime> getAccelTs() {
        return accelTs;
    }

    public List<Double> getAccelXs() {
        return accelXs;
    }

    public List<Double> getAccelYs() {
        return accelYs;
    }

    public List<Double> getAccelZs() {
        return accelZs;
    }

    public List<OffsetDateTime> getMagnetometerTs() {
        return magnetometerTs;
    }

    public List<Double> getMagnetometerXs() {
        return magnetometerXs;
    }

    public List<Double> getMagnetometerYs() {
        return magnetometerYs;
    }

    public List<Double> getMagnetometerZs() {
        return magnetometerZs;
    }

    public List<OffsetDateTime> getGravityTs() {
        return gravityTs;
    }

    public List<Double> getGravityXs() {
        return gravityXs;
    }

    public List<Double> getGravityYs() {
        return gravityYs;
    }


    public List<Double> getGravityZs() {
        return gravityZs;
    }

    public List<OffsetDateTime> getLocationTs() {
        return locationTs;
    }

    public List<Double> getBearings() {
        return bearings;
    }

    public List<Double> getSpeeds() {
        return speeds;
    }

    public List<Double> getAltitudes() {
        return altitudes;
    }


    public List<Point> getGeomLocations() {
        return geomLocations;
    }


    public List<String> getSessionIds() {
        return sessionIds;
    }

    public List<OffsetDateTime> getdBFSTs() {
        return dBFSTs;
    }


    public List<Double> getdBFSs() {
        return dBFSs;
    }

    public List<OffsetDateTime> getLightValueTs() {
        return lightValueTs;
    }


    public List<Double> getLightValues() {
        return lightValues;
    }

    public List<OffsetDateTime> getBrightnessTs() {
        return brightnessTs;
    }


    public List<Double> getBrightness() {
        return brightness;
    }


    public List<OffsetDateTime> getActivityTs() {
        return activityTs;
    }


    public List<Integer> getConfidences() {
        return confidences;
    }


    public List<String> getActivityTypes() {
        return activityTypes;
    }

}
