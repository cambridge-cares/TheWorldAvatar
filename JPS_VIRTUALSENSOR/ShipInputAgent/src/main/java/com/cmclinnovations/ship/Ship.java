package com.cmclinnovations.ship;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.json.JSONObject;
import org.postgis.Point;

public class Ship {
    private String iri;
    private String locationMeasureIri;
    private String courseMeasureIri;
    private String speedMeasureIri;
    private String latMeasureIri;
    private String lonMeasureIri;
    private int mmsi;
    private double speed;
    private double course;
    private int shipType = 0;
    private double lat;
    private double lon;
    private Instant timestamp; // timestamp when data was obtained
    private String shipName;
    private double draught = 0;
    private JSONObject dimension = new JSONObject(); // {"A": 1, "B":2, ...} according to aisstream
    private int imoNumber = 0;
    private String callSign = "";

    private List<Double> speedList;
    private List<Double> latList;
    private List<Double> lonList;
    private List<Double> cogList; // course over ground
    private List<Double> rotList; // rate of turn
    private List<Double> headingList; // true heading
    private List<Instant> timestampList;
    private boolean hasTimeSeries;

    // this constructor is used for live data, ais stream
    public Ship(int mmsi) {
        this.mmsi = mmsi;
        hasTimeSeries = true;
        timestampList = new ArrayList<>();
        speedList = new ArrayList<>();
        latList = new ArrayList<>();
        lonList = new ArrayList<>();
        cogList = new ArrayList<>();
        rotList = new ArrayList<>();
        headingList = new ArrayList<>();
    }

    public boolean hasTimeSeries() {
        return hasTimeSeries;
    }

    public void addTimeSeriesData(Instant timestamp, double speed, double lat, double lon, double cog, double rot,
            double heading) {
        timestampList.add(timestamp);
        speedList.add(speed);
        latList.add(lat);
        lonList.add(lon);
        cogList.add(cog);
        rotList.add(rot);
        headingList.add(heading);
    }

    public List<Double> getSpeedList() {
        return speedList;
    }

    public List<Long> getTimestampList() {
        return timestampList.stream().map(Instant::getEpochSecond).collect(Collectors.toList());
    }

    public List<Double> getCogList() {
        return cogList;
    }

    // raw data
    public Ship(JSONObject json, int timeOffset) {
        // this is specific to data from an API
        this.mmsi = json.getInt("MMSI");
        this.speed = json.getInt("SPEED");
        this.course = json.getDouble("COURSE");
        this.lat = json.getDouble("LAT");
        this.lon = json.getDouble("LON");
        this.shipType = json.getInt("SHIPTYPE");
        this.timestamp = LocalDateTime.parse(json.getString("TIMESTAMP")).toInstant(ZoneOffset.UTC)
                .plus(Duration.ofHours(timeOffset));

        shipName = "Ship: ";
        if (json.has("SHIPNAME")) {
            shipName += json.getString("SHIPNAME");
        } else {
            shipName += "Ship - " + mmsi;
        }
    }

    public void setTimestamp(Instant timestamp) {
        this.timestamp = timestamp;
    }

    public Instant getTimestamp() {
        return this.timestamp;
    }

    public void setIri(String iri) {
        this.iri = iri;
    }

    public String getIri() {
        return this.iri;
    }

    public void setMmsi(int mmsi) {
        this.mmsi = mmsi;
    }

    public int getMmsi() {
        return this.mmsi;
    }

    public void setSpeed(double speed) {
        this.speed = speed;
    }

    public double getSpeed() {
        return this.speed;
    }

    public void setCourse(int course) {
        this.course = course;
    }

    public double getCourse() {
        return this.course;
    }

    public Point getLocation() {
        Point point = new Point();
        point.setX(this.lon);
        point.setY(this.lat);
        point.setSrid(4326);

        return point;
    }

    public double getLat() {
        return lat;
    }

    public double getLon() {
        return lon;
    }

    public List<Point> getLocationList() {
        List<Point> pointList = new ArrayList<>();

        for (int i = 0; i < latList.size(); i++) {
            Point point = new Point(lonList.get(i), latList.get(i));
            point.setSrid(4326);
            pointList.add(point);
        }

        return pointList;
    }

    public void setShipType(int shipType) {
        this.shipType = shipType;
    }

    public int getShipType() {
        return this.shipType;
    }

    public void setLocationMeasureIri(String locationMeasureIri) {
        this.locationMeasureIri = locationMeasureIri;
    }

    public String getLocationMeasureIri() {
        return this.locationMeasureIri;
    }

    public void setCourseMeasureIri(String courseMeasureIri) {
        this.courseMeasureIri = courseMeasureIri;
    }

    public String getCourseMeasureIri() {
        return this.courseMeasureIri;
    }

    public void setSpeedMeasureIri(String speedMeasureIri) {
        this.speedMeasureIri = speedMeasureIri;
    }

    public String getSpeedMeasureIri() {
        return this.speedMeasureIri;
    }

    public void setLatMeasureIri(String latMeasureIri) {
        this.latMeasureIri = latMeasureIri;
    }

    public String getLatMeasureIri() {
        return latMeasureIri;
    }

    public void setLonMeasureIri(String lonMeasureIri) {
        this.lonMeasureIri = lonMeasureIri;
    }

    public String getLonMeasureIri() {
        return lonMeasureIri;
    }

    public void setShipName(String shipName) {
        this.shipName = "Ship: " + shipName;
    }

    public String getShipName() {
        return shipName;
    }

    public void setDraught(double draught) {
        this.draught = draught;
    }

    public double getDraught() {
        return draught;
    }

    public void setDimension(JSONObject dimension) {
        this.dimension = dimension;
    }

    public JSONObject getDimension() {
        return dimension;
    }

    public void setImoNumber(int imoNumber) {
        this.imoNumber = imoNumber;
    }

    public int getImoNumber() {
        return imoNumber;
    }

    public void setCallSign(String callSign) {
        this.callSign = callSign.trim();
    }

    public String getCallSign() {
        return callSign;
    }

    public List<Double> getLatList() {
        return latList;
    }

    public List<Double> getLonList() {
        return lonList;
    }
}
