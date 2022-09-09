package com.cmclinnovations.emissions;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

import org.json.JSONObject;
import org.postgis.Point;

public class Ship {
    private String iri;
    private int mmsi;
    private int speed;
    private int course;
    private int shipType;
    private double lat;
    private double lon;
    private Instant timestamp; // timestamp when data was obtained

    public Ship() {}

    // raw data
    public Ship(JSONObject json, int timeOffset) {
        // this is specific to data from an API
        this.mmsi = json.getInt("MMSI");
        this.speed = json.getInt("SPEED");
        this.course = json.getInt("COURSE");
        this.lat = json.getDouble("LAT");
        this.lon = json.getDouble("LON");
        this.shipType = json.getInt("SHIPTYPE");
        this.timestamp = LocalDateTime.parse(json.getString("TIMESTAMP")).toInstant(ZoneOffset.UTC).plus(Duration.ofHours(timeOffset));
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

    public void setSpeed(int speed) {
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

    public void setLat(double lat) {
        this.lat = lat;
    }
    public double getLat() {
        return this.lat;
    }

    public void setLon(double lon) {
        this.lon = lon;
    }
    public double getLon() {
        return this.lon;
    }

    public Point getLocation() {
        Point point = new Point();
        point.setX(this.lon);
        point.setY(this.lat);
        point.setSrid(4326);

        return point;
    }

    public void setShipType(int shipType) {
        this.shipType = shipType;
    }
    public int getShipType() {
        return this.shipType;
    }
}
