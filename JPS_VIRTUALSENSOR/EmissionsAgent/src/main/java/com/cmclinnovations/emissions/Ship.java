package com.cmclinnovations.emissions;

import java.time.Instant;
import org.postgis.Point;

public class Ship {
    private String iri;
    private int mmsi;
    private double speed;
    private int course;
    private int shipType;
    private double lat;
    private double lon;
    private Instant timestamp; // timestamp when data was obtained

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
