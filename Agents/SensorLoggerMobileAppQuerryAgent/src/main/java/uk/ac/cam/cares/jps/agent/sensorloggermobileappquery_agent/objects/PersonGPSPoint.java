package uk.ac.cam.cares.jps.agent.sensorloggermobileappquery_agent.objects;

import java.time.OffsetDateTime;

import org.postgis.Point;

public class PersonGPSPoint {
    private String iri;
    private Point location;
    private OffsetDateTime time;
    
    public PersonGPSPoint(String iri) {
        this.iri = iri;
    }

    public String getIri() {
        return this.iri;
    }

    public void setLocation(Point location) {
        this.location = location;
    }
    public Point getLocation() {
        return this.location;
    }
    public void setTime(OffsetDateTime time) {
        this.time = time;
    }
    public OffsetDateTime getTime() {
        return this.time;
    }
}