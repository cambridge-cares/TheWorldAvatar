package uk.ac.cam.cares.jps.agent.sensorloggermobileappquery_agent.objects;

import org.postgis.Point;

public class PersonGPSPoint {
    private String iri;
    private Point location;
    
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
}