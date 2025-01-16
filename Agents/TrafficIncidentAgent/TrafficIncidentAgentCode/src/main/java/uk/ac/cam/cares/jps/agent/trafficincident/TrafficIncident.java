package uk.ac.cam.cares.jps.agent.trafficincident;

import java.util.UUID;

public class TrafficIncident {
    public long startTime;
    public long endTime;
    public String incidentType;
    public double latitude;
    public double longitude;
    // message field can be updated during different call, depending on specific scenario
    public String message;
    public boolean status; // true : incident is ongoing
    public String iri; 

    public TrafficIncident(String incidentType, double latitude, double longitude, String message, long startTime, boolean status) {
        this.startTime = startTime;
        this.endTime = 0;
        this.incidentType = incidentType;
        this.latitude = latitude;
        this.longitude = longitude;
        this.message = message;
        this.status = status;
        this.iri = UUID.randomUUID().toString();
    }

    public TrafficIncident(String iri, String incidentType, double latitude, double longitude, String message, long startTime, boolean status) {
        this.iri = iri;
        this.startTime = startTime;
        this.endTime = 0;
        this.incidentType = incidentType;
        this.latitude = latitude;
        this.longitude = longitude;
        this.message = message;
        this.status = status;
    }

    public void setEndTime(long endTime) {
        this.endTime = endTime;
    }

    public void setStatus(boolean status) {
        this.status = status;
    }

    @Override
    public String toString() {
        if (this.endTime == 0) {
            return String.format("%s %s %s at latitude %f, longitude %f starting from %d", (this.status ? "Ongoing" : "Ended"), this.iri, this.incidentType, this.latitude, this.longitude, this.startTime);
        } else {
            return String.format("%s %s %s at latitude %f, longitude %f starting from %d to %d", (this.status ? "Ongoing" : "Ended"), this.iri, this.incidentType, this.latitude, this.longitude, this.startTime, this.endTime);
        }

    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof TrafficIncident) {
            // under the assumption that TrafficIncident with same type, location and start time must be the same (message may get updated during different call)
            TrafficIncident other = (TrafficIncident) obj;
            return this.incidentType.equals(other.incidentType)
                    && this.latitude == other.latitude
                    && this.longitude == other.longitude
                    && this.startTime == other.startTime;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return String.format("%s at latitude %f, longitude %f starting from %d", this.incidentType, this.latitude, this.longitude, this.startTime).hashCode();
    }
}