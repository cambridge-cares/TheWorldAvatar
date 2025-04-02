package uk.ac.cam.cares.jps.timeline.model.trajectory;

import org.json.JSONObject;

import uk.ac.cam.cares.jps.timeline.model.bottomsheet.ActivityItem;

public class TrajectorySegment {
    private final long startTime;
    private final long endTime;
    private final int id;
    private final String activityType;
    private final String sessionId;
    private final JSONObject geom;
    private final int distanceTraveled;
    private final String iri;
    private final ActivityItem activity;
    
    TrajectorySegment(long startTime, long endTime, int id, String activityType, String sessionId, JSONObject geom, int distanceTraveled, String iri) {
        this.startTime = startTime;
        this.endTime = endTime;
        this.id = id;
        this.activityType = activityType;
        this.sessionId = sessionId;
        this.geom = geom;
        this.distanceTraveled = distanceTraveled;
        this.iri = iri;
        this.activity = new ActivityItem(id, activityType, startTime, endTime);
    }


    public long getStartTime() {
        return this.startTime;
    }

    public long getEndTime() {
        return this.endTime;
    }

    public int getId() {
        return this.id;
    }

    public String getActivityType() {
        return this.activityType;
    }

    public String getSessionId() {
        return this.sessionId;
    }

    public JSONObject getGeom() {
        return this.geom;
    }

    public int getDistanceTraveled() {
        return this.distanceTraveled;
    }

    public String getIri() {
        return this.iri;
    }

    public ActivityItem getActivity() {
        return this.activity;
    }

}