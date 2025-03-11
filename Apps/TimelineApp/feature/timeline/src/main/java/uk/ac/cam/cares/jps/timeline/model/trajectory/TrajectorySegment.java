package uk.ac.cam.cares.jps.timeline.model.trajectory;

import org.json.JSONObject;

public record TrajectorySegment(long startTime, long endTime, int id, String activityType,
                                String sessionId, JSONObject geom, int distanceTraveled, String iri) {
}