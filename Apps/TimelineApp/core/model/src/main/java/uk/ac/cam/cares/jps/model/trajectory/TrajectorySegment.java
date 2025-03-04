package uk.ac.cam.cares.jps.model;

public record TrajectorySegment(long startTime, long endTime, int id, String activityType,
                                String sessionId, int distanceTraveled, String iri) {
}