package uk.ac.cam.cares.jps.timeline.model.trajectory;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Map;
import java.util.Objects;

import uk.ac.cam.cares.jps.timeline.model.bottomsheet.ActivityItem;
import uk.ac.cam.cares.jps.timeline.model.bottomsheet.ActivitySummary;
import uk.ac.cam.cares.jps.timeline.model.bottomsheet.Session;

public class TrajectoryByDate {
    private final String trajectoryString;
    private final LocalDate date;
    private final List<Session> sessions;
    private final List<ActivitySummary> activitySummaries;

    public TrajectoryByDate(String trajectoryStr, LocalDate date) {
        this.trajectoryString = trajectoryStr;
        this.sessions = parseForUniqueSessions(trajectoryStr); //sessionId, sessionTitle, List<Segments>
        this.activitySummaries = summarizeSessions(this.sessions);
        this.date = date;
    }

    private List<ActivitySummary> summarizeSessions(List<Session> sessions) {

        List<TrajectorySegment> segments = new ArrayList<>();
        for (Session s : sessions) {
            segments.addAll(s.getTrajectorySegments());
        }

        List<ActivitySummary> summaries = new ArrayList<>();

        Map<String, List<Integer>> distancePerActivityType = new HashMap<>();
        Map<String, List<Long>> timePerActivityType = new HashMap<>();
        long startTime;

        List<TrajectorySegment> trajectorySegments = getTrajectorySegments();
            for (TrajectorySegment segment : trajectorySegments) {

                String activityType = segment.getActivityType();
                startTime = segment.getStartTime();
                long endTime = segment.getEndTime();
                int distance = segment.getDistanceTraveled();

                long minutes = (endTime > startTime) ? differenceInTime(startTime, endTime) : 0;

                timePerActivityType.putIfAbsent(activityType, new ArrayList<>());
                Objects.requireNonNull(timePerActivityType.get(activityType)).add(minutes);

                distancePerActivityType.putIfAbsent(activityType, new ArrayList<>());
                Objects.requireNonNull(distancePerActivityType.get(activityType)).add(distance);
            }
        for (String activity : distancePerActivityType.keySet()) {
            int totalDistance = activity.equals("still") ? 0 : Objects.requireNonNull(distancePerActivityType.get(activity)).stream().mapToInt(Integer::intValue).sum();
            long totalTime = Objects.requireNonNull(timePerActivityType.get(activity)).stream().mapToLong(Long::longValue).sum();

            summaries.add(new ActivitySummary(activity, totalDistance, totalTime));
        }

        return summaries;

    }

    private List<Session> parseForUniqueSessions(String trajectory) {
        JSONObject trajectoryStr;
        List<Session> sessions = new ArrayList<>();
        try{
            trajectoryStr = new JSONObject(trajectory);
            JSONArray features = trajectoryStr.getJSONArray("features");

            List<String> parsedSessionIds = new ArrayList<>();
            int sessionTitleNumber = 0;
            int numberInSession = 0;
                            
            for(int i = 0; i < features.length(); i++) {

                JSONObject feature = features.getJSONObject(i);
                JSONObject properties = feature.getJSONObject("properties");


                String sessionId = properties.optString("session_id", "unknown");

                if(!parsedSessionIds.contains(sessionId)) {
                    List<TrajectorySegment> segmentsInSession = new ArrayList<>();

                    parsedSessionIds.add(sessionId);
                    sessionTitleNumber++;
                    numberInSession = 0;
                    String sessionTitle = "Trip " + sessionTitleNumber;
                    for(int j = i; j < features.length(); j++) {

                        JSONObject feature2 = features.getJSONObject(j);
                        JSONObject properties2 = feature2.getJSONObject("properties");

                        if(properties2.optString("session_id", "unknown").equals(sessionId)) {
                            numberInSession++;
                            long startTime = properties2.optLong("start_time", 0);
                            long endTime = properties2.optLong("end_time", 0);
                            int id = properties2.optInt("id", 0);
                            String activityType = properties2.optString("activity_type", "unknown");
                            JSONObject geom = feature2.optJSONObject("geometry");
                            JSONArray bbox = feature2.optJSONArray("bbox");
                            int distanceTraveled = properties2.optInt("distance_traveled", 0);
                            String iri = properties2.optString("iri", "unknown");

                            segmentsInSession.add((new TrajectorySegment(startTime, endTime, id, activityType, sessionId, geom, bbox, distanceTraveled, iri, sessionTitleNumber, numberInSession)));
                        }
                    }
                    sessions.add(new Session(sessionId, sessionTitle, segmentsInSession));
                }
            }
        }
        catch(JSONException e) {
            e.printStackTrace();
        }
        return sessions;
    }

    public List<Session> getSessions() {
        return this.sessions;
    }

    public LocalDate getDate() {
        return this.date;
    }

    public String getTrajectoryStr() {
        return this.trajectoryString;
    }

    private long differenceInTime(long startTime, long endTime) {
        return ChronoUnit.MINUTES.between(
                Instant.ofEpochMilli(startTime).atZone(ZoneId.systemDefault()).toLocalDateTime(),
                Instant.ofEpochMilli(endTime).atZone(ZoneId.systemDefault()).toLocalDateTime()
        );
    }

    public List<TrajectorySegment> getTrajectorySegments() {
        List<TrajectorySegment> segments = new ArrayList<>();
        for (Session s : sessions) {
            segments.addAll(s.getTrajectorySegments());
        }
        return segments;
    }

    public List<ActivitySummary> getActivitySummary() {
        return this.activitySummaries;
    }
}