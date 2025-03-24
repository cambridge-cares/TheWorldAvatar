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
import uk.ac.cam.cares.jps.timeline.model.bottomsheet.TrajectorySummaryByDate;


public class TrajectoryByDate {
    private final String trajectoryString;
    //private final List<TrajectorySegment> segments;
    private final LocalDate date;
    private final List<Session> sessions;

    public TrajectoryByDate(String trajectoryStr, LocalDate date) {
        this.trajectoryString = trajectoryStr;
        this.sessions = parseForUniqueSessions(trajectoryStr); //sessionId, sessionTitle, List<Segments>
        this.date = date;
    }

    private List<TrajectorySegment> parseTrajectoryStr(String trajectory) {
        JSONObject trajectoryStr;
        List<TrajectorySegment> segments = new ArrayList<>();
        try {
            trajectoryStr = new JSONObject(trajectory);
            JSONArray features = trajectoryStr.getJSONArray("features");



            for(int i = 0; i < features.length(); i++) {
                JSONObject feature = features.getJSONObject(i);
                JSONObject properties = feature.getJSONObject("properties");

                long startTime = properties.optLong("start_time", 0);
                long endTime = properties.optLong("end_time", 0);
                int id = properties.optInt("id", 0);
                String activityType = properties.optString("activity_type", "unknown");
                String sessionId = properties.optString("session_id", "unknown");
                JSONObject geom = feature.optJSONObject("geometry");
                int distanceTraveled = properties.optInt("distance_traveled", 0);
                String iri = properties.optString("iri", "unknown");

                segments.add(new TrajectorySegment(startTime, endTime, id, activityType, sessionId, geom, distanceTraveled, iri));

            }
        }
        catch(JSONException e) {
            e.printStackTrace();
        }
        return segments;
    }

    private List<Session> parseForUniqueSessions(String trajectory) {
        JSONObject trajectoryStr;
        List<Session> sessions = new ArrayList<>();
        try{
            trajectoryStr = new JSONObject(trajectory);
            JSONArray features = trajectoryStr.getJSONArray("features");

            List<String> parsedSessionIds = new ArrayList<>();

            for(int i = 0; i < features.length(); i++) {
                
                JSONObject feature = features.getJSONObject(i);
                JSONObject properties = feature.getJSONObject("properties");

                int sessionTitleNumber = 0;
                String sessionId = properties.optString("session_id", "unknown");

                if(!parsedSessionIds.contains(sessionId)) {
                    List<TrajectorySegment> segmentsInSession = new ArrayList<>();

                    parsedSessionIds.add(sessionId);
                    sessionTitleNumber++;
                    String sessionTitle = "Title " + sessionTitleNumber;
                    for(int j = i; j < features.length(); j++) {

                        JSONObject feature2 = features.getJSONObject(i);
                        JSONObject properties2 = feature.getJSONObject("properties");

                        if(properties2.optString("session_id", "unknown").equals(sessionId)) {
                            long startTime = properties2.optLong("start_time", 0);
                            long endTime = properties2.optLong("end_time", 0);
                            int id = properties2.optInt("id", 0);
                            String activityType = properties2.optString("activity_type", "unknown");
                            JSONObject geom = feature2.optJSONObject("geometry");
                            int distanceTraveled = properties2.optInt("distance_traveled", 0);
                            String iri = properties2.optString("iri", "unknown");

                            segmentsInSession.add((new TrajectorySegment(startTime, endTime, id, activityType, sessionId, geom, distanceTraveled, iri)));
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

    public TrajectorySummaryByDate parseSessionSummaries() {
        List<Session> uniqueSessions = parseUniqueSessions();
        List<ActivitySummary> summaryActivityItems = parseActivitySummary();
        LocalDate date = this.date;

        TrajectorySummaryByDate sessionSummary = new TrajectorySummaryByDate(date, summaryActivityItems, uniqueSessions);

        return sessionSummary;
    }

    private List<Session> parseUniqueSessions() {

        List<Session> uniqueSessions = new ArrayList<>();
        List<Session> sessions = this.sessions;

        for(Session session : sessions) {
                uniqueSessions.add(session);
        }

        return uniqueSessions;
    }


    private List<ActivityItem> parseActivityItemsBySession(String sessionId) {

        for (Session s : this.sessions) {
            if (s.getSessionId().equals(sessionId)) {
                return s.getActivityList();
            }
        }
        return null;
    }

    private List<ActivitySummary> parseActivitySummary() {
        List<ActivitySummary> summaries = new ArrayList<>();

        Map<String, List<Integer>> distancePerActivityType = new HashMap<>();
        Map<String, List<Long>> timePerActivityType = new HashMap<>();
        long startTime = 0;

        List<Session> sessions = this.sessions;

        List<TrajectorySegment> trajectorySegments = getTrajectorySegments();
            for (TrajectorySegment segment : trajectorySegments) {

                String activityType = segment.activityType();
                startTime = segment.startTime();
                long endTime = segment.endTime();
                int distance = segment.distanceTraveled();

                long minutes = (endTime > startTime) ? differenceInTime(startTime, endTime) : 0;

                timePerActivityType.putIfAbsent(activityType, new ArrayList<>());
                Objects.requireNonNull(timePerActivityType.get(activityType)).add(minutes);

                distancePerActivityType.putIfAbsent(activityType, new ArrayList<>());
                Objects.requireNonNull(distancePerActivityType.get(activityType)).add(distance);
            }
        for (String activity : distancePerActivityType.keySet()) {
            int totalDistance = activity.equals("still") ? 0 : Objects.requireNonNull(distancePerActivityType.get(activity)).stream().mapToInt(Integer::intValue).sum();
            long totalTime = Objects.requireNonNull(timePerActivityType.get(activity)).stream().mapToLong(Long::longValue).sum();;

            summaries.add(new ActivitySummary(activity, totalDistance, totalTime));
        }

        return summaries;
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
            for(TrajectorySegment segment : s.getTrajectorySegments()) {
                segments.add(segment);
            }
        }
        return segments;
    }

}