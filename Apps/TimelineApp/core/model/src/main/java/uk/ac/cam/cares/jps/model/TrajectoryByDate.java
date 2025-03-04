package uk.ac.cam.cares.jps.model;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.time.Instant;
import java.time.ZoneId;


public class TrajectoryByDate {
    private final String trajectoryString;
    private final List<TrajectorySegment> segments;
    private final LocalDate date;

    public TrajectoryByDate(String trajectoryStr, LocalDate date) {
        this.trajectoryString = trajectoryStr;
        this.segments = parseTrajectoryStr(trajectoryStr);
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
                int distanceTraveled = properties.optInt("distance_traveled", 0);
                String iri = properties.optString("iri", "unknown");

                segments.add(new TrajectorySegment(startTime, endTime, id, activityType, sessionId, distanceTraveled, iri));

            }
        }
        catch(JSONException e) {
            e.printStackTrace();
        }
        return segments;
    }

    public List<TrajectorySegment> getTrajectory() {
        return this.segments;
    }

    public LocalDate getDate() {
        return this.date;
    }

    public String getTrajectoryStr() {
        return this.trajectoryString;
    }

}