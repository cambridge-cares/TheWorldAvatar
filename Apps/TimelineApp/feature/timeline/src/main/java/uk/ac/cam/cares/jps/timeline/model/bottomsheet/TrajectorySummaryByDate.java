package uk.ac.cam.cares.jps.timeline.model.bottomsheet;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.timeline.model.trajectory.TrajectorySegment;


public class TrajectorySummaryByDate {
    LocalDate date;
    List<ActivitySummary> activitySummary;
    List<Session> uniqueSessions;

    public TrajectorySummaryByDate(LocalDate date, List<ActivitySummary> activitySummary, List<Session> uniqueSessions) {
        this.date = date;
        this.activitySummary = activitySummary;
        this.uniqueSessions = uniqueSessions;
    }

    public LocalDate getDate() {
        return date;
    }

    public List<ActivitySummary> getActivitySummary() {
        return activitySummary;
    }

    public List<Session> getUniqueSessions() {
        return uniqueSessions;
    }

    
}