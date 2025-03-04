package uk.ac.cam.cares.jps.model;

import java.time.LocalDate;
import java.util.List;

public class SessionSummaryByDate {
    LocalDate date;
    List<SummaryActivityItem> activitySummary;
    List<UniqueSessions> uniqueSessions;

    public SessionSummaryByDate(LocalDate date, List<SummaryActivityItem> activitySummary, List<UniqueSessions> uniqueSessions) {
        this.date = date;
        this.activitySummary = activitySummary;
        this.uniqueSessions = uniqueSessions;
    }

    public LocalDate getDate() {
        return date;
    }

    public List<SummaryActivityItem> getActivitySummary() {
        return activitySummary;
    }

    public List<UniqueSessions> getUniqueSessions() {
        return uniqueSessions;
    }
}