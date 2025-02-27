package uk.ac.cam.cares.jps.model;

public class SummaryActivityItem {
    private final int activityItem;
    private final String totalDistance; //meters
    private final String totalTimeSummary; //hours and minutes

    public SummaryActivityItem(int activityItem, int totalDistance, long totalTimeInMinutes) {
        this.activityItem = activityItem;
        this.totalDistance = totalDistance + " m";
        this.totalTimeSummary = toMinutesAndHours(totalTimeInMinutes);
    }

    private String toMinutesAndHours(long totalTimeInMinutes) {
        int hours = (int) (totalTimeInMinutes / 60);
        int minutes = (int) (totalTimeInMinutes - (hours * 60));
        if(hours > 0) {
            return hours + " hr " + minutes + " min";
        }
        else return  minutes + " min";
    }

    public int getActivityImage() {
        return this.activityItem;
    }

    public String getTotalDistance() {
        return this.totalDistance;
    }

    public String getTotalTimeSummary() {
        return this.totalTimeSummary;
    }
}