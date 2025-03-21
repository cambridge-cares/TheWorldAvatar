package uk.ac.cam.cares.jps.timeline.model.bottomsheet;

;

/**
 * class that represents the summary of trajectory information over a period of time
 */
public class SummaryActivityItem {
    private final String activity;
    private final int activityItem;
    private final String totalDistance; //meters
    private final String totalTimeSummary; //hours and minutes

    /**
     * constructor for summarizing trajectory info
     * @param activityItem image representing an activity type
     * @param totalDistance total distance in meters
     * @param totalTimeInMinutes total time for activity in minutes
     */
    public SummaryActivityItem(String activity, int activityItem, int totalDistance, long totalTimeInMinutes) {
        this.activity = activity;
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

    public String getActivity() {
        return activity;
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