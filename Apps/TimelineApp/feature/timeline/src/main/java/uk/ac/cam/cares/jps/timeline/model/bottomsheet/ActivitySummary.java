package uk.ac.cam.cares.jps.timeline.model.bottomsheet;

/**
 * class that represents the summary of trajectory information over a period of time
 */
public class ActivitySummary extends Activity {
    private final String totalDistance; //meters
    private final String totalTimeSummary; //hours and minutes

    public ActivitySummary(String activityType, int totalDistance, long totalTime) {
        super(activityType);
        this.totalDistance = totalDistance + " m";
        this.totalTimeSummary = toHoursAndMinutes(totalTime);
    }





    String toHoursAndMinutes(long totalTimeInMinutes) {
        int hours = (int) (totalTimeInMinutes / 60);
        int minutes = (int) (totalTimeInMinutes - (hours * 60));
        if(hours > 0) {
            return hours + " hr " + minutes + " min";
        }
        else return  minutes + " min";
    }

    public String getTotalDistance() {
        return this.totalDistance;
    }

    public String getTimeSummary() {
        return this.totalTimeSummary;
    }

}