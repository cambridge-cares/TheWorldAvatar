package uk.ac.cam.cares.jps.timeline.model.bottomsheet;

import uk.ac.cam.cares.jps.timelinemap.R;

public abstract class Activity {
    String activityType;
    int activityImage;

    Activity(String activityType) {
        this.activityType = activityType;
        this.activityImage = setActivityImage(activityType);
    }

    private int setActivityImage(String activity) {
       int activityImage = R.drawable.baseline_arrow_circle_right_24;
        if (activity.equals("walking")) activityImage = R.drawable.baseline_directions_walk_24;
        else if (activity.equals("vehicle")) activityImage = R.drawable.baseline_directions_car_24;
        else if (activity.equals("bike")) activityImage = R.drawable.baseline_directions_bike_24;
        else if (activity.equals("still")) activityImage = R.drawable.baseline_man_24;
        else if (activity.equals("unknown")) activityImage = R.drawable.baseline_arrow_circle_right_24;

        return activityImage;
    }

    abstract String toHoursAndMinutes(long timestamp);

    
    public String getActivityType() {
        return activityType;
    }

    public int getActivityImage() {
        return this.activityImage;
    }

    public abstract String getTimeSummary();

    

}