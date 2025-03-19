package uk.ac.cam.cares.jps.timeline.model.bottomsheet;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

/**
 * class to represent one segment of activity and its time summary
 */
public class ActivityItem {
    private final int id;
    private final int activityType;
    private final String activity;
    private final String timeSummary;
    private boolean clicked;


    /**
     * Constructor for ActivityItem class
     * @param activityType the type of activity as an image
     * @param startTime the start timestamp of the segment
     * @param endTime the end timestamp of the segment
     */
    public ActivityItem(int id, String activity, int activityType, long startTime, long endTime) {
        this.id = id;
        this.activity = activity;
        this.activityType = activityType;
        this.timeSummary = toTimeSummary(startTime, endTime);
        this.clicked = false;

    }


    private String toTimeSummary(long start_time, long end_time) {
        return toHoursAndMinutes(start_time) + " - " + toHoursAndMinutes(end_time);
    }

    private String toHoursAndMinutes(long timestamp) {

        LocalDateTime dateTime = Instant.ofEpochMilli(timestamp)
                .atZone(ZoneId.systemDefault())
                .toLocalDateTime();

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");

        return dateTime.format(formatter);
    }

    public String getTimeSummary() {
        return timeSummary;
    }

    public String getActivity() {
        return activity;
    }


    public int getActivityImage() {
        return this.activityType;
    }

    public int getId() {
        return this.id;
    }

    public void setClicked(boolean click) {
        this.clicked = click;
    }

    public boolean getClicked() {
        return this.clicked;
    }

} 