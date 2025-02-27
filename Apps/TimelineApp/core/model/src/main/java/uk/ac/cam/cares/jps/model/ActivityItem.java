package uk.ac.cam.cares.jps.model;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

/**
 * class to represent one segment of activity and its time summary
 */
public class ActivityItem {
    private final int activityType;
    private final String timeSummary;


    /**
     * Constructor for ActivityItem class
     * @param activityType the type of activity as an image
     * @param start_time the start timestamp of the segment
     * @param end_time the end timestamp of the segment
     */
    public ActivityItem(int activityType, long start_time, long end_time) {
        this.activityType = activityType;
        this.timeSummary = toTimeSummary(start_time, end_time);

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


    public int getActivityImage() {
        return this.activityType;
    }
} 