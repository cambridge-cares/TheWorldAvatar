package uk.ac.cam.cares.jps.model;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

/**
 * class to represent one segment of activity and its time summary
 */
public class ActivityItem {
    private final int activity_type;
    private final String time_summary;


    /**
     * Constructor for ActivityItem class
     * @param activity_type the type of activity as an image
     * @param start_time the start timestamp of the segment
     * @param end_time the end timestamp of the segment
     */
    public ActivityItem(int activity_type, long start_time, long end_time) {
        this.activity_type = activity_type;
        this.time_summary = toTimeSummary(start_time, end_time);

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
        return time_summary;
    }


    public int getActivityImage() {
        return this.activity_type;
    }
} 