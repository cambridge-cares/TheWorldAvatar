package uk.ac.cam.cares.jps.model;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;

public class ActivityItem {
    private int activity_type;
    private String time_summary;


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

    public Integer getActivityType() {
        return activity_type;
    }

    public String getTimeSummary() {
        return time_summary;
    }


    public int getActivityImage() {
        return this.activity_type;
    }
} 