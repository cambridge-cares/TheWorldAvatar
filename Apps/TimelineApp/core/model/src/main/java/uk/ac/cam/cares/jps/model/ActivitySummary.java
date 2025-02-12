package uk.ac.cam.cares.jps.model;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

public class ActivitySummary {
    private final String activity_type;
    private final String startTime;
    private final String endTime;

    public ActivitySummary(String activity_type, long start_timestamp, long end_timestamp) {
        this.activity_type = activity_type;
        this.startTime = toHoursAndMinutes(start_timestamp);
        this.endTime = toHoursAndMinutes(end_timestamp);
    }

    public String getActivityType() {
        return this.activity_type;
    }

    public String getStartTime() {
        return this.startTime;
    }

    public String getEndTime() {
        return this.endTime;
    }

    private String toHoursAndMinutes(long timestamp) {

    LocalDateTime dateTime = Instant.ofEpochMilli(timestamp)
                                    .atZone(ZoneId.systemDefault())
                                    .toLocalDateTime();

    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");

    return dateTime.format(formatter);
}

}