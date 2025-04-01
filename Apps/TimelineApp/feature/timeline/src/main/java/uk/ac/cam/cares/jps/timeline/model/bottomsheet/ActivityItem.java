package uk.ac.cam.cares.jps.timeline.model.bottomsheet;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

/**
 * class to represent one segment of activity and its time summary
 */
public class ActivityItem extends Activity {
    private final int id;
    private final String timeSummary;
    private boolean clicked;


    /**
     * Constructor for ActivityItem class
     * @param activityType the type of activity as an image
     * @param startTime the start timestamp of the segment
     * @param endTime the end timestamp of the segment
     */
    public ActivityItem(int id, String activityType, long startTime, long endTime) {
        super(activityType);
        this.id = id;
        this.timeSummary = toTimeSummary(startTime, endTime);
        this.clicked = false;

    }

    String toHoursAndMinutes(long timestamp) {

        LocalDateTime dateTime = Instant.ofEpochMilli(timestamp)
                .atZone(ZoneId.systemDefault())
                .toLocalDateTime();

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");

        return dateTime.format(formatter);
    }


    private String toTimeSummary(long start_time, long end_time) {
        return toHoursAndMinutes(start_time) + " - " + toHoursAndMinutes(end_time);
    }
    

    public String getTimeSummary() {
        return timeSummary;
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