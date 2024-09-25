package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.room.Entity;
import androidx.room.PrimaryKey;

@Entity(tableName = "activity_data")
public class ActivityData {
    @PrimaryKey
    public long timestamp;
    public String activityType;
    public int confidence;


    public ActivityData(String activityType, int confidence, long timestamp) {
        this.activityType = activityType;
        this.confidence = confidence;
        this.timestamp = timestamp;
    }
}