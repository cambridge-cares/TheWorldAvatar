package uk.ac.cam.cares.jps.sensor.source.database.model.entity;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import org.json.JSONException;
import org.json.JSONObject;

@Entity(tableName = "activity_data")
public class ActivityData extends SensorData {

    public String activityType;
    @NonNull
    public int confidence;
    @NonNull
    public int uploaded;


    public ActivityData(String activityType, int confidence, long timestamp) {
        this.activityType = activityType;
        this.confidence = confidence;
        this.time = timestamp * 1_000_000L;
        this.uploaded = 0;
    }

    public ActivityData(){
        super();
        this.uploaded = 0;
    }

    @Override
    public String toJSONString() {
        return "";
    }

    @Override
    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        try {
            json.put("name", "activity");


            JSONObject values = new JSONObject();
            values.put("type", this.activityType);
            values.put("confidence", this.confidence);

            json.put("values", values);
            json.put("time", this.time);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return json;
    }
}