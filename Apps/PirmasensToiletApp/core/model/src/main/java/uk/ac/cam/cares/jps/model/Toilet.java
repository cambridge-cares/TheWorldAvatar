package uk.ac.cam.cares.jps.model;

import com.mapbox.geojson.Point;

import java.util.HashMap;
import java.util.Map;

public class Toilet {
    Point location;
    Boolean hasMale;
    Boolean hasFemale;
    String fee;
    String wheelchair;
    String name;
    Price price;
    String openTime;
    String endTime;

    Map<String, String> otherInfo = new HashMap<>();

    public Toilet(double lng, double lat) {
        location = Point.fromLngLat(lng, lat);
    }

    public Point getLocation() {
        return location;
    }

    public void setLocation(Point location) {
        this.location = location;
    }

    public Boolean getHasMale() {
        return hasMale;
    }

    public void setHasMale(Boolean hasMale) {
        this.hasMale = hasMale;
    }

    public Boolean getHasFemale() {
        return hasFemale;
    }

    public void setHasFemale(Boolean hasFemale) {
        this.hasFemale = hasFemale;
    }

    public String getFee() {
        return fee;
    }

    public void setFee(String fee) {
        this.fee = fee;
    }

    public String getWheelchair() {
        return wheelchair;
    }

    public void setWheelchair(String wheelchair) {
        this.wheelchair = wheelchair;
    }

    public Map<String, String> getOtherInfo() {
        return otherInfo;
    }

    public void addOtherInfo(String infoName, String infoValue) {
        this.otherInfo.put(infoName, infoValue);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Price getPrice() {
        return price;
    }

    public void setPrice(Price price) {
        this.price = price;
    }

    public void setOpenTime(String opensOn) {
        openTime = opensOn;
    }

    public void setEndTime(String endsOn) {
        endTime = endsOn;
    }

    public String getOpenTime() {
        return openTime;
    }

    public String getEndTime() {
        return endTime;
    }
}