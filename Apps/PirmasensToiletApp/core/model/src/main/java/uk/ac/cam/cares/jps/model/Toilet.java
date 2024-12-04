package uk.ac.cam.cares.jps.model;

import com.mapbox.geojson.Point;

import java.util.HashMap;
import java.util.Map;

public class Toilet {
    Point location;
    Boolean hasMale;
    Boolean hasFemale;
    String wheelchair;
    String name;
    String street_address;
    String locality;
    String postal_code;
    Price price;


    String averageRating;

    String image;

    String id;


    Map<String, String> otherInfo = new HashMap<>();

    @Override
    public String toString() {
        return "Toilet{" +
                "location=" + location +
                ", hasMale=" + hasMale +
                ", hasFemale=" + hasFemale + '\'' +
                ", wheelchair='" + wheelchair + '\'' +
                ", name='" + name + '\'' +
                ", StreetAddress='" + street_address + '\'' +
                ", locality='" + locality +
                ", PostalCode='" + postal_code +
                ", price=" + price + '\'' +
                ", otherInfo=" + otherInfo +
                ", hasImage=" + image +
                '}';
    }


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

    public String getImage() {
        return image;
    }

    public void setImage(String image) {
        this.image = image;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setPrice(Price price) {
        this.price = price;
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

    public String getStreetAddress() {
        return street_address;
    }

    public String getLocality() {
        return locality;
    }

    public String getPostalCode() {
        return postal_code;
    }

    public String getAddress() {
        return getStreetAddress() + ", " + getPostalCode() + " " + getLocality();
    }

    public String getAverageRating() {
        return averageRating;
    }

    public void setAverageRating(String averageRating) {
        this.averageRating = averageRating;
    }

    public void setStreetAddress(String street_address) {
        this.street_address = street_address;
    }

    public void setLocality(String locality) {
        this.locality = locality;
    }

    public void setPostalCode(String postal_code) {
        this.postal_code = postal_code;
    }
}