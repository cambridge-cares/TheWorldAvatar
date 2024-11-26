package uk.ac.cam.cares.jps.agent.buildingflooragent;

public class OSMBuilding {

    private String buildingIri;
    private String buildingUsage;
    private String street;
    private boolean hasAddress;
    private boolean hasUsage;
    private String streetNumber;
    private FloorCategory floorCat;
    private int floors;

    public OSMBuilding(String buildingIri) {
        this.buildingIri = buildingIri;
    }

    public void setAddress(String streetNumber, String street) {
        this.streetNumber = streetNumber;
        this.street = street;
        hasAddress = true;
    }

    public String getStreetNumber() {
        return streetNumber;
    }

    public String getBuildingIri() {
        return this.buildingIri;
    }

    public String getUsage() {
        return this.buildingUsage;
    }

    public boolean hasAddress() {
        return hasAddress;
    }

    public boolean hasUsage() {
        return hasUsage;
    }

    public String getStreet() {
        return this.street;
    }

    public void setBuildingIri(String buildingIri) {
        this.buildingIri = buildingIri;
    }

    public void setUsage(String buildingUsage) {
        this.buildingUsage = buildingUsage;
        hasUsage = true;
    }

    public void setStreet(String street) {
        this.street = street;
    }

    public void setFloorCategory(FloorCategory floorCat) {
        this.floorCat = floorCat;
    }

    public FloorCategory getFloorCategory() {
        return floorCat;
    }

    public void setFloors(int floors) {
        this.floors = floors;
    }

    public int getFloors() {
        return floors;
    }

    enum FloorCategory {
        A, B, C
    }
}
