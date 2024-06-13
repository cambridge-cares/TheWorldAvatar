package uk.ac.cam.cares.jps.agent.buildingflooragent;

public class OSMBuilding {

    private String buildingIri;
    private String buildingUsage;
    private String street;
    private String unit;
    private String postcode;
    private boolean hasAddress;
    private boolean hasUsage;
    private String streetNumber;

    public OSMBuilding(String buildingIri) {
        this.buildingIri = buildingIri;
    }

    public OSMBuilding() {
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

    public String getPostcode() {
        return this.postcode;
    }

    public String getUnit() {
        return this.unit;
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

    public void setPostcode(String postcode) {
        this.postcode = postcode;
    }

    public void seUnit(String unit) {
        this.unit = unit;
    }
}
