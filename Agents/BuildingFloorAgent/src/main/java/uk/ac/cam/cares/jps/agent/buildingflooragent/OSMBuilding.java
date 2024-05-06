package uk.ac.cam.cares.jps.agent.buildingflooragent;

public class OSMBuilding {
    
    private String buildiingIri;
    private String buildingUsage;
    private String street;
    private String unit;
    private String postcode;

    public OSMBuilding () {

    }

    public String getBuildingIri() {
        return this.buildiingIri;
    }

    public String getUsage() {
        return this.buildingUsage;
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
        this.buildiingIri = buildingIri;
    }

    public void setUsage(String buildingUsage) {
        this.buildingUsage = buildingUsage;
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
