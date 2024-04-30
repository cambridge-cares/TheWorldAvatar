package uk.ac.cam.cares.jps.agent.buildingflooragent;

public class OSMBuilding {
    
    private String buildiingIri;
    private String buildingUsage;
    private int floors;
    private String street;
    private String unit;
    private Integer postcode;

    public String getBuildingIri() {
        return this.buildiingIri;
    }

    public String getUsage() {
        return this.buildingUsage;
    }

    public int getFloors() {
        return this.floors;
    }

    public String getStreet() {
        return this.street;
    }

    public Integer getPostcode() {
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

    public void setFloors(int floors) {
        this.floors = floors;
    }

    public void setStreet(String street) {
        this.street = street;
    }

    public void setPostcode(Integer postcode) {
        this.postcode = postcode;
    }

    public void seUnit(String unit) {
        this.unit = unit;
    }
}
