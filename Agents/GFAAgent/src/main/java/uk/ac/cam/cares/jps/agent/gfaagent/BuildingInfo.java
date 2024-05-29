package uk.ac.cam.cares.jps.agent.gfaagent;

public class BuildingInfo {
    
    private String type;
    private int floors;
    private String buildingIri;
    private float usageshare;

    public BuildingInfo () {
        
    }

    public String getType() {
        return this.type;
    }

    public String getBuildingIri() {
        return this.buildingIri;
    }

    public int getFloors() {
        return this.floors;
    }

    public float getUsageShare(){
        return this.usageshare;
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setFloors(int floors) {
        this.floors = floors;
    }

    public void setBuildingIri(String buildingIri) {
        this.buildingIri = buildingIri;
    }

    public void setUsageShare(float usageshare) {
        this.usageshare = usageshare;
    }
}
