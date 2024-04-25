package uk.ac.cam.cares.jps.agent.buildingflooragent;

public class OSMBuilding {
    
    private String buildingUsage;
    private int floors;
    private String address;

    public String getUsage() {
        return this.buildingUsage;
    }

    public int getFloors() {
        return this.floors;
    }

    public String getAddress() {
        return this.address;
    }

    public void setUsage(String buildingUsage) {
        this.buildingUsage = buildingUsage;
    }

    public void setFloors(int floors) {
        this.floors = floors;
    }

    public void setAddress(String address) {
        this.address = address;
    }
}
