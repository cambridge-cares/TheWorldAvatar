package uk.ac.cam.cares.jps.agent.gfaagent;

import java.util.List;
public class BuildingInfo {
    
    
    private int floors;
    private String buildingIri;
    private List<BuildingUsageInfo> usageInfo;
    

    public BuildingInfo () {
        
    }

   

    public String getBuildingIri() {
        return this.buildingIri;
    }

    public int getFloors() {
        return this.floors;
    }

    public List<BuildingUsageInfo> getBuildingUsageInfo(){
        return this.usageInfo;
    }
    

    public void setFloors(int floors) {
        this.floors = floors;
    }

    public void setBuildingIri(String buildingIri) {
        this.buildingIri = buildingIri;
    }

    public void setBuildingUisageInfo(List<BuildingUsageInfo> usageInfo) {
        this.usageInfo = usageInfo;
    }

    
}
