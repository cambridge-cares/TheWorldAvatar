package uk.ac.cam.cares.jps.agent.gfaagent;

public class BuildingUsageInfo {
    private String usage;
    private float usageshare;
    private float unitCost;

    public BuildingUsageInfo() {}
    
    public String getUsage() {
        return this.usage;
    }

    public float getUsageShare(){
        return this.usageshare;
    }

    public float getUnitCost() {
        return this.unitCost;
    }

    public void setUsge(String usage) {
        this.usage = usage;
    }

    public void setUsageShare(float usageshare) {
        this.usageshare = usageshare;
    }

    public void setUnitCost (float unitCost) {
        this.unitCost = unitCost;
    }
}
