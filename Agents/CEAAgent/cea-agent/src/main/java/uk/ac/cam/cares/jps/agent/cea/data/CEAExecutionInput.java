package uk.ac.cam.cares.jps.agent.cea.data;
import java.util.ArrayList;
import org.json.JSONObject;

public class CEAExecutionInput {
    
    private ArrayList<CEABuildingData> buildingData;
    private CEAMetaData ceaMetaData;
    private ArrayList<String> uris;
    private Integer threadNumber;
    private String crs;
    private String ceaDatabase;
    private JSONObject solar;
    
    public CEAExecutionInput() {
    }

    public CEAExecutionInput(
            ArrayList<CEABuildingData> buildingData, 
            CEAMetaData ceaMetaData, 
            ArrayList<String> uris, 
            Integer threadNumber, 
            String crs, 
            String ceaDatabase, 
            JSONObject solar) {
        this.buildingData = buildingData;
        this.ceaMetaData = ceaMetaData;
        this.uris = uris;
        this.threadNumber = threadNumber;
        this.crs = crs;
        this.ceaDatabase = ceaDatabase;
        this.solar = solar;
    }

    public ArrayList<CEABuildingData> getBuildingData() {
        return buildingData;
    }

    public void setBuildingData(ArrayList<CEABuildingData> buildingData) {
        this.buildingData = buildingData;
    }

    public CEAMetaData getCeaMetaData() {
        return ceaMetaData;
    }

    public void setCeaMetaData(CEAMetaData ceaMetaData) {
        this.ceaMetaData = ceaMetaData;
    }

    public ArrayList<String> getUris() {
        return uris;
    }

    public void setUris(ArrayList<String> uris) {
        this.uris = uris;
    }

    public Integer getThreadNumber() {
        return threadNumber;
    }

    public void setThreadNumber(Integer threadNumber) {
        this.threadNumber = threadNumber;
    }

    public String getCrs() {
        return crs;
    }

    public void setCrs(String crs) {
        this.crs = crs;
    }

    public String getCeaDatabase() {
        return ceaDatabase;
    }

    public void setCeaDatabase(String ceaDatabase) {
        this.ceaDatabase = ceaDatabase;
    }

    public JSONObject getSolar() {
        return solar;
    }

    public void setSolar(JSONObject solar) {
        this.solar = solar;
    }
}