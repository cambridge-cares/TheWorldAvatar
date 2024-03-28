package uk.ac.cam.cares.jps.agent.gfaagent;

import com.opencsv.bean.CsvBindByName;

public class MatchingType {
     @CsvBindByName(column = "LU_DESC")
    private String LU_DESC;

    @CsvBindByName(column = "Key")
    private String key;

    @CsvBindByName(column = "Value")
    private String type;

    public String getLUType() {
        return this.LU_DESC;
    }

    public String getKey() {
        return this.key;
    }

    public String getType() {
        return this.type;
    }
}
