package uk.ac.cam.cares.jps.agent.gfaagent;

import com.opencsv.bean.CsvBindByName;

public class MatchingType {
     @CsvBindByName(column = "OntoBuiltEnv")
    private String envType;

    @CsvBindByName(column = "Key")
    private String key;

    @CsvBindByName(column = "Value")
    private String type;

    public String getenvType() {
        return this.envType;
    }

    public String getKey() {
        return this.key;
    }

    public String getType() {
        return this.type;
    }
}
