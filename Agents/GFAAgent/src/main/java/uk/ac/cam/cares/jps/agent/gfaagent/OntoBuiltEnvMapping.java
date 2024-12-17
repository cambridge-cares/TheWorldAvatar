package uk.ac.cam.cares.jps.agent.gfaagent;

import com.opencsv.bean.CsvBindByName;

public class OntoBuiltEnvMapping {
    @CsvBindByName(column = "OntoBuiltEnv")
    private String envType;

    @CsvBindByName(column = "Value")
    private String type;

    public String getEnvType() {
        return envType;
    }

    public String getAISType() {
        return type;
    }
}
