package uk.ac.cam.cares.jps.agent.gfaagent;

import com.opencsv.bean.CsvBindByName;

public class AISCost {
    @CsvBindByName(column = "Category")
    private String category;

    @CsvBindByName(column = "average")
    private double average;

    public String getCategory() {
        return category;
    }

    public double getAverage() {
        return average;
    }
}
