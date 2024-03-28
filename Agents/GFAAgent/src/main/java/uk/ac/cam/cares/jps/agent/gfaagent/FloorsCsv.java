package uk.ac.cam.cares.jps.agent.gfaagent;

import com.opencsv.bean.CsvBindByName;

public class FloorsCsv {

    @CsvBindByName(column = "blk_no")
    private String blk;

    @CsvBindByName(column = "street")
    private String street;

    @CsvBindByName(column = "max_floor_lvl")
    private int floors;

    public String getBLK() {
        return this.blk;
    }

    public String getStreet() {
        return this.street;
    }

    public int getFloors() {
        return this.floors;
    }
}
