package uk.ac.cam.cares.jps.agent.flood.objects;

import java.util.HashMap;
import java.util.Map;

public class Measure {
    private String iri;
    private double typicalRangeHigh;
    private double typicalRangeLow;
    private String parameterName;
    private String unit;
    private String qualifier;
    private String trend;
    private String range;

    private static Map<String,String> rangeDisplayTextMap = new HashMap<>();

    static {
        rangeDisplayTextMap.put("https://www.theworldavatar.com/kg/ontoems/LowRange","Low");
        rangeDisplayTextMap.put("https://www.theworldavatar.com/kg/ontoems/HighRange","High");
        rangeDisplayTextMap.put("https://www.theworldavatar.com/kg/ontoems/NormalRange","Normal");
        rangeDisplayTextMap.put("https://www.theworldavatar.com/kg/ontoems/UnavailableRange","Unavailable");
    }

    private static Map<String,String> trendDisplayTextMap = new HashMap<>(); 
    static {
        trendDisplayTextMap.put("https://www.theworldavatar.com/kg/ontoems/Steady","Steady");
        trendDisplayTextMap.put("https://www.theworldavatar.com/kg/ontoems/Rising","Rising");
        trendDisplayTextMap.put("https://www.theworldavatar.com/kg/ontoems/Falling","Falling");
        trendDisplayTextMap.put("https://www.theworldavatar.com/kg/ontoems/UnavailableTrend","Unavailable");
    }

    public Measure(String iri) {
        this.iri = iri;
    }

    public void setTypicalRangeHigh(double typicalRangeHigh) {
        this.typicalRangeHigh = typicalRangeHigh;
    }

    public void setTypicalRangeLow(double typicalRangeLow) {
        this.typicalRangeLow = typicalRangeLow;
    }

    public String getIri() {
        return this.iri;
    }

    public double getRangeHigh() {
        return this.typicalRangeHigh;
    }

    public double getRangeLow() {
        return this.typicalRangeLow;
    }

    public void setParameterName(String parameterName) {
        this.parameterName = parameterName;
    }

    public String getParameterName() {
        return this.parameterName;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }

    public String getUnit() {
        return this.unit;
    }

    public void setQualifier(String qualifier) {
        this.qualifier = qualifier;
    }

    public String getQualifier() {
        return this.qualifier;
    }

    public void setTrend(String trend) {
        this.trend = trend;
    }

    public String getTrend() {
        return trendDisplayTextMap.get(this.trend);
    }

    public void setRange(String range) {
        this.range = range;
    }

    public String getRange() {
        return rangeDisplayTextMap.get(this.range);
    }
}
