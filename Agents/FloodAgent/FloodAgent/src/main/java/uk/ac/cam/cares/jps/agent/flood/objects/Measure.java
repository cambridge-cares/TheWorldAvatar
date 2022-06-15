package uk.ac.cam.cares.jps.agent.flood.objects;

public class Measure {
    private String iri;
    private double typicalRangeHigh;
    private double typicalRangeLow;
    private String parameterName;
    private String unit;
    private String qualifier;

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
}
