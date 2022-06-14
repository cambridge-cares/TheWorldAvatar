package uk.ac.cam.cares.jps.agent.flood.objects;

public class Measure {
    private String iri;
    private double typicalRangeHigh;
    private double typicalRangeLow;

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
}
