package uk.ac.cam.cares.jps.bmsqueryapp.data.buildings;

public class Equipment extends Instance{
    String type;

    public Equipment(String iri, String label, String type) {
        this.iri = iri;
        this.label = label;
        this.type = type;
    }

    public String getType() {
        return type;
    }

    // no sublevel items, so it is an empty list
}
