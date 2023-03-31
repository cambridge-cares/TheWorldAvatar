package uk.ac.cam.cares.jps.bmsqueryapp.data;

public class Equipment extends Instance{
    String type;

    public Equipment(String iri, String label, String type) {
        this.iri = iri;
        this.label = label;
        this.type = type;
    }

    // no sublevel items, so it is an empty list
}
