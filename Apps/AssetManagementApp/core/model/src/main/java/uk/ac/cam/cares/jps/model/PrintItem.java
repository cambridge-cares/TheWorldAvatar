package uk.ac.cam.cares.jps.model;

public class PrintItem {
    String inventoryID;
    String label;
    String iri;

    public PrintItem(String inventoryID, String label, String iri) {
        this.inventoryID = inventoryID;
        this.label = label;
        this.iri = iri;
    }

    public String getInventoryID() {
        return inventoryID;
    }

    public String getLabel() {
        return label;
    }

    public String getIri() {
        return iri;
    }

}
