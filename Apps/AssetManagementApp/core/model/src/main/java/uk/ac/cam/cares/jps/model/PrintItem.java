package uk.ac.cam.cares.jps.model;

public class PrintItem {
    String inventoryID;
    String label;
    String iri;
    Boolean status;

    public PrintItem(String inventoryID, String label, String iri, Boolean status) {
        this.inventoryID = inventoryID;
        this.label = label;
        this.iri = iri;
        this.status = status;
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

    public Boolean getStatus() {
        return status;
    }
}
