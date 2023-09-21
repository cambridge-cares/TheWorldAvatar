package uk.ac.cam.cares.jps.data;

public class PrintItemModel {
    String inventoryID;
    String label;
    String iri;
    Boolean status;

    public PrintItemModel(String inventoryID, String label, String iri, Boolean status) {
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
