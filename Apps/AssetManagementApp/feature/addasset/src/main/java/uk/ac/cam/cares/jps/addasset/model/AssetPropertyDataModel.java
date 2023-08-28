package uk.ac.cam.cares.jps.addasset.model;

public class AssetPropertyDataModel {
    // data
    String fieldValue;
    String fieldIri;
    String fieldName;
    // ui related

    public static enum ViewType {
        INPUT_FIELD,
        DROP_DOWN,
        DOCUMENT
    }
    ViewType type;
    boolean isRequired;
    boolean isSearchable;
    String hint;

    AssetPropertyDataModel(String fieldName, boolean isRequired, boolean isSearchable, ViewType type) {
        this.fieldName = fieldName;
        this.isRequired = isRequired;
        this.isSearchable = isSearchable;
        this.type = type;
    }

    AssetPropertyDataModel(String fieldName, boolean isRequired, boolean isSearchable, String hint) {
        this.fieldName = fieldName;
        this.isRequired = isRequired;
        this.isSearchable = isSearchable;
        this.hint = hint;
    }


    public String getFieldName() {
        return fieldName;
    }

    public String getFieldValue() {
        return fieldValue;
    }

    public String getFieldIri() {
        return fieldIri;
    }

    public ViewType getType() {
        return type;
    }

    public boolean isRequired() {
        return isRequired;
    }

    public boolean isSearchable() {
        return isSearchable;
    }

    public String getHint() {
        return hint;
    }

    public void setFieldValue(String fieldValue) {
        this.fieldValue = fieldValue;
    }
}
