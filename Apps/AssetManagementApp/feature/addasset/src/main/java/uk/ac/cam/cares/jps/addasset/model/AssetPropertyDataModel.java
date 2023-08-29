package uk.ac.cam.cares.jps.addasset.model;

public class AssetPropertyDataModel {
    // data
    String fieldValue;
    String fieldIri;
    String fieldName;
    // ui related

    public enum ViewType {
        INPUT_FIELD,
        DROP_DOWN,
        DOCUMENT
    }
    ViewType type;
    boolean isRequired;
    boolean disallowInput;
    boolean isMultiLine;
    String helperText;

    AssetPropertyDataModel(String fieldName, ViewType type) {
        this.fieldName = fieldName;
        this.type = type;
    }

    AssetPropertyDataModel(String fieldName, boolean isRequired, boolean disallowInput, ViewType type) {
        this.fieldName = fieldName;
        this.isRequired = isRequired;
        this.type = type;
        this.disallowInput = disallowInput;
    }

    AssetPropertyDataModel(String fieldName, boolean isRequired, boolean disallowInput, ViewType type, String helperText) {
        this.fieldName = fieldName;
        this.isRequired = isRequired;
        this.helperText = helperText;
        this.disallowInput = disallowInput;
        this.type = type;
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

    public String getHelperText() {
        return helperText;
    }

    public void setFieldValue(String fieldValue) {
        this.fieldValue = fieldValue;
    }

    public boolean isDisallowInput() {
        return disallowInput;
    }

    public void setRequired(boolean required) {
        isRequired = required;
    }

    public void setDisallowInput(boolean disallowInput) {
        this.disallowInput = disallowInput;
    }

    public void setMultiLine(boolean multiLine) {
        isMultiLine = multiLine;
    }

    public boolean isMultiLine() {
        return isMultiLine;
    }
}
