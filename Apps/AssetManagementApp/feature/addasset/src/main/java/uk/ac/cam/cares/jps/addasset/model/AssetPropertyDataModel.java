package uk.ac.cam.cares.jps.addasset.model;

public class AssetPropertyDataModel {
    // data
    String fieldValue;

    String fieldName;
    // ui related

    boolean isRequired;

    boolean isMultiLine;
    String helperText;

    AssetPropertyDataModel(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getFieldName() {
        return fieldName;
    }

    public String getFieldValue() {
        return fieldValue;
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

    public void setRequired(boolean required) {
        isRequired = required;
    }

    public void setMultiLine(boolean multiLine) {
        isMultiLine = multiLine;
    }

    public boolean isMultiLine() {
        return isMultiLine;
    }


}
