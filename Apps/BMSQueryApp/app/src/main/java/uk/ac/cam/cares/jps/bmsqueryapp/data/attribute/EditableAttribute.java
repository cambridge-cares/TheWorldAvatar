package uk.ac.cam.cares.jps.bmsqueryapp.data.attribute;

import android.text.InputType;

public class EditableAttribute {
    String iri;
    String name;
    String type;

    String unit;

    String value;

    public EditableAttribute(String iri, String name, String type, String unit) {
        this.iri = iri;
        this.name = name;
        this.type = type;
        this.unit = unit;
    }

    public String getIri() {
        return iri;
    }

    public String getName() {
        return name;
    }

    public String getType() {
        return type;
    }

    public String getUnit() {
        return unit;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    // TODO: need to review how to get the data type from the knowledge graph
    public int getInputType() {
        if (type.equals("double")) {
            return InputType.TYPE_CLASS_NUMBER | InputType.TYPE_NUMBER_FLAG_DECIMAL;
        } else {
            return InputType.TYPE_CLASS_TEXT;
        }
    }
}
