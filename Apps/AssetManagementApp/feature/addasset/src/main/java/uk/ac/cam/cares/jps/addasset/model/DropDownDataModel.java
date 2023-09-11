package uk.ac.cam.cares.jps.addasset.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.data.OtherInfoModel;

public class DropDownDataModel extends AssetPropertyDataModel{
    boolean disallowInput;
    String valueIri = "";
    Map<String, String> labelsToIri = new HashMap<>();

    DropDownDataModel(String fieldName) {
        super(fieldName);
    }
    public boolean isDisallowInput() {
        return disallowInput;
    }
    public void setDisallowInput(boolean disallowInput) {
        this.disallowInput = disallowInput;
    }

    public String getValueIri() {
        if (!valueIri.isEmpty()) {
            return valueIri;
        }
        return labelsToIri.getOrDefault(this.fieldValue, "");
    }

    public void setValueIri(String valueIri) {
        this.valueIri = valueIri;
    }

    // NOTICE: Assume no duplication in names, and each name has only one corresponding iri
    public void setLabelsToIri(List<OtherInfoModel> options) {
        for (OtherInfoModel model : options) {
            labelsToIri.put(model.getName(), model.getIri());
        }
    }
}
