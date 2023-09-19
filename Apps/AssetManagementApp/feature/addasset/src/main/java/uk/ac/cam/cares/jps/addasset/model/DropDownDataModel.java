package uk.ac.cam.cares.jps.addasset.model;

import androidx.lifecycle.MutableLiveData;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import uk.ac.cam.cares.jps.data.OtherInfoModel;

public class DropDownDataModel extends AssetPropertyDataModel{
    boolean disallowNewItem;
    MutableLiveData<Map<String, String>> mutableLabelsToIri = new MutableLiveData<>();
    Map<String, String> labelsToIri = new HashMap<>();

    DropDownDataModel(String fieldName) {
        super(fieldName);
    }
    public boolean isDisallowNewItem() {
        return disallowNewItem;
    }
    public void setDisallowNewItem(boolean disallowNewItem) {
        this.disallowNewItem = disallowNewItem;
    }

    public String getValueIri() {
        return mutableLabelsToIri.getValue().getOrDefault(this.fieldValue, "");
    }

    // NOTICE: Assume no duplication in names, and each name has only one corresponding iri
    public void setLabelsToIri(List<OtherInfoModel> options) {
        for (OtherInfoModel model : options) {
            labelsToIri.put(model.getName(), model.getIri());
        }
    }

    public MutableLiveData<Map<String, String>> getMutableLabelsToIri() {
        return mutableLabelsToIri;
    }

    public List<String> getOrderedOptionList() {
        return mutableLabelsToIri.getValue().keySet().stream().sorted(Comparator.comparing(String::toLowerCase)).collect(Collectors.toList());
    }
}
