package uk.ac.cam.cares.jps.addasset.model;

import androidx.lifecycle.MutableLiveData;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class DropDownDataModel extends AssetPropertyDataModel{
    // NOTICE: Assume no duplication in names, and each name has only one corresponding iri
    MutableLiveData<Map<String, String>> mutableLabelsToIri = new MutableLiveData<>(new HashMap<>());
    MutableLiveData<Boolean> showDisallowError = new MutableLiveData<>(false);

    DropDownDataModel(String fieldName) {
        super(fieldName);
    }
    public MutableLiveData<Boolean> getShowDisallowError() {
        return showDisallowError;
    }

    public String getValueIri() {
        return mutableLabelsToIri.getValue().getOrDefault(this.fieldValue, "");
    }

    public MutableLiveData<Map<String, String>> getMutableLabelsToIri() {
        return mutableLabelsToIri;
    }

    public List<String> getOrderedOptionList() {
        return mutableLabelsToIri.getValue().keySet().stream().sorted(Comparator.comparing(String::toLowerCase)).collect(Collectors.toList());
    }
}
