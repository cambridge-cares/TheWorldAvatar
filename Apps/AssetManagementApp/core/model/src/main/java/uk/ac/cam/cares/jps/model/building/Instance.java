package uk.ac.cam.cares.jps.model.building;

import androidx.annotation.NonNull;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class Instance {

    String label;
    String iri;

    List<Instance> subLevelItems = new ArrayList<>();

    public Instance() {}
    public Instance(String label) {
        this.label = label;
    }
    public Instance(String iri, String label) {
        this.iri = iri;
        this.label = label;
    }

    public String getLabel() {
        return label;
    }

    public String getIri() {
        return iri;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public List<Instance> getSortedSubLevelItems() {
        return subLevelItems.stream()
                .sorted(Comparator.comparing(o -> o.label))
                .collect(Collectors.toList());
    }

    public void addToSublist(Instance item) {
        subLevelItems.add(item);
    }

    public void addAllToSublist(List<Instance> items) {
        subLevelItems.addAll(items);
    }

    void generateSubLevelItemsFromJSON(JSONObject jsonObject) throws JSONException {}

    @NonNull
    @Override
    public String toString() {
        return label;
    }
}
