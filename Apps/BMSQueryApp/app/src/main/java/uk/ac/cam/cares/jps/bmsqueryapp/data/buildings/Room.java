package uk.ac.cam.cares.jps.bmsqueryapp.data.buildings;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class Room extends Instance{

    public Room(String iri, JSONObject response) throws JSONException {
        this.iri = iri;
        this.label = response.getString("label");
    }

    public void buildEquipmentFromJSON(JSONArray response) throws JSONException {
        subLevelItems = new ArrayList<>();

        for (int i = 0; i < response.length(); i++) {
            JSONObject jo = response.getJSONObject(i);
            Equipment equipment = new Equipment(jo.getString("iri"), jo.getString("label"), jo.getString("type"));
            subLevelItems.add(equipment);
        }
    }

    public List<String> getEquipmentTypes() {
        Set<String> types = new HashSet();
        for (Instance equipment : subLevelItems) {
            types.add(parseEquipmentType(equipment));
        }
        return types.stream().sorted().collect(Collectors.toList());
    }

    private String parseEquipmentType(Instance equipment) {
        String[] parts = ((Equipment)equipment).type.split("/");
        return parts[parts.length - 1];
    }

    public List<Equipment> getSortedSubLevelItemsOfGivenType(String type) {
        return subLevelItems.stream()
                .filter(o -> parseEquipmentType(o).equals(type))
                .sorted(Comparator.comparing(o -> o.label))
                .map(o -> (Equipment)o)
                .collect(Collectors.toList());
    }
}
