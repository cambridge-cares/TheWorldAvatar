package uk.ac.cam.cares.jps.bmsqueryapp.data.buildings;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Iterator;

public class Facility extends Instance{

    public Facility(String iri, JSONObject response) throws JSONException {
        this.iri = iri;
        this.label = response.getString("label");
        if (response.has("rooms")) {
            generateSubLevelItemsFromJSON(response.getJSONObject("rooms"));
        }
    }

    @Override
    void generateSubLevelItemsFromJSON(JSONObject jsonObject) throws JSONException {
        this.subLevelItems = new ArrayList<>();
        Iterator<String> iter = jsonObject.keys();
        while (iter.hasNext()) {
            String roomIRI = iter.next();
            Room room = new Room(roomIRI, jsonObject.getJSONObject(roomIRI));
            subLevelItems.add(room);
        }
    }
}
