package uk.ac.cam.cares.jps.bmsqueryapp.data;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.Iterator;

public class Building extends Instance{

    public Building(JSONObject response, String iri) throws JSONException {
        this.label = response.getString("label");
        this.iri = iri;

        if (response.has("facilities")) {
            generateSubLevelItemsFromJSON(response.getJSONObject("facilities"));
        }
    }

    @Override
    void generateSubLevelItemsFromJSON(JSONObject jsonObject) throws JSONException{
        subLevelItems = new ArrayList<>();
        Iterator<String> iter = jsonObject.keys();
        while (iter.hasNext()) {
            String facilityIRI = iter.next();
            Facility facility = new Facility(facilityIRI, jsonObject.getJSONObject(facilityIRI));
            subLevelItems.add(facility);
        }
    }
}
