package uk.ac.cam.cares.jps.model.building;

import org.json.JSONException;
import org.json.JSONObject;

public class Room extends Instance{

    public Room(String iri, JSONObject response) throws JSONException {
        this.iri = iri;
        this.label = response.getString("label");
    }

    public Room(String iri, String label) {
        super(iri, label);
    }
}
