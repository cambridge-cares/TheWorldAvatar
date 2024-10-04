package uk.ac.cam.cares.jps.model;

import androidx.annotation.NonNull;

public class OtherInfo {
    String iri;
    String name;

    public OtherInfo(String iri, String name) {
        this.iri = iri;
        this.name = name;
    }

    public String getIri() {
        return iri;
    }

    public String getName() {
        return name;
    }

    @NonNull
    @Override
    public String toString() {
        return name;
    }
}
