package uk.ac.cam.cares.jps.data;

import androidx.annotation.NonNull;

public class OtherInfoModel {
    String iri;
    String name;

    public OtherInfoModel(String iri, String name) {
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
