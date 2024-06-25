package uk.ac.cam.cares.jps.model.building;

import androidx.annotation.Nullable;

public class Element extends Instance{

    public Element(String iri, String label) {
        super(iri, label);
    }

    @Override
    public boolean equals(@Nullable Object obj) {
        if (obj instanceof Element) {
            return this.iri.equals(((Element) obj).iri);
        }
        return false;
    }
}
