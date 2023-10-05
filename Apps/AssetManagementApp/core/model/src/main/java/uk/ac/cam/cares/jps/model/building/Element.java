package uk.ac.cam.cares.jps.model.building;

import androidx.annotation.Nullable;

public class Element extends Instance{

    @Override
    public boolean equals(@Nullable Object obj) {
        if (obj instanceof Element) {
            return this.iri.equals(((Element) obj).iri);
        }
        return false;
    }
}
