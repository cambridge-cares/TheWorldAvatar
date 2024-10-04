package uk.ac.cam.cares.jps.model.building;

import androidx.annotation.Nullable;

import java.util.List;

public class Workspace extends Instance{

    // todo: should add element to workspace
    public Workspace(String iri, String label) {
        super(iri, label);
    }

    @Override
    public boolean equals(@Nullable Object obj) {
        if (obj instanceof Workspace) {
            return this.iri.equals(((Workspace) obj).iri);
        }
        return false;
    }
}
