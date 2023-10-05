package uk.ac.cam.cares.jps.model.building;

import androidx.annotation.Nullable;

import java.util.List;

public class Workspace extends Instance{

    // todo: should add element to workspace
    public Workspace(String iri) {
        this.iri = iri;
    }

    public void constructElements(List<String> elementIris) {
        for (String elementIri : elementIris) {
            Element temp = new Element();
            temp.iri = elementIri;
            this.subLevelItems.add(temp);
        }
    }

    @Override
    public boolean equals(@Nullable Object obj) {
        if (obj instanceof Workspace) {
            return this.iri.equals(((Workspace) obj).iri);
        }
        return false;
    }
}
