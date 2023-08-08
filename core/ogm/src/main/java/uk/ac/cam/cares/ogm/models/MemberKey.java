package uk.ac.cam.cares.ogm.models;


import java.util.Objects;

public class MemberKey {
    public final Class<?> ofClass;
    public final String iri;

    public MemberKey(Class<?> ofClass, String iri) {
        this.ofClass = ofClass;
        this.iri = iri;
    }

    @Override
    public int hashCode() {
        return Objects.hash(ofClass, iri);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof MemberKey && ((MemberKey) obj).ofClass.equals(ofClass) && ((MemberKey) obj).iri.equals(iri);
    }
}
