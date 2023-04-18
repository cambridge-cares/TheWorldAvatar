package uk.ac.cam.cares.jsp.integration;

import java.util.ArrayList;
import java.util.List;

public class BuildingLink {
    List<GeoObject3D> geoObject3Ds = new ArrayList<>();
    List<KGObjects> kgObjects = new ArrayList<>();

    BuildingLink (List<GeoObject3D> geoObject3Ds, List<KGObjects> kgObjects){
        this.geoObject3Ds = geoObject3Ds;
        this.kgObjects = kgObjects;
    }
    public void fuzzyMatch(List<GeoObject3D> geoObject3Ds, List<KGObjects> kgObjects){

    }
}
