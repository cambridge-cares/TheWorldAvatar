package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;

import java.util.ArrayList;

public class Address extends OntoChemPlantModel {

    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasLatitudeEPSG24500") protected Double LatitudeEPSG24500;
    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasLongitudeEPSG24500") protected Double LongitudeEPSG24500;

    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasLandLotDetails", innerType = LandLot.class)
            private ArrayList<LandLot> LandLotDetails;

}
