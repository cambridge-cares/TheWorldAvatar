package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;

import java.net.URI;

public class LandLot extends OntoChemPlantModel {

    @Getter @Setter @FieldAnnotation("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasLandlotNumber") protected String LandLotNumber;
    @Getter @Setter @FieldAnnotation("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasLotArea") protected URI LotArea;

}
