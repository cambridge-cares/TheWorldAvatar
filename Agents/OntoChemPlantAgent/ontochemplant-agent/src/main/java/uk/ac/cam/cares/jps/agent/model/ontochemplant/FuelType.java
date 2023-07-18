package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;

import java.util.ArrayList;

public class FuelType extends OntoChemPlantModel {
    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasCarbonEmissionIndex",
            innerType = CarbonEmissionIndex.class) private ArrayList<CarbonEmissionIndex> hasCarbonEmissionIndex;

}
