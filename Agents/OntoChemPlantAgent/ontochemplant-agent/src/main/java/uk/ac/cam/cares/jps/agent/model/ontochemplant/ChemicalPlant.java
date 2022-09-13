package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;

import java.util.ArrayList;

public class ChemicalPlant extends OntoChemPlantModel {
    @Getter @Setter @FieldAnnotation("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasThermalEfficiency") protected Double hasThermalEfficiency;

    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasFuelType",
            innerType = FuelType.class) private ArrayList<FuelType> hasFuelType;
    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasPlantCO2Emission",
            innerType = PlantCO2Emission.class) private ArrayList<PlantCO2Emission> hasPlantCO2Emission;
    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasDesignCapacity",
            innerType = DesignCapacity.class) private ArrayList<DesignCapacity> hasDesignCapacity;

    @Getter @Setter @FieldAnnotation(
            value = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isOwnerOf",
            innerType = Company.class,
            backward = true)
    private ArrayList<Company> Owner;

}
