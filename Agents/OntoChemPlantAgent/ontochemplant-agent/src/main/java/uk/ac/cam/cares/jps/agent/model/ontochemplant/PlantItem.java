package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;

import java.net.URI;
import java.util.ArrayList;

public class PlantItem extends OntoChemPlantModel {

    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasIndividualCO2Emission",
            innerType = IndividualCO2Emission.class) private ArrayList<IndividualCO2Emission> IndividualCO2Emission;
    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasGeneratedHeat",
            innerType = GeneratedHeat.class) private ArrayList<GeneratedHeat> GeneratedHeat;

    @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation")
            protected URI CityFurnitureIRI;

    @Getter @Setter @FieldAnnotation(value = "http://www.opengis.net/ont/geosparql#ehContains", backward = true)
            protected URI OwnedBy;

    @Getter @Setter @FieldAnnotation(value = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_performance/economic_performance.owl#hasCost",
            innerType = Costs.class) private ArrayList<Costs> Costs;

    /* Use this for cases where detailed information regarding the chemical plant is required. For now, the use case
     only needs the name of the chemical plant */

//    @Getter @Setter @FieldAnnotation(
//            value = "http://www.opengis.net/ont/geosparql#ehContains",
//            innerType = ChemicalPlant.class,
//            backward = true)
//    private ArrayList<ChemicalPlant> ChemicalPlant;




}
