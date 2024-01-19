package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;

import java.util.ArrayList;

public class Company extends OntoChemPlantModel {
    @Getter @Setter @FieldAnnotation("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasYearOfEstablishment") protected Double hasYearOfEstablishment;
    @Getter @Setter @FieldAnnotation("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasSectoralDescription") protected String hasSectoralDescription;
    @Getter @Setter @FieldAnnotation("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasSSICCode") protected Integer hasSSICCode;
    @Getter @Setter @FieldAnnotation("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasNumberofEmployees") protected Integer hasNumberofEmployees;
    @Getter @Setter @FieldAnnotation("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasBusinessActivity") protected String hasBusinessActivity;
    @Getter @Setter @FieldAnnotation("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasProductionTechnology") protected String hasProductionTechnology;

    @Getter @Setter @FieldAnnotation(value = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_performance/economic_performance.owl#hasRevenue",
            innerType = Revenue.class) private ArrayList<Revenue> hasRevenue;

    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasPowerConsumption",
            innerType = PowerConsumption.class) private ArrayList<PowerConsumption> hasPowerConsumption;


}
