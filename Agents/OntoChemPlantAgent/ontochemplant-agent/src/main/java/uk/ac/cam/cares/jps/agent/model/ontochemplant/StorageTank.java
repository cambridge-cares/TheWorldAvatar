package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;

import java.net.URI;
import java.util.ArrayList;

public class StorageTank extends OntoChemPlantModel {
    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasStorageCapacity",
            innerType = StorageCapacity.class)
    private ArrayList<StorageCapacity> StorageCapacity;

    @Getter @Setter @FieldAnnotation("http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#stores") protected String Contents;

    @Getter @Setter @FieldAnnotation(value = "http://www.opengis.net/ont/geosparql#ehContains", backward = true)
    protected URI ownedBy;

}
