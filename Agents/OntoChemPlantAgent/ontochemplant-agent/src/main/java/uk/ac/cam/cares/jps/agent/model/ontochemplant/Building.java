package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;

import java.net.URI;
import java.util.ArrayList;


public class Building extends OntoChemPlantModel {

    @Getter @Setter @FieldAnnotation(value = "http://ontology.eil.utoronto.ca/icontact.owl#hasAddress", innerType = Address.class)
            private ArrayList<Address> address;

    @Getter @Setter @FieldAnnotation(value = "http://theworldavatar.com/ontology/ontochemplant/OntoChemPlant.owl#hasUtilityCost", innerType = UtilityCosts.class)
            private ArrayList<UtilityCosts> utilitycosts;

    @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation")
            protected URI BuildingIRI;

    @Getter @Setter @FieldAnnotation(value = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isOwnerOf", backward = true)
            protected URI OwnedBy;

}
