package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import java.net.URI;
import java.util.ArrayList;

public class OntoChemPlantModel extends Model {

    @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation", backward = true, innerType = URI.class)
    private ArrayList<URI> OntoCityGMLRepresentationOf;

}
