package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;

import java.net.URI;

public class OntoChemPlantModel extends Model {

    @Getter @Setter @FieldAnnotation(value = "https://www.theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation", backward = true)
    protected URI OntoCityGMLRepresentationOf;

}
