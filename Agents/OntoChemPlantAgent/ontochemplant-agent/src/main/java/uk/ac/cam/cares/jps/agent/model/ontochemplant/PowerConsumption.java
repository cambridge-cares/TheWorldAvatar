package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;

public class PowerConsumption extends OntoChemPlantModel {
    @Getter @Setter @FieldAnnotation("http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue") protected Double PowerConsumption_GigaJoulePerYear;

}
