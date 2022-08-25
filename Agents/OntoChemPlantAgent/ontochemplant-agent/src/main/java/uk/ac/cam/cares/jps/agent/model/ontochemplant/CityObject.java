package uk.ac.cam.cares.jps.agent.model.ontochemplant;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;

import java.math.BigInteger;

@ModelAnnotation(defaultGraphName = "cityobject/")//SchemaManagerAdapter.CITY_OBJECT_GRAPH + "/")
public class CityObject extends Model {

  @Getter @Setter @FieldAnnotation("http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#objectClassId") protected BigInteger objectClassId;

}
