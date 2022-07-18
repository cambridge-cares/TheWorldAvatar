package uk.ac.cam.cares.ogm.models.geo;

import lombok.Getter;
import lombok.Setter;
import org.citydb.database.adapter.blazegraph.SchemaManagerAdapter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;

import java.math.BigInteger;
import java.util.ArrayList;

/**
 * Model representing OntoCityGML CityObject objects.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
@ModelAnnotation(defaultGraphName = SchemaManagerAdapter.CITY_OBJECT_GRAPH + "/")
public class CityObject extends OntoCityGMLModel {

  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_CREATION_DATE) protected String creationDate;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_DESCRIPTION) protected String description;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_ENVELOPE_TYPE) protected EnvelopeType envelopeType;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_GML_ID) protected String gmlId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LAST_MODIFICATION_DATE) protected String lastModificationDate;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_LINEAGE) protected String lineage;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_NAME) protected String name;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_NAME_CODESPACE) protected String nameCodespace;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_OBJECT_CLASS_ID) protected BigInteger objectClassId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_REASON_FOR_UPDATE) protected String reasonForUpdate;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_RELATIVE_TO_TERRAIN) protected String relativeToTerrain;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_RELATIVE_TO_WATER) protected String relativeToWater;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_TERMINATION_DATE) protected String terminationDate;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_UPDATING_PERSON) protected String updatingPerson;

  @Getter @Setter @FieldAnnotation(
          value = SchemaManagerAdapter.ONTO_CITY_OBJECT_ID,
          graphName = SchemaManagerAdapter.GENERIC_ATTRIB_GARPH + "/",
          innerType = GenericAttribute.class,
          backward = true)
  private ArrayList<GenericAttribute> genericAttributes;

  @Getter @Setter @FieldAnnotation(
          value = SchemaManagerAdapter.ONTO_CITY_OBJECT_ID,
          graphName = SchemaManagerAdapter.EXTERNAL_REFERENCES_GRAPH + "/",
          innerType = ExternalReference.class,
          backward = true)
  private ArrayList<ExternalReference> externalReferences;

}
