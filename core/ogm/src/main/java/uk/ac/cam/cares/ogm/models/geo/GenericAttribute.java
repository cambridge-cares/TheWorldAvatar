package uk.ac.cam.cares.ogm.models.geo;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;

import java.math.BigInteger;
import java.net.URI;

/**
 * Model representing OntoCityGML CityObjectGenericAttribute objects.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
@ModelAnnotation(defaultGraphName = SchemaManagerAdapter.GENERIC_ATTRIB_GARPH + "/")
public class GenericAttribute extends OntoCityGMLModel {

  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_ATTR_NAME) protected String attrName;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_URI_VAL) protected String uriVal;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_STR_VAL) protected String strVal;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_UNIT) protected String unit;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_ROOT_GENATTRIB_ID) protected String rootGenattribId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_REAL_VAL) protected String realVal;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_PARRENT_GENATTRIB_ID) protected String parentGenattribId;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_INT_VAL) protected String intVal;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_DATE_VAL) protected String dateVal;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_DATA_TYPE) protected BigInteger dataType;
  @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_CITY_OBJECT_ID) protected URI cityObjectId;

}
