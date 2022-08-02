package uk.ac.cam.cares.ogm.models.geo;

import lombok.Getter;
import lombok.Setter;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;

import java.net.URI;

/**
 * Model representing OntoCityGML ExternalReference objects.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
@ModelAnnotation(defaultGraphName = SchemaManagerAdapter.EXTERNAL_REFERENCES_GRAPH + "/")
public class ExternalReference extends OntoCityGMLModel {

    @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_CITY_OBJECT_ID) private URI cityObjectId;
    @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_INFO_SYS) private String infoSys;
    @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_NAME)  private String name;
    @Getter @Setter @FieldAnnotation(SchemaManagerAdapter.ONTO_URI) private URI uri;

}