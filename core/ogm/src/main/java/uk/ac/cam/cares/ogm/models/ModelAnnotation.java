package uk.ac.cam.cares.ogm.models;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation describing the native graph name of a {@link Model}, which is used as declared fields' default
 * <code>FieldKey.graphName</code> if not specified in each {@link FieldAnnotation}, and also as the graph name for
 * <code>Model.setIri(String uuid, String namespace)</code>.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface ModelAnnotation {
	/**
	 * @return the native graph of the {@link Model}, which will be used for declared fields' default
	 * <code>FieldKey.graphName</code> if <code>FieldAnnotation.graphName</code> is not explicitly specified.
	 */
	String defaultGraphName();
}