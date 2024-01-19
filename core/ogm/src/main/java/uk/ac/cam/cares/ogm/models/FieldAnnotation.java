package uk.ac.cam.cares.ogm.models;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation describing the sufficient identifying features of an ontology role. Used to build a {@link FieldKey}.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface FieldAnnotation {

  /**
   * @return the name of the predicate, either a full IRI or a qualified name, e.g. <code>ocgml:id</code>.
   */
  String value();

  /**
   * @return whether the declaring class' instance IRI is the object (true) or subject (false) of the quad.
   */
  boolean backward() default false;

  /**
   * @return the short graph name of the quad, e.g. "surfacegeometry". If not overridden, this defaults to the declaring
   * class' <code>ModelAnnotation.defaultGraphName()</code>
   */
  String graphName() default "";

  /**
   * @return <code>T</code> if the field is of type {@link java.util.ArrayList}; else, the value of this field is ignored.
   */
  Class<?> innerType() default Model.class;

}