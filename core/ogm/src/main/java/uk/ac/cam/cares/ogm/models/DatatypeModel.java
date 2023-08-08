package uk.ac.cam.cares.ogm.models;

import org.apache.jena.graph.Node;

/**
 * The interface for object representations of complex RDF datatypes, such as GeometryType. In addition to the
 * methods declared, subclasses should also implement constructors which accept arguments
 * <code>(String value, String datatype)</code>; this requirement cannot be encoded in the interface definition as
 * static methods or constructors are are not permitted in Java interfaces.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
public interface DatatypeModel {

  /*
   * There should be a constructor accepting (String value, String datatype), but we can't declare that here, and
   * static interface methods are not allowed. The constructor is retrieved and invoked via reflection.
   */

  /**
   * Returns a Jena {@link Node} representing the object.
   * @return the {@link Node} representing the object.
   */
  Node getNode();

}
