package uk.ac.cam.cares.ogm.models;

import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.InvalidClassException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * A collection of methods and functions for interacting with data from a {@link FieldAnnotation}-annotated field
 * of a {@link Model} subclass. This should only be used by Model base class methods via {@link MetaModel} for
 * the purposes of change checking, parsing SPARQL query responses and writing SPARQL updates.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
public class FieldInterface {

  /**
   * Puts a value object into a field of a {@link Model} object. If the field is a List, this should add the value
   * as a new element; else, it should set (overwrite) the value of the field.
   */
  @FunctionalInterface
  interface Putter {
    void consume(Model model, Object value) throws Exception;
  }

  /**
   * Converts string representation into Java object
   */
  @FunctionalInterface
  interface Parser {
    Object parse(String value, String datatype, ModelContext context) throws Exception;
  }

  /**
   * Converts Java object into Jena {@link Node}
   */
  @FunctionalInterface
  interface NodeGetter {
    Node get(Object value) throws Exception;
  }

  private static final String INNER_TYPE_OF_ARRAYLIST_NOT_SPECIFIED_ERROR_TEXT = "Inner type of ArrayList field not specified.";

  // Convenience metadata
  public final boolean isList;
  public final boolean isModel;
  public final int index;

  // Direct access to the field itself
  public final Field field;
  public final Method getter;
  public final Method setter;

  // Only for vector fields (lists)
  private final Constructor<?> listConstructor;

  // Only depends on outerType (isList)
  private final Putter putter;

  // The below only depend on innerType; they below operate on the element level. See FunctionalInterface descriptions.
  private final Parser parser;
  private final Function<Object, Object> minimiser;
  private final NodeGetter nodeGetter;

  public FieldInterface(Field field, int index) throws NoSuchMethodException, InvalidClassException {
    this.field = field;
    this.index = index;
    // Determine characteristics of field
    Class<?> parentType = field.getDeclaringClass();
    Class<?> outerType = field.getType();
    isList = List.class.isAssignableFrom(outerType);
    Class<?> innerType = isList ? field.getAnnotation(FieldAnnotation.class).innerType() : outerType;
    if(innerType == Model.class) throw new InvalidClassException(INNER_TYPE_OF_ARRAYLIST_NOT_SPECIFIED_ERROR_TEXT);
    // Get the lombok accessors/modifiers --- we can't access private/protected fields. :(
    String fieldName = field.getName();
    fieldName = fieldName.substring(0, 1).toUpperCase() + fieldName.substring(1);
    getter = parentType.getMethod("get" + fieldName);
    setter = parentType.getMethod("set" + fieldName, outerType);
    // Generate parser
    isModel = Model.class.isAssignableFrom(innerType);
    if (isModel) {
      parser = (String value, String datatype, ModelContext context) -> context.getModel(innerType.asSubclass(Model.class), value);
      nodeGetter = (Object value) -> {
        String iri = ((Model) value).iri;
        return iri == null ? NodeFactory.createBlankNode() : NodeFactory.createURI(iri);
      };
      minimiser = (Object object) -> ((Model) object).iri;
    } else if (innerType == URI.class) {
      parser = (String value, String datatype, ModelContext context) -> URI.create(value);
      nodeGetter = (Object value) -> NodeFactory.createURI(value.toString());
      minimiser = Object::toString;
    } else if (DatatypeModel.class.isAssignableFrom(innerType)) {
      Constructor<?> constructor = innerType.getConstructor(String.class, String.class);
      parser = (String value, String datatype, ModelContext context) -> constructor.newInstance(value, datatype);
      nodeGetter = (Object value) -> ((DatatypeModel) value).getNode();
      minimiser = (Object value) -> ((DatatypeModel) value).getNode().toString();
    } else {
      minimiser = (Object value) -> value;
      if (innerType == Integer.class) {
        parser = (String value, String datatype, ModelContext context) -> Integer.valueOf(value);
        nodeGetter = (Object value) -> NodeFactory.createLiteral(String.valueOf((int) value), XSDDatatype.XSDint);
      } else if (innerType == BigInteger.class) {
        parser = (String value, String datatype, ModelContext context) -> new BigInteger(value);
        nodeGetter = (Object value) -> NodeFactory.createLiteral(value.toString(), XSDDatatype.XSDinteger);
      } else if (innerType == Double.class) {
        parser = (String value, String datatype, ModelContext context) -> Double.valueOf(value);
        nodeGetter = (Object value) -> NodeFactory.createLiteral(String.valueOf((double) value), XSDDatatype.XSDdouble);
      } else if (innerType == String.class) {
        parser = (String value, String datatype, ModelContext context) -> value;
        nodeGetter = (Object value) -> NodeFactory.createLiteral((String) value, XSDDatatype.XSDstring);
      } else {
        throw new InvalidClassException(innerType.toString());
      }
    }
    // Generate pusher
    if (isList) {
      // Due to type erasure, ArrayLists accept Objects, not innerTypes.
      Method adder = outerType.getMethod("add", Object.class);
      putter = (Model model, Object value) -> adder.invoke(getter.invoke(model), value);
      // Override previously assigned default constructor, which was for innerType
      listConstructor = outerType.getConstructor();
    } else {
      putter = setter::invoke;
      listConstructor = null;
    }
  }

  /**
   * Parses and puts a datum represented by a value string and a datatype string into this field of a {@link Model}.
   * For a scalar field, this sets the value. For a vector field (list), this adds the parsed value to the array.
   * @param model          the model to put the value into.
   * @param valueString    the string representation of the value to be put.
   * @param datatypeString the string representation of the RDF datatype of the value to be put.
   */
  public void put(Model model, String valueString, String datatypeString) {
    try {
      putter.consume(model, valueString == null ? null : parser.parse(valueString, datatypeString, model.context));
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * Overwrites the existing value of a field with a default value. For a scalar field, this is <code>null</code>. For
   * a vector field, this is an empty {@link ArrayList}.
   * @param model the model for which to clear this field.
   */
  public void clear(Model model) {
    try {
      setter.invoke(model, isList ? listConstructor.newInstance() : null);
    } catch (Exception e) {
      throw new JPSRuntimeException(e);
    }
  }

  /**
   * Gets a minimised representation of the field value that can be used with <code>equals</code> to check equality on a
   * database representation level (i.e. for all <code>getNode(a).equals(getNode(b))</code>,
   * <code>getMinimised(a).equals(getMinimised(b))</code>), which is value equality but with caveats: only IRI is
   * considered for models, and order is ignored for vector fields.
   * @param model the model from which to read and minimise the field value.
   * @return the minimised representation.
   */
  public Object getMinimised(Model model) {
    try {
      Object value = getter.invoke(model);
      if (isList) {
        // Use a list instead of an array since the List<?>.equals does element-by-element comparison.
        return ((List<?>) value).stream().map(
            (obj) -> obj == null ? null : minimiser.apply(obj)
        ).collect(Collectors.toSet());
      } else {
        return value == null ? null : minimiser.apply(value);
      }
    } catch (IllegalStateException | InvocationTargetException | IllegalAccessException e) {
      throw new JPSRuntimeException(e);
    }
  }

  /**
   * Gets a {@link Node}<code>[]</code> representing the current field value, for use in UpdateBuilder. For scalar
   * fields, the array is of length 1.
   * @param model the model from which to read the values.
   * @return an array of {@link Node}s representing the values.
   */
  public Node[] getNodes(Model model) {
    try {
      if (isList) {
        List<?> list = (List<?>) getter.invoke(model);
        Node[] literals = new Node[list.size()];
        for (int i = 0; i < literals.length; i++)
          literals[i] = list.get(i) == null ? NodeFactory.createBlankNode() : nodeGetter.get(list.get(i));
        return literals;
      } else {
        Object value = getter.invoke(model);
        return new Node[]{value == null ? NodeFactory.createBlankNode() : nodeGetter.get(value)};
      }
    } catch (Exception e) {
      throw new JPSRuntimeException(e);
    }
  }

  /**
   * Determines if two model are equal in this field on a database representation (i.e. {@link Node}) level. This
   * is value equality with the caveats: only IRI is considered for models, and order is ignored for vector fields.
   * Internally uses <code>getMinimised</code>.
   * @param m1 the first model to compare.
   * @param m2 the second model to compare.
   * @return whether they are equal on a database representation level.
   */
  public boolean equals(Model m1, Model m2) {
    Object min1 = getMinimised(m1);
    Object min2 = getMinimised(m2);
    return Objects.equals(min1, min2);
  }

}