package uk.ac.cam.cares.ogm.models;

import uk.ac.cam.cares.ogm.models.SPARQLUtils;

import java.util.Objects;

/**
 * A data structure describing the sufficient identifying features of an ontology role. Built from the
 * {@link FieldAnnotation} and the {@link ModelAnnotation} of the field's declaring class. Used to look up
 * query response rows for matching FieldInterfaces in {@link MetaModel}.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
public class FieldKey implements Comparable<FieldKey> {
  // The predicate is expanded to a full IRI but the graph is kept as a fragment because the full graph iri is namespace
  // -dependent and will not be the same across different applications, while the full predicate iri does not change.
  // Note that the graphName is "" for triple fields.
  public final String predicate;
  public final String graphName;
  public final boolean backward;

  /**
   * Constructs a {@link FieldKey} from a graph name and a predicate IRI.
   * @param graphName the short name of a graph, i.e. the part after the graph namespace. {@code ""} for triple use.
   * @param predicate full predicate IRI.
   * @param backward  whether the declaring class is the object of the quad.
   */
  public FieldKey(String graphName, String predicate, boolean backward) {
    this.predicate = predicate;
    this.graphName = graphName;
    this.backward = backward;
  }

  /**
   * Constructs a {@link FieldKey} from a {@link FieldAnnotation} and the {@link ModelAnnotation} of the
   * declaring class of the field.
   * @param fieldAnnotation of the field.
   * @param modelAnnotation of the declaring class of the field.
   */
  public FieldKey(FieldAnnotation fieldAnnotation, ModelAnnotation modelAnnotation) {
    predicate = SPARQLUtils.expandQualifiedName(fieldAnnotation.value());
    graphName = !fieldAnnotation.graphName().isEmpty() ? fieldAnnotation.graphName() :
        (modelAnnotation == null ? "" : modelAnnotation.defaultGraphName());
    backward = fieldAnnotation.backward();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    FieldKey fieldKey = (FieldKey) o;
    return backward == fieldKey.backward && Objects.equals(predicate, fieldKey.predicate) && Objects.equals(graphName, fieldKey.graphName);
  }

  @Override
  public int hashCode() {
    return Objects.hash(predicate, graphName, backward);
  }

  /**
   * The comparison order of keys here is deliberate. It optimises querying by first sorting by graph, enabling the
   * <code>GRAPH &lt;graph&gt; { ... }</code> pattern, and then by backward/forward, enabling the <code> &lt;subject&gt;
   * &lt;predicate&gt; &lt;object&gt;; &lt;predicate&gt; &lt;object&gt;; &lt;predicate&gt; &lt;object&gt;.</code> pattern.
   * @param otherKey the key being compared against.
   * @return the comparison outcome.
   */
  @Override
  public int compareTo(FieldKey otherKey) {
    return (graphName + backward + predicate).compareTo(otherKey.graphName + otherKey.backward + otherKey.predicate);
  }

}
