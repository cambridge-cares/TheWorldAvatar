package uk.ac.cam.cares.ogm.models;

import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang.ArrayUtils;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.Objects;

/**
 * {@link Model} is the abstract base for classes which represent objects in Blazegraph. It implements a number of
 * methods which push data to and pull data from Blazegraph, and also maintains automatic dirty tracking of field values
 * to only write updates for modified fields on push. Also see {@link MetaModel}.
 * <p>
 * Subclasses of {@link Model} should annotate fields meant for database read-write with {@link Getter},
 * {@link Setter} and {@link FieldAnnotation}.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
public abstract class Model {

  public enum SpecialFieldInstruction {
    NEW,
    UNPULLED,
    FORCE_PUSH
  }

  public enum LifeCycle {
    LIVE,
    TO_DELETE,
    TO_DELETE_ZEALOUS,
    DESTROYED,
  }

  private static final String FIELD_NOT_FOUND_ERROR_TEXT = "No field of name found in Model.";

  @Getter String iri;
  @Getter ModelContext context;
  final MetaModel metaModel;
  public LifeCycle state;

  // Minimised copies of field values at the last synchronisation with the database, indexed by FieldInterface.index.
  final Object[] cleanValues;

  /**
   * The direct Model constructor is for {@link ModelContext} internal use only. To create a model, use one of the
   * factory functions in {@link ModelContext}.
   */
  public Model() {
    metaModel = MetaModel.get(this.getClass());
    state = LifeCycle.LIVE;
    cleanValues = new Object[metaModel.fieldMap.size()];
    // Initialise lists to empty lists if they haven't already been initialised
    for (Map.Entry<FieldKey, FieldInterface> vectorEntry : metaModel.vectorFieldList) {
      try {
        if (vectorEntry.getValue().getter.invoke(this) == null)
          vectorEntry.getValue().clear(this);
      } catch (IllegalAccessException | InvocationTargetException e) {
        throw new JPSRuntimeException(e);
      }
    }
  }

  /**
   * Clears all field of the instance
   */
  public void clear(String ... fieldNames) {
    for (FieldInterface field : metaModel.fieldMap.values())
      if (fieldNames.length == 0 || ArrayUtils.contains(fieldNames, field.field.getName()))
        field.clear(this);
  }

  /**
   * Makes the specified fields of the instance clean, so they will not be written on push unless changed again.
   * @param fieldNames the fields to clean. If empty, all fields are cleaned.
   */
  public void setClean(String... fieldNames) {
    for (FieldInterface field : metaModel.fieldMap.values())
      if (fieldNames.length == 0 || ArrayUtils.contains(fieldNames, field.field.getName()))
        cleanValues[field.index] = field.getMinimised(this);
  }

  /**
   * Makes the specified fields of the instance dirty, so they will be written on push.
   * @param fieldNames the fields to clean. If empty, all fields are dirtied.
   */
  public void setDirty(String... fieldNames) {
    for (FieldInterface field : metaModel.fieldMap.values())
      if (fieldNames.length == 0 || ArrayUtils.contains(fieldNames, field.field.getName()))
        cleanValues[field.index] = SpecialFieldInstruction.FORCE_PUSH;
  }

  /**
   * @return if the named field is clean; fields immediately after {@link ModelContext#createNewModel(Class, String)}
   * before first push are not considered clean. Dirty fields are pushed to the database on
   * {@link ModelContext#pushChanges(Model)}.
   */
  public boolean isClean(String fieldName) {
    for (FieldInterface field : metaModel.fieldMap.values())
      if (field.field.getName().equals(fieldName))
        return !shouldWriteInsert(field);
    throw new JPSRuntimeException(FIELD_NOT_FOUND_ERROR_TEXT);
  }

  /**
   * @return if the field is hollow, i.e. its cleanValues entry is set to SpecialFieldInstruction.UNPULLED.
   * This is the case if it has been manually set hollow, or if the model instance was created with createHollowModel
   * and the field has not been pulled or pushed.
   */
  public boolean isHollow(String fieldName) {
    for (FieldInterface field : metaModel.fieldMap.values())
      if (field.field.getName().equals(fieldName))
        return cleanValues[field.index] == SpecialFieldInstruction.UNPULLED;
    throw new JPSRuntimeException(FIELD_NOT_FOUND_ERROR_TEXT);
  }

  /**
   * Internal function for use by {@link ModelContext} for determining updates to push
   */
  boolean shouldWriteInsert(FieldInterface field) {
    if (cleanValues[field.index] == SpecialFieldInstruction.UNPULLED) return false;
    if (cleanValues[field.index] == SpecialFieldInstruction.NEW) return true;
    if (cleanValues[field.index] == SpecialFieldInstruction.FORCE_PUSH) return true;
    return !Objects.equals(cleanValues[field.index], field.getMinimised(this));
  }

  /**
   * Internal function for use by {@link ModelContext} for determining updates to push
   */
  boolean shouldWriteDelete(FieldInterface field) {
    return shouldWriteInsert(field) && cleanValues[field.index] != SpecialFieldInstruction.NEW;
  }

  /**
   * Wraps {@link ModelContext#pushChanges(Model)}.
   */
  public void pushChanges() {
    context.pushChanges(this);
  }

  /**
   * Wraps {@link ModelContext#delete(Model, boolean)}.
   */
  public void delete(boolean zealous) {
    context.delete(this, zealous);
  }

  /**
   * Wraps {@link ModelContext#retire(Model)}.
   */
  public void retire() {
    context.retire(this);
  }

  /**
   * Wraps {@link ModelContext#recursivePullAll(Model, int)}.
   */
  public void recursivePullAll(int recursionRadius) {
    context.recursivePullAll(this, recursionRadius);
  }

  /**
   * Wraps {@link ModelContext#pullAll(Model)}.
   */
  public void pullAll() {
    context.pullAll(this);
  }


  /**
   * Wraps {@link ModelContext#recursivePullPartial(Model, int, String...)}.
   */
  public void recursivePull(int recursionRadius, String... fieldNames) {
    context.recursivePullPartial(this, recursionRadius, fieldNames);
  }

  /**
   * Wraps {@link ModelContext#pullPartial(Model, String...)}.
   */
  public void pull(String... fieldNames) {
    context.pullPartial(this, fieldNames);
  }

  /**
   * Equality is defined as all {@link Model}-managed fields matching. <code>originalFieldValues</code> and fields
   * without {@link FieldAnnotation} annotations do not have to match.
   * @param o the object to compare against.
   * @return whether the objects are equal.
   */
  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    for (FieldInterface field : metaModel.fieldMap.values())
      if (!field.equals(this, (Model) o))
        return false;
    return true;
  }

}
