package uk.ac.cam.cares.ogm.models;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.InvalidClassException;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A collection of {@link FieldInterface}s generated from the metadata of a {@link Model} subclass and other
 * supporting functions. All instances of a {@link Model} subclass share one {@link MetaModel}.
 * @author <a href="mailto:jec226@cam.ac.uk">Jefferson Chua</a>
 * @version $Id$
 */
public class MetaModel {

  private final static ConcurrentHashMap<Class<?>, MetaModel> metaModelMap = new ConcurrentHashMap<>();

  public final boolean isQuads;
  public final TreeMap<FieldKey, FieldInterface> fieldMap;
  public final List<Map.Entry<FieldKey, FieldInterface>> vectorFieldList;
  public final List<Map.Entry<FieldKey, FieldInterface>> scalarFieldList;

  private MetaModel(Class<?> target) throws NoSuchMethodException, InvalidClassException {
    ModelAnnotation modelAnnotation = target.getAnnotation(ModelAnnotation.class);
    // Collect fields through the full inheritance hierarchy
    Class<?> currentClass = target;
    ArrayList<Field> fields = new ArrayList<>();
    do {
      fields.addAll(Arrays.asList(currentClass.getDeclaredFields()));
      currentClass = currentClass.getSuperclass();
    } while (currentClass != null);
    // Build fieldMaps. Use TreeMaps because sorting makes compiled queries and updates more efficient.
    // See queuePushUpdates and FieldKey.compareTo for more details.
    fieldMap = new TreeMap<>();
    TreeMap<FieldKey, FieldInterface> scalarFieldMap = new TreeMap<>();
    TreeMap<FieldKey, FieldInterface> vectorFieldMap = new TreeMap<>();
    int index = 0;
    boolean anyGraphs = false;
    for (Field field : fields) {
      FieldAnnotation fieldAnnotation = field.getAnnotation(FieldAnnotation.class);
      if (fieldAnnotation != null) {
        FieldInterface fieldInterface = new FieldInterface(field, index++);
        FieldKey fieldKey = new FieldKey(fieldAnnotation, modelAnnotation);
        fieldMap.put(fieldKey, fieldInterface);
        (fieldInterface.isList ? vectorFieldMap : scalarFieldMap).put(fieldKey, fieldInterface);
        anyGraphs = anyGraphs || !fieldKey.graphName.equals("");
      }
    }
    scalarFieldList = new ArrayList<>(scalarFieldMap.entrySet());
    vectorFieldList = new ArrayList<>(vectorFieldMap.entrySet());
    isQuads = anyGraphs;
  }

  public static <T extends Model> MetaModel get(Class<T> ofClass) {
    if (metaModelMap.containsKey(ofClass)) {
      return metaModelMap.get(ofClass);
    } else {
      try {
        MetaModel newMetaModel = new MetaModel(ofClass);
        metaModelMap.put(ofClass, newMetaModel);
        return newMetaModel;
      } catch (NoSuchMethodException | InvalidClassException e) {
        throw new JPSRuntimeException(e);
      }
    }
  }

}
