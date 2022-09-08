package uk.ac.cam.cares.ogm.models;

import org.apache.commons.lang.ArrayUtils;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.UpdateBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.sparql.core.Quad;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.stream.Collectors;

public class ModelContext {

  public RecursivePullSession currentPullSession;

  // SPARQL variable names
  private static final String MODEL = "model";
  private static final String GRAPH = "graph";
  private static final String PREDICATE = "predicate";
  private static final String VALUE = "value";
  private static final String DATATYPE = "datatype";
  private static final String ISBLANK = "isblank";
  // Helper constants for constructing SPARQL queries
  private static final String ISBLANK_FUN = "ISBLANK";
  private static final String DATATYPE_FUN = "DATATYPE";
  private static final String QM = "?";
  private static final String OP = "(";
  private static final String CP = ")";
  // Error text
  private static final String QUAD_MODEL_IN_TRIPLE_CONTEXT_ERROR_TEXT = "Quad Model cannot be initialised in triple context.";
  private static final String OBJECT_NOT_FOUND_EXCEPTION_TEXT = "Object not found in database.";
  private static final String MODEL_ALREADY_REGISTERED_EXCEPTION_TEXT = "Model already registered for IRI.";
  // Threshold for executeUpdates to execute if "force" is not specified
  private static final int EXECUTION_CHARACTER_THRESHOLD = 250000;

  public final String targetResourceId;
  public final String graphNamespace;
  public final Map<MemberKey, Model> members;

  /**
   * @return whether the context is for a quad or triple store.
   */
  public boolean isQuads() {
    return graphNamespace != null;
  }

  /**
   * Creates a triple store context.
   * @param targetResourceId the target resource ID passed to AccessAgentCaller for database access.
   */
  public ModelContext(String targetResourceId) {
    this.targetResourceId = targetResourceId;
    this.graphNamespace = null;
    this.members = new HashMap<>();
  }

  /**
   * Creates a triple store context with the specified initial member capacity.
   * @param targetResourceId the target resource ID passed to AccessAgentCaller for database access.
   * @param initialCapacity  the capacity to initialise the context members hashmap with.
   */
  public ModelContext(String targetResourceId, int initialCapacity) {
    this.targetResourceId = targetResourceId;
    this.graphNamespace = null;
    this.members = new HashMap<>(initialCapacity);
  }

  /**
   * Creates a quad store context.
   * @param targetResourceId the target resource ID passed to AccessAgentCaller for database access.
   * @param graphNamespace   the graph namespace to which graph names provided in {@link Model} definitions are appended
   *                         to obtain the actual graph IRIs. Do include the trailing # or /.
   */
  public ModelContext(String targetResourceId, String graphNamespace) {
    this.targetResourceId = targetResourceId;
    this.graphNamespace = graphNamespace;
    this.members = new HashMap<>();
  }

  /**
   * Creates a quad store context with the specified initial member capacity.
   * @param targetResourceId the target resource ID passed to AccessAgentCaller for database access.
   * @param graphNamespace   the graph namespace to which graph names provided in {@link Model} definitions are appended
   *                         to obtain the actual graph IRIs. Do include the trailing # or /.
   * @param initialCapacity  the capacity to initialise the context members hashmap with.
   */
  public ModelContext(String targetResourceId, String graphNamespace, int initialCapacity) {
    this.targetResourceId = targetResourceId;
    this.graphNamespace = graphNamespace;
    this.members = new HashMap<>(initialCapacity);
  }

  /**
   * Creates a model with uninitialised {@code cleanValues}. Only for internal use.
   */
  private <T extends Model> T createPrototypeModel(Class<T> ofClass, String iri) {
    if (MetaModel.get(ofClass).isQuads && !isQuads()) {
      throw new JPSRuntimeException(QUAD_MODEL_IN_TRIPLE_CONTEXT_ERROR_TEXT);
    }
    MemberKey key = new MemberKey(ofClass, iri);
    if (members.containsKey(key)) {
      throw new JPSRuntimeException(MODEL_ALREADY_REGISTERED_EXCEPTION_TEXT);
    }
    try {
      Constructor<T> constructor = ofClass.getDeclaredConstructor();
      T instance = constructor.newInstance();
      instance.iri = iri;
      instance.context = this;
      members.put(key, instance);
      return instance;
    } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
      throw new JPSRuntimeException(e);
    }
  }

  /**
   * Creates a model whose fields are all at default values and marked dirty.
   */
  public <T extends Model> T createNewModel(Class<T> ofClass, String iri) {
    T instance = createPrototypeModel(ofClass, iri);
    Arrays.fill(instance.cleanValues, Model.SpecialFieldInstruction.NEW);
    return instance;
  }

  /**
   * Creates a model whose fields are all at default values and disabled (fields never push regardless of value). Any
   * pull actions will cause the pulled fields to become enabled and resume normal clean/dirty push behaviour.
   * Equivalent to
   */
  public <T extends Model> T createHollowModel(Class<T> ofClass, String iri) {
    T model = createPrototypeModel(ofClass, iri);
    Arrays.fill(model.cleanValues, Model.SpecialFieldInstruction.UNPULLED);
    return model;
  }

  /**
   * If this context already has registered a {@link Model} of the given IRI, returns that. Otherwise, creates a hollow
   * model for it. If this context has a {@link RecursivePullSession} active, i.e. this is a downstream request of a
   * {@link #recursivePullAll(Model, int)} or {@link #recursivePullPartial(Model, int, String...)}  call), this fetch
   * request is reported to the pull session.
   * @return the requested {@link Model}.
   */
  public <T extends Model> T getModel(Class<T> ofClass, String iri) {
    Model model = members.get(new MemberKey(ofClass, iri));
    if (model == null) model = createHollowModel(ofClass, iri);
    //
    if (currentPullSession != null) currentPullSession.queue(model);
    return ofClass.cast(model);
  }

  /**
   * If this context already has registered a {@link Model} of the given IRI, returns that. Otherwise, returns null.
   */
  public <T extends Model> T optGetModel(Class<T> ofClass, String iri) {
    return ofClass.cast(members.get(new MemberKey(ofClass, iri)));
  }

  /**
   * Convenience wrapper for {@link #createHollowModel(Class, String)} followed by {@link #pullAll(Model)}.
   * @return the loaded model.
   */
  public <T extends Model> T loadAll(Class<T> ofClass, String iri) {
    T model = createHollowModel(ofClass, iri);
    pullAll(model);
    return model;
  }

  /**
   * Convenience wrapper for {@link #createHollowModel(Class, String)} followed by {@link #recursivePullAll(Model, int)}.
   * @return the loaded model.
   */
  public <T extends Model> T recursiveLoadAll(Class<T> ofClass, String iri, int recursionRadius) {
    T model = createHollowModel(ofClass, iri);
    recursivePullAll(model, recursionRadius);
    return model;
  }

  /**
   * Convenience wrapper for {@link #createHollowModel(Class, String)} followed by {@link #pullPartial(Model, String...)}.
   * @return the partially loaded model.
   */
  public <T extends Model> T loadPartial(Class<T> ofClass, String iri, String... fieldNames) {
    T model = createHollowModel(ofClass, iri);
    pullPartial(model, fieldNames);
    return model;
  }

  /**
   * Convenience wrapper for {@link #createHollowModel(Class, String)} followed by {@link #recursivePullPartial(Model, int, String...)}.
   * @return the partially loaded model.
   */
  public <T extends Model> T recursiveLoadPartial(Class<T> ofClass, String iri, int recursionRadius, String... fieldNames) {
    T model = createHollowModel(ofClass, iri);
    recursivePullPartial(model, recursionRadius, fieldNames);
    return model;
  }

  /**
   * Equivalent to {@link #pullAllWhere}, but also recursively loads to the specified recursion radius the retrieved
   * objects in the normal {@link #loadAll(Class, String)} behaviour.
   */
  public <T extends Model> List<T> recursivePullAllWhere(Class<T> ofClass, WhereBuilder condition, int recursionRadius) {
    currentPullSession = new RecursivePullSession(recursionRadius-1, this);
    List<T> models = pullAllWhere(ofClass, condition);
    for (T model : models) currentPullSession.queue(model);
    currentPullSession.execute();
    currentPullSession = null;
    return models;
  }

  /**
   * Loads all objects which match the given search condition as {@code ofClass}, using the {@link #pullAll} query
   * pattern. The condition should use the {@link Node} from {@link #getModelVar()} to represent the models to be
   * captured. Note that this can only be used for {@link Model}s of a single class. Note that an insufficiently
   * specific WHERE may result in entities not of your target class being misidentified and loaded in.
   */
  public <T extends Model> List<T> pullAllWhere(Class<T> ofClass, WhereBuilder condition) {
    Node modelNode = NodeFactory.createVariable(MODEL);
    Set<String> resultIris = new HashSet<>();
    for (boolean backward : new boolean[]{false, true}) {
      // Execute query
      SelectBuilder query = buildPullAllInDirectionQuery(modelNode, backward);
      if (condition != null) query.addWhere(condition);
      query.addVar(modelNode).addOrderBy(modelNode);
      JSONArray response = query(query.buildString());
      if (response.length() == 0) continue;
      // Identify intervals corresponding to single Model and read the intervals in
      String previousIri = response.getJSONObject(0).getString(MODEL);
      int end, start = 0;
      for (end = 1; end < response.length(); end++) {
        String nextIri = response.getJSONObject(end).getString(MODEL);
        if (!nextIri.equals(previousIri)) {
          Model model = getModel(ofClass, previousIri);
          if (resultIris.add(previousIri)) model.clear();
          readPullAllInDirectionResponse(model, response, backward, start, end);
          start = end;
          previousIri = nextIri;
        }
      }
      // Last interval
      readPullAllInDirectionResponse(getModel(ofClass, previousIri), response, backward, start, end);
      resultIris.add(previousIri);
    }
    // Note that we do not directly use a HashSet<T> to avoid expensive equals() invocations on Model classes.
    return resultIris.stream().map((resultIri) -> {
      T model = getModel(ofClass, resultIri);
      model.setClean();
      return model;
    }).collect(Collectors.toList());
  }

  /**
   * Equivalent to {@link #pullPartialWhere}, but also recursively loads to the specified recursion radius the retrieved
   * objects, using the same field names in the normal {@link #loadPartial(Class, String, String...)} behaviour.
   */
  public <T extends Model> List<T> recursivePullPartialWhere(Class<T> ofClass, WhereBuilder condition, int recursionRadius, String... fieldNames) {
    currentPullSession = new RecursivePullSession(recursionRadius-1, this, fieldNames);
    List<T> models = pullPartialWhere(ofClass, condition, fieldNames);
    for (T model : models) currentPullSession.queue(model);
    currentPullSession.execute();
    currentPullSession = null;
    return models;
  }

  /**
   * Loads all objects which match the given search condition as {@code ofClass}, using the {@link #pullPartial} query
   * pattern. The condition should use the {@link Node} from {@link #getModelVar()} to represent the models to be
   * captured. Note that this can only be used for {@link Model}s of a single class. Note that an insufficiently
   * specific WHERE may result in entities not of your target class being misidentified and loaded in.
   */
  public <T extends Model> List<T> pullPartialWhere(Class<T> ofClass, WhereBuilder condition, String... fieldNames) {
    Node modelNode = NodeFactory.createVariable(MODEL);
    MetaModel metaModel = MetaModel.get(ofClass);
    Set<String> resultIris = new HashSet<>();
    Boolean scalar = false;
    // Scalars
    SelectBuilder scalarsQuery = buildScalarsQuery(modelNode, metaModel, fieldNames);
    if (scalarsQuery.getVars().size() > 0) {
      scalar = true;
      if (condition != null) scalarsQuery.addWhere(condition);
      scalarsQuery.addVar(modelNode);
      JSONArray scalarsResponse = query(scalarsQuery.buildString());
      for (int i = 0; i < scalarsResponse.length(); i++) {
        JSONObject row = scalarsResponse.getJSONObject(i);
        T model = getModel(ofClass, row.getString(MODEL));
        readScalarsResponse(model, row);
        resultIris.add(model.iri);
      }
    }
    // Vectors
    for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.vectorFieldList) {
      // Check field was named
      FieldInterface field = entry.getValue();
      if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, field.field.getName())) continue;
      // Build and execute query for this field
      SelectBuilder vectorQuery = buildVectorQuery(modelNode, entry.getKey());
      if (condition != null) vectorQuery.addWhere(condition);
      vectorQuery.addVar(modelNode).addOrderBy(modelNode);
      JSONArray response = query(vectorQuery.buildString());
      if (response.length() == 0) continue;
      // Identify intervals corresponding to each single Model and read the intervals in
      String previousIri = response.getJSONObject(0).getString(MODEL);
      int end, start = 0;
      String nextIri = null;
      for (end = 1; end < response.length(); end++) {
        nextIri = response.getJSONObject(end).getString(MODEL);
        if (!nextIri.equals(previousIri)) {
          // Only pull objects which have a scalar match
          if (resultIris.contains(previousIri) || !scalar)
            readPullVectorsResponse(getModel(ofClass, previousIri), field, response, start, end);
          start = end;
          previousIri = nextIri;
        }
      }
      if(nextIri!=null && (resultIris.contains(previousIri) || !scalar))
        readPullVectorsResponse(getModel(ofClass, previousIri), field, response, start, end);
    }
    // Note that we do not directly use a HashSet<T> to avoid expensive equals() invocations on Model classes.
    return resultIris.stream().map((resultIri) -> getModel(ofClass, resultIri)).collect(Collectors.toList());
  }

  /**
   * Executes {@link #pullAll(Model)} on the model instance, loading its data from the database, and also does this
   * recursively for all its model references to a depth of {@code recursionRadius}. If the referenced
   * models already exist in this context, they are updated with new values; else, they are created and pulled.
   * <p>
   * Note that this uses a general query for all quads linked to the IRI and filters afterwards, which may be less
   * performant when the {@link Model} only declares a small subset of them; for alternative behaviour, see
   * {@link #pullPartial(Model, String...)}.
   * @param recursionRadius degrees of separation from the original target model through which referenced
   *                        models should be pulled. Models just one degree beyond will be instantiated as
   *                        a hollow model if not already registered in the context, but not pulled.
   */
  public void recursivePullAll(Model model, int recursionRadius) {
    currentPullSession = new RecursivePullSession(recursionRadius, this);
    currentPullSession.queue(model);
    currentPullSession.execute();
    currentPullSession = null;
  }

  /**
   * Populates all fields of the model instance with values from the database and sets all fields clean. {@link Model}
   * references are instantiated as hollow models if not already present in the context; to recursively
   * instantiate/update, see {@link #recursivePullAll(Model, int)}.
   * <p>
   * Note that this uses a general query for all quads linked to the IRI and filters afterwards, which may be less
   * performant when the {@link Model} only declares a small subset of them; for alternative behaviour, see
   * {@link #pullPartial(Model, String...)}.
   */
  public void pullAll(Model model) {
    model.clear();
    for (boolean forward : new boolean[]{false, true}) {
      SelectBuilder query = buildPullAllInDirectionQuery(NodeFactory.createURI(model.iri), forward);
      JSONArray response = query(query.buildString());
      readPullAllInDirectionResponse(model, response, forward, 0, response.length());
    }
    model.setClean();
  }

  /**
   * Reads the query results of a pull-all-in-direction-query into the fields of the {@link Model}.
   * @param start the first index (inclusive) to start reading in rows at.
   * @param end   the index (exclusive) to stop reading in rows at.
   */
  private void readPullAllInDirectionResponse(Model model, JSONArray response, boolean backward, int start, int end) {
    for (int index = start; index < end; index++) {
      JSONObject row = response.getJSONObject(index);
      // If using named graphs (quads), decompose the retrieved graph IRI into a graph name
      String graph = row.optString(GRAPH, "");
      if (graphNamespace != null && graph.startsWith(graphNamespace))
        graph = graph.substring(graphNamespace.length());
      // Do the field lookup and put
      FieldKey key = new FieldKey(graph, row.getString(PREDICATE), backward);
      FieldInterface field = model.metaModel.fieldMap.get(key);
      if (field == null) continue;
      field.put(model, isTruthy(row.getString(ISBLANK)) ? null : row.getString(VALUE), row.optString(DATATYPE));
    }
  }

  /**
   * Composes a query to pull all triples relating to the target IRI.
   * @param model    the {@link Node} of the model for which data is being pulled.
   * @param backward whether to query quads with <code>iriName</code> as the object (true) or subject (false).
   * @return the composed query.
   */
  private SelectBuilder buildPullAllInDirectionQuery(Node model, boolean backward) {
    try {
      // SELECT ?value ?predicate ?datatype ?isblank
      SelectBuilder select = new SelectBuilder()
          .addVar(QM + VALUE).addVar(QM + PREDICATE).addVar(QM + DATATYPE).addVar(QM + ISBLANK);
      // WHERE { <self> ?predicate ?value }   or   WHERE { ?value ?predicate <self> }
      WhereBuilder where = new WhereBuilder()
          .addWhere(backward ? (QM + VALUE) : model, QM + PREDICATE, backward ? model : (QM + VALUE));
      if (isQuads()) {
        select.addVar(QM + GRAPH).addGraph(QM + GRAPH, where);
      } else {
        select.addWhere(where);
      }
      // BIND(datatype(?value) AS ?datatype) BIND(isBlank(?value) AS ?isblank)
      select.addBind(DATATYPE_FUN + OP + QM + VALUE + CP, QM + DATATYPE)
          .addBind(ISBLANK_FUN + OP + QM + VALUE + CP, QM + ISBLANK);
      return select;
    } catch (ParseException e) {
      throw new JPSRuntimeException(e);
    }
  }

  /**
   * Executes {@link #pullPartial(Model, String...)} on the model instance, loading its data from the database, and also
   * does this recursively for all its model references to a depth of {@code recursionRadius}. If the
   * referenced models already exist in this context, they are updated with new values; else, they are created and
   * pulled.
   * <p>
   * If {@code fieldNames} is empty, all fields are pulled; this is different behaviour from {@link #pullAll(Model)}
   * as this uses a specific query for desired values, not a general query for all quads linked to the IRI, and may be
   * more performant when only a small subset is defined in the {@link Model}.
   * @param recursionRadius degrees of separation from the original target model through which referenced
   *                        models should be pulled. Models just one degree beyond will be instantiated as
   *                        a hollow model if not already registered in the context, but not pulled.
   * @param fieldNames      the names of vector and scalar fields to be populated. The same
   *                        {@code fieldNames} is provided to all partial pulls in the recursion; if a
   *                        model does not have a named field, that name is ignored.
   */
  public void recursivePullPartial(Model model, int recursionRadius, String... fieldNames) {
    currentPullSession = new RecursivePullSession(recursionRadius, this, fieldNames);
    currentPullSession.queue(model);
    currentPullSession.execute();
    currentPullSession = null;
  }

  /**
   * Populates the named fields from the target resource. {@link Model} references are instantiated as hollow models if
   * not already present in the context; to recursively instantiate/update, see {@link #recursivePullPartial(Model, int, String...)}.
   * <p>
   * If {@code fieldNames} is empty, all fields are pulled; this is different behaviour from {@link #pullAll(Model)}
   * as this uses a specific query for desired values, not a general query for all quads linked to the IRI, and may be
   * more performant when only a small subset is defined in the {@link Model}.
   * @param model      the {@link Model} to populate.
   * @param fieldNames the names of vector and scalar fields to be populated.
   */
  public void pullPartial(Model model, String... fieldNames) {
    // Scalars
    SelectBuilder scalarsQuery = buildScalarsQuery(NodeFactory.createURI(model.iri), model.metaModel, fieldNames);
    if (scalarsQuery.getVars().size() > 0) {
      JSONArray scalarsResponse = query(scalarsQuery.buildString());
      if (scalarsResponse.length() == 0) throw new JPSRuntimeException(OBJECT_NOT_FOUND_EXCEPTION_TEXT);
      readScalarsResponse(model, scalarsResponse.getJSONObject(0));
    }
    // Vectors
    for (Map.Entry<FieldKey, FieldInterface> entry : model.metaModel.vectorFieldList) {
      FieldInterface field = entry.getValue();
      if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, field.field.getName())) continue;
      SelectBuilder vectorQuery = buildVectorQuery(NodeFactory.createURI(model.iri), entry.getKey());
      JSONArray vectorResponse = query(vectorQuery.buildString());
      readPullVectorsResponse(model, field, vectorResponse, 0, vectorResponse.length());
    }
  }

  /**
   * Reads the query results of a scalars query into the fields of the {@link Model}.
   */
  private void readScalarsResponse(Model model, JSONObject row) {
    for (Map.Entry<FieldKey, FieldInterface> entry : model.metaModel.scalarFieldList) {
      FieldInterface field = entry.getValue();
      if (!row.has(VALUE + field.index)) continue;
      field.put(model,
          isTruthy(row.getString(ISBLANK + field.index)) ? null : row.getString(VALUE + field.index),
          row.optString(DATATYPE + field.index));
      model.cleanValues[field.index] = field.getMinimised(model);
    }
  }

  /**
   * Composes a query to retrieve the values for named scalar fields of this instance.
   */
  private SelectBuilder buildScalarsQuery(Node model, MetaModel metaModel, String... fieldNames) {
    SelectBuilder select = new SelectBuilder();
    for (Map.Entry<FieldKey, FieldInterface> entry : metaModel.scalarFieldList) {
      // Filter for only requested fields
      FieldInterface field = entry.getValue();
      FieldKey key = entry.getKey();
      if (fieldNames.length > 0 && !ArrayUtils.contains(fieldNames, field.field.getName())) continue;
      // Create nodes to use
      Node predicate = NodeFactory.createURI(key.predicate);
      String valueN = QM + VALUE + field.index;
      String datatypeN = QM + DATATYPE + field.index;
      String isBlankN = QM + ISBLANK + field.index;
      // SELECT ?value ?predicate ?datatype ?isblank
      select.addVar(valueN).addVar(datatypeN).addVar(isBlankN);
      // WHERE { <self> <predicate> ?value }   or   WHERE { ?value <predicate> <self> }
      WhereBuilder where = new WhereBuilder().addWhere(key.backward ? valueN : model, predicate, key.backward ? model : valueN);
      if (isQuads()) {
        select.addGraph(NodeFactory.createURI(graphNamespace + key.graphName), where);
      } else {
        select.addWhere(where);
      }
      try {
        // BIND(datatype(?value) AS ?datatype) BIND(isBlank(?value) AS ?isblank)
        select.addBind(DATATYPE_FUN + OP + valueN + CP, datatypeN)
            .addBind(ISBLANK_FUN + OP + valueN + CP, isBlankN);
      } catch (ParseException e) {
        throw new JPSRuntimeException(e);
      }
    }
    return select;
  }

  /**
   * Reads the query results of a vector query into the fields of the {@link Model}.
   * @param start the first index (inclusive) to start reading in rows at.
   * @param end   the index (exclusive) to stop reading in rows at.
   */
  private void readPullVectorsResponse(Model model, FieldInterface field, JSONArray response, int start, int end) {
    field.clear(model);
    for (int i = start; i < end; i++) {
      JSONObject row = response.getJSONObject(i);
      field.put(model, isTruthy(row.getString(ISBLANK)) ? null : row.getString(VALUE), row.optString(DATATYPE));
    }
    model.cleanValues[field.index] = field.getMinimised(model);
  }

  /**
   * Composes a query for all matches of a property described by a {@link FieldKey}, for this instance.
   */
  private SelectBuilder buildVectorQuery(Node model, FieldKey key) {
    Node predicate = NodeFactory.createURI(key.predicate);
    // SELECT ?value ?predicate ?datatype ?isblank
    SelectBuilder select = new SelectBuilder().addVar(QM + VALUE).addVar(QM + DATATYPE).addVar(QM + ISBLANK);
    // WHERE { <self> <predicate> ?value }   or   WHERE { ?value <predicate> <self> }
    WhereBuilder where = new WhereBuilder()
        .addWhere(key.backward ? (QM + VALUE) : model, predicate, key.backward ? model : (QM + VALUE));
    if (isQuads()) {
      Node graph = NodeFactory.createURI(graphNamespace + key.graphName);
      select.addGraph(graph, where);
    } else {
      select.addWhere(where);
    }
    try {
      // BIND(datatype(?value) AS ?datatype) BIND(isBlank(?value) AS ?isblank)
      select.addBind(DATATYPE_FUN + OP + QM + VALUE + CP, QM + DATATYPE)
          .addBind(ISBLANK_FUN + OP + QM + VALUE + CP, QM + ISBLANK);
      return select;
    } catch (ParseException e) {
      throw new JPSRuntimeException(e);
    }
  }

  /**
   * Flags the specified model to be (a) destroyed in the target resource, including all quads described or not
   * described in the Java class definition, and (b) removed from the context's registry, the next time a context-wide
   * push is requested. Note that it is an error to delete a model and create a new one of the same IRI before first
   * pushing the change; the model is still registered to the context between this and the {@link #pushChanges(Model)}.
   */
  public void delete(Model model, boolean zealous) {
    model.state = zealous ? Model.LifeCycle.TO_DELETE_ZEALOUS : Model.LifeCycle.TO_DELETE;
  }

  /**
   * Deregisters the model from the context. The model values may still be inspected, pulled and pushed by direct
   * reference, but pushChanges() will no longer scan the model, and crosslinking and fetching by IRI will no longer see
   * the deregistered model and instead instantiate a new one. The principal use case for this method is "upgrading" an
   * object from a superclass to a subclass; the subclass is instantiated and the old superclass is retired.
   */
  public void retire(Model model) {
    members.remove(new MemberKey(model.getClass(), model.iri));
  }

  /**
   * Pushes all changes in the context. Equivalent to {@link #pushChanges(Model)}} on every member of the context, but more
   * optimised.
   */
  public void pushAllChanges() {
    UpdateRequest deletions = new UpdateRequest();
    UpdateBuilder insertions = new UpdateBuilder();
    Stack<MemberKey> toBeRemoved = new Stack<>();
    boolean anyInserts = false;
    for (Map.Entry<MemberKey, Model> entry : members.entrySet()) {
      Model model = entry.getValue();
      switch (model.state) {
        case LIVE:
          // makeChangeDeltas returns whether any insertions were added
          anyInserts = makeChangeDeltas(model, deletions, insertions) || anyInserts;
          break;
        case TO_DELETE:
          makeDeleteDeltas(model, deletions);
          model.state = Model.LifeCycle.DESTROYED;
          toBeRemoved.add(entry.getKey());
          break;
        case TO_DELETE_ZEALOUS:
          makeDeleteZealousDeltas(model, deletions);
          model.state = Model.LifeCycle.DESTROYED;
          toBeRemoved.add(entry.getKey());
          break;
        case DESTROYED:
          toBeRemoved.add(entry.getKey());
      }
      if (deletions.toString().length() > EXECUTION_CHARACTER_THRESHOLD) {
        update(deletions.add(insertions.build()).toString());
        deletions = new UpdateRequest();
        insertions = new UpdateBuilder();
      }
      entry.getValue().setClean();
    }
    for (MemberKey key : toBeRemoved) members.remove(key);
    // anyInserts tracking is needed since trying to build an empty UpdateBuilder causes an error, and I cannot find
    // a way to probe whether an UpdateBuilder contains any operations.
    if (anyInserts) deletions.add(insertions.build());
    if (deletions.getOperations().size() > 0)
      update(deletions.toString());
  }

  /**
   * Pushes all dirty field values to the database for a model.
   */
  public void pushChanges(Model model) {
    UpdateRequest deletions = new UpdateRequest();
    UpdateBuilder insertions = new UpdateBuilder();
    if (makeChangeDeltas(model, deletions, insertions)) // return value is whether insertions were added
      deletions.add(insertions.build());
    model.setClean();
    if (deletions.getOperations().size() > 0)
      update(deletions.toString());
  }

  /**
   * Determines the changes in a model which should be pushed, and add necessary deletion updates and insertion quads
   * to the given output update builders.
   * @param model         the model to make deltas for.
   * @param deletionsOut  the output destination for deletion updates.
   * @param insertionsOut the output destination for insertion quads.
   * @return whether any insertions were performed.
   */
  private boolean makeChangeDeltas(Model model, UpdateRequest deletionsOut, UpdateBuilder insertionsOut) {
    boolean anyInserts = false;
    for (Map.Entry<FieldKey, FieldInterface> entry : model.metaModel.fieldMap.entrySet()) {
      FieldInterface fieldInterface = entry.getValue();
      FieldKey key = entry.getKey();
      Node self = NodeFactory.createURI(model.iri);
      Node predicate = NodeFactory.createURI(key.predicate);
      Node graph = isQuads() ? NodeFactory.createURI(graphNamespace + key.graphName) : null;
      // Add deletion
      if (model.shouldWriteDelete(fieldInterface)) {
        WhereBuilder where = new WhereBuilder().addWhere(key.backward ? (QM + VALUE) : self, predicate, key.backward ? self : (QM + VALUE));
        if (isQuads()) {
          deletionsOut.add(new UpdateBuilder().addGraph(graph, where).buildDeleteWhere());
        } else {
          deletionsOut.add(new UpdateBuilder().addWhere(where).buildDeleteWhere());
        }
      }
      // Add insertion; technically object properties are duplicate-inserted from both ends, but I don't think it's
      // actually going to be distinguishably more performant if we do a check for this and do it once each instead.
      if (model.shouldWriteInsert(fieldInterface)) {
        for (Node valueValue : fieldInterface.getNodes(model)) {
          Triple triple = new Triple(key.backward ? valueValue : self, predicate, key.backward ? self : valueValue);
          if (isQuads()) {
            insertionsOut.addInsert(new Quad(graph, triple));
          } else {
            insertionsOut.addInsert(triple);
          }
          anyInserts = true;
        }
      }
    }
    return anyInserts;
  }

  /**
   * Adds the updates for the deletion of a model to an output update request. This deletes only quads described in the
   * {@link Model} definition.
   */
  private void makeDeleteDeltas(Model model, UpdateRequest deletionsOut) {
    for (Map.Entry<FieldKey, FieldInterface> entry : model.metaModel.fieldMap.entrySet()) {
      FieldKey key = entry.getKey();
      Node self = NodeFactory.createURI(model.iri);
      Node predicate = NodeFactory.createURI(key.predicate);
      Node graph = isQuads() ? NodeFactory.createURI(graphNamespace + key.graphName) : null;
      WhereBuilder where = new WhereBuilder().addWhere(key.backward ? (QM + VALUE) : self, predicate, key.backward ? self : (QM + VALUE));
      if (isQuads()) {
        deletionsOut.add(new UpdateBuilder().addGraph(graph, where).buildDeleteWhere());
      } else {
        deletionsOut.add(new UpdateBuilder().addWhere(where).buildDeleteWhere());
      }
    }
  }

  /**
   * Adds the updates for the zealous deletion of a model to an output update request. This deletes all quads with the
   * {@link Model}'s IRI as subject or object, not only those described by the {@link Model} definition.
   */
  private void makeDeleteZealousDeltas(Model model, UpdateRequest deletionsOut) {
    Node self = NodeFactory.createURI(model.iri);
    deletionsOut.add(new UpdateBuilder().addWhere(QM + VALUE, QM + PREDICATE, self).buildDeleteWhere());
    deletionsOut.add(new UpdateBuilder().addWhere(self, QM + PREDICATE, QM + VALUE).buildDeleteWhere());
  }

  /**
   * Executes a SPARQL query at the target resource of this context and returns the response.
   * @param query the query string.
   * @return the deserialised {@link JSONArray} of rows in the response.
   */
  public JSONArray query(String query) {
    return AccessAgentCaller.queryStore(targetResourceId, query);
  }

  /**
   * Executes a  SPARQL update at the target resource of this context.
   * @param update the update string.
   */
  public void update(String update) {
    AccessAgentCaller.updateStore(targetResourceId, update);
  }

  /**
   * @return true if the string is <code>true</code> or <code>1</code>.
   */
  public static boolean isTruthy(String str) {
    return str.equals("true") || str.equals("1");
  }

  /**
   * @return the {@link Node} which should be used for the {@link Model} variable in construction of WHERE statements
   * for {@link #pullAllWhere(Class, WhereBuilder)} and {@link #pullPartialWhere(Class, WhereBuilder, String...)}.
   */
  public static Node getModelVar() {
    return NodeFactory.createVariable(MODEL);
  }

}
