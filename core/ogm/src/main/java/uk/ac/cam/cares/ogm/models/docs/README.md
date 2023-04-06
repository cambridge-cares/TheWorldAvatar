The Model Framework is a lightweight Framework for the concise creation of Java classes to interact with structured data
in a knowledge graph. This document outlines how to use it, how it works, and how to extend its functionality. 

# Model Framework User Guide

## Demo

There is a short, heavily annotated file `ModelDemo.java` in this folder which showcases the key features of the 
Framework and walks through some typical design decisions and usage patterns that one may encounter. It and the 
[Using the Model Framework](# Using the Model Framework) section of this README may be read in any order. Also see 
the diagrammatic overview of the classes and methods on offer in `API.pdf`.

## Defining a Model

The `Model` class is the main class used in the Framework to interact with structured data. In general,

- each `Model` subclass (henceforth "a model") corresponds to a class in the knowledge graph ontology, or a part of such
  an ontological class;
- each *field* of a model corresponds to a type of quad involving the node (a role in its ontology); and
- each *instance* of a model corresponds to a node (IRI) of that class in the knowledge graph, and its field values
  correspond to the counterparties to its class-declared roles.

An example model implementation is shown below:

```Java
@ModelAnnotation(defaultGraphName = "employees")
class Employee extends Model {

  @Getter @Setter @FieldAnnotation("http://mycompany.org/ontology#hasName")
  protected String name;

  @Getter @Setter @FieldAnnotation("http://mycompany.org/ontology#hasAge")
  protected Integer age;

  @Getter @Setter @FieldAnnotation("http://mycompany.org/ontology#hasDepartment")
  protected URI department;

  @Getter @Setter
  @FieldAnnotation(
      value = "http://mycompany.org/ontology#manages",
      backward = true,
      graphName = "companyhierarchy")
  protected Employee manager;

  @Getter @Setter
  @FieldAnnotation(
      value = "http://mycompany.org/ontology#manages",
      graphName = "companyhierarchy",
      innerType = Employee.class)
  protected ArrayList<Employee> subordinates;

}
```

The `Employee` model contains five fields, `name`, `age`, `department`, `manager`, and `subordinates`, the last of which is an array. It
describes an Employee ontology where each Employee has

- exactly one `ex:hasName` datatype property of `xsd:string` type in the `employees` graph;
- exactly one `ex:hasAge` datatype property of `xsd:integer` type in the `employees` graph;
- exactly one `ex:hasDepartment` object property in the `employees` graph; and
- any number of `ex:manages` object properties in the `companyhierarchy` graph; also,
- for each Employee there exists exactly one other Employee with object property `ex:manages` with the first Employee as
  the object, in the `companyhierarchy` graph.

These details are specified through each field's `FieldAnnotation`, which characterises the role (quad) the field
corresponds to in its arguments:

- `value`: the IRI of the predicate of the quad described. Qualified names will be looked up in the JPSBaseLib
  PrefixToUrlMap and also custom specifications in the config file. No default value.
- `graphName`: the short name of the graph of the quad, which is appended to the application's target namespace to
  obtain the actual graph IRI used. Defaults to the `defaultGraphName` of the declaring class' `ModelAnnotation`.
- `backward`: whether the declaring model class is the subject or the object of the quad. Default: `false`.
- `innerType`: the class of the elements of the `ArrayList`, if the field is an `ArrayList`. This exists because Java's
  runtime type erasure means this information is not available at runtime even with reflection. Can be left unspecified
  for non-list fields.

The `ModelAnnotation` just provides the `defaultGraphName` as a fallback for fields with unspecified `graphName`. The
Lombok `Getter` and `Setter` are mandatory.

Note that the choice of access level for the annotated fields is purely a stylistic choice. `FieldInterface` uses 
the Lombok accessors and mutators.

An example of compliant data for an Employee "John Smith", formatted in TriG:

```
@prefix    : <http://mycompany.org/>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ont: <http://mycompany.org/ontology#> .
@prefix ppl: <http://mycompany.org/people/> .
@prefix grp: <http://mycompany.org/groups/> .

:employees { ppl:john ont:hasName    "John Smith"^^xsd:string .
             ppl:john ont:hasAge     "26"^^xsd:integer        .
             ppl:john ont:department grp:accounting           . }

:companyhierarchy { ppl:john  ont:manages ppl:sarah .
                    ppl:john  ont:manages ppl:bill  .
                    ppl:edith ont:manages ppl:john  . }
```

Currently supported types for `FieldAnnotation`-annotated fields are:

- `String`
- `Integer` (corresponds to `xsd:int`)
- `BigInteger` (corresponds to `xsd:integer`)
- `Double`
- `java.net.URI`
- Any subclass of `Model`
- Any subclass of `DatatypeModel` (see "Creating a DatatypeModel")
- Any `ArrayList` thereof

Note that non-list ("vector", as opposed to "scalar") fields must always match exactly one value. **If there are 
multiple or 
zero 
matches, 
behaviour is
undefined.**

For an example of a `Model` built for a triple store (without named graphs), see `ModelDemo.java`.

## Working with Models

<object data="./API.pdf" type="application/pdf" width="700px" height="700px">
    <embed src="./API.pdf">
        <p>This browser does not support PDFs. An overview graphic of the API is at API.pdf.</p>

</object>

### Creating a ModelContext

Each `Model` instance must exist in a `ModelContext`. A `ModelContext` stores the access information of the graph database,
the base namespace for graph IRIs (if applicable), and keeps track of instantiated `Model`s to crosslink them when loading
from the database. A `ModelContext` may be a triple context or a quad context, the latter supporting named graphs.

- `ModelContext(String targetResourceId[, int initialCapacity])`

  Creates a triple context with optional specification of the starting capacity of the context, which is used to
  initialise the internal `HashMap` of members. The `targetResourceId` is the ID provided to AccessAgentCaller for 
  SPARQL queries and updates.

- `ModelContext(String targetResourceId, String graphNamespace[, int initialCapacity])`

  Creates a quad context with optional specification of initial capacity. The `graphNamespace` is prepended to the
  graph name definitions in `Model` definitions to create graph IRIs. This enables the same `Model` class to be used
  different namespaces with differently named graph IRIs. For example, fields in a `Building` model class of `graphName`
  `building/` instantiated in a `ModelContext` of `graphNamespace` `http://[...]/namespace/berlin/sparql` will be
  interpreted as using named graph `http://[...]/namespace/berlin/sparql/building/`, while one instantiated in a
  `ModelContext` of `graphNamespace` `http://[...]/namespace/churchill/sparql` will be interpreted as using named
  graph `http://[...]/namespace/churchill/sparql/building/`, which would not be possible if the full IRI had to be
  hardcoded into the `Model` class definition.

### Instantiating a Model

Once a `ModelContext` is created, `Model`s should be created through factory functions of the `ModelContext`. These are:

- `createNewModel(Class<T> ofClass, String iri)`

  Constructor for `Model`s which do not exist in the database. The first time changes are pushed, all fields of the 
  `Model` will be written; unassigned fields will write their default values, typically `null`, translating to blank 
  nodes in the database. Also, deletions will not be executed on first push. See [Pushing changes](#Pushing changes).

- `createHollowModel(Class<T> ofClass, String iri)` 

  Constructor for `Model`s which exist in the database. Creates a "hollow" model, which has its IRI assigned, but 
  all of its fields at default values and disabled: they will never be written, even if changed by the user. A field 
  becomes enabled (resumes normal change tracking behaviour) once it has been populated by a pull method. See 
  [Pushing changes](#Pushing changes).

- `loadAll(Class<T> ofClass, String iri)`

  `loadPartial(Class<T> ofClass, String iri, String... fieldNames)`

  `recursiveLoadAll(Class<T> ofClass, String iri, int recursionRadius)`

  `recursiveLoadPartial(Class<T> ofClass, String iri, int recursionRadius, String... fieldNames)`

  Creates a hollow model with `createHollowModel` and invokes `pullAll`, `pullPartial`, `recursivePullAll` or 
  `recursivePullPartial` on it, populating `Model` fields (recursively) with the respective pull algorithms. See 
  [Pulling data from the knowledge graph](#Pulling data from the knowledge graph) for detail on behaviour.

### Pulling data from the knowledge graph

`Model`s may be populated with data from the database using one of the eight pull methods, which vary on three axes:

#### AXIS 1:  Algorithm: `all` vs. `partial`

The `all` algorithm queries all triples or quads linked to the IRI in question and filters them Java-side for 
field matches:

```
SELECT ?graph ?value ?predicate ?datatype ?isblank
WHERE {
  GRAPH ?graph { <model_iri> ?predicate ?value }
  BIND(DATATYPE(?value) AS ?datatype)
  BIND(ISBLANK(?value) AS ?isblank)
}
```
```
SELECT ?graph ?value ?predicate ?datatype ?isblank
WHERE {
  GRAPH ?graph { ?value ?predicate <model_iri> }
  BIND(DATATYPE(?value) AS ?datatype)
  BIND(ISBLANK(?value) AS ?isblank)
}
```

while the `partial` algorithm builds a specific query for target fields:

```
SELECT ?value1 ?datatype1 ?isblank1 ?value3 ?datatype3 ?isblank3 ?value6 ?datatype6 ?isblank6
WHERE {
  GRAPH <graph_1> { <model_iri> <scalar_role_1> ?value1 }
  BIND(DATATYPE(?value1) AS ?datatype1)
  BIND(ISBLANK(?value1) AS ?isblank1)
  GRAPH <graph_3> { <model_iri> <scalar_role_3> ?value3 }
  BIND(DATATYPE(?value3) AS ?datatype3)
  BIND(ISBLANK(?value3) AS ?isblank3)
  GRAPH <graph_6> { ?value6 <scalar_role_6> <model_iri> }
  BIND(DATATYPE(?value6) AS ?datatype6)
  BIND(ISBLANK(?value6) AS ?isblank6)
}
```
```
SELECT ?value ?datatype ?isblank
WHERE {
  GRAPH <graph_2> { ?value <vector_role_2> <model_iri> }
  BIND(DATATYPE(?value) AS ?datatype)
  BIND(ISBLANK(?value) AS ?isblank)
}
```
```
SELECT ?value ?datatype ?isblank
WHERE {
  GRAPH <graph_3> { ?value <vector_role_3> <model_iri> }
  BIND(DATATYPE(?value) AS ?datatype)
  BIND(ISBLANK(?value) AS ?isblank)
}
```

Aside from allowing users to selectively populate fields, the differences have performance implications:

|               | number of queries           | amount of data returned by query              |
|---------------|-----------------------------|-----------------------------------------------|
| `all`     | 2                           | all triples in DB with IRI as subject/object. |
| `partial` | 1 + number of vector fields | only the values actually desired.             |

`all` is usually more efficient in terms of number of queries (constant vs. scaling with vector field count), but
`partial` decouples performance from the presence of extraneous target-associated triples. Therefore,

- `pullAll` should be preferred when *query latency* is a performance bottleneck and there is more than one vector
  field, so long as the number of target-associated triples in the database is not prohibitively larger than the number
- we are interested in.
- `pullPartial` should be preferred when the former would return too many triples, so long as we are not querying a
  prohibitively large number of vector fields; an empty list of field names can be provided to instruct to pull all
  fields in the model.
- `recursivePullPartial` may also be of particular interest over its `all` counterpart as a shortcut for directional
  discovery, e.g. selecting to only recursively pull a "father" property in a "Person" class to investigate
  patrilineal lineage without loading an exponential number of relatives; see the next part for information on 
  recursive methods.

#### AXIS 2: Recursion: `recursive` vs. non-recursive

Non-recursive (normal) pulls straightforwardly populate the specified model, while recursive pulls also go on to 
execute more recursive pulls on model objects linked in the pull. This is repeated until a specified
"recursion radius" from the original target. A breadth-first traversal with tracking of visited nodes is used to
prevent duplicate pulls or early truncation due to path intersection in non-acyclic graphs.

The way this works is that by default, model-type fields are crosslinked by object reference via IRI lookup within 
the context; if none are found, a hollow model is created for that IRI and that linked.

The recursive methods listen to the lookup step (specifically, `ModelContext.getModel`) and queue the
requested `Model`s to also be pulled later (repeating until exhaustion of `recursionRadius`). What is important to
note from this is that even a `Model` pre-existing in the context will be pulled (updated) if it falls within the
radius of a recursive pull, since the trigger is the lookup, not the creation. *Therefore, a recursive pull on a
`Model` may cause changes in neighbouring `Model`s you are working on to be overwritten.*

`Model`s one step past the recursion radius will be hollow, as secondary `Model`s would be for a non-recursive pull.

#### AXIS 3: Targeting: by name vs. conditional search (`where`)

The base pulls (by name) accept a `Model` argument to populate. Pulls suffixed with `where` accept a SPARQL WHERE 
statement in place of a `Model` instance, and query for all matches for it by replacing the fixed model IRI with a 
variable that is constrained by the WHERE. The number of queries a `where` pull executes is the same as the 
equivalent non-`where` pull, although it retrieves more data and is a bit more complex.

#### Examples

``pullAll`` is implicitly non-recursive and name-targeting. A `Model` instance is provided, and its fields are 
populated with the `all` algorithm, by querying all triples linked to the IRI in the database.

``recursivePullPartialWhere`` accepts a SPARQL WHERE condition and pulls all matches in the database, but only 
pulling the specified fields, using the `partial` algorithm. The returned matches then have their linked models 
recursively `pullPartial`ed until a the given recursion radius. Note that only the fields specified in the pulled 
fields will trigger the recursive pull, although a field which is requested but is not actually modified by the pull 
*will* be caught by the recursive pull. The WHERE condition does not apply to the pulls after the initial search, but 
the field name specified for the `partial` algorithm will be passed on and used in the recursive pulls.

### Pushing changes

Pushing changes to the database is easy. Modify data values directly in the models, and then simply call 
`ModelContext.pushChanges(Model model)` to write the modifications to the database. Alternatively, `ModelContext.
pushAllChanges()` does this for all `Model`s in the context.

When publishing changes, the Framework has a system for determining what updates to write. Each `Model` keeps a 
cache of its field values on last synchronisation with the database (pull or push), and only if the current value of a 
field is different from the cached "clean" value will an update for it be created. The update (collected between 
fields) will take the form of:

```
DELETE WHERE { GRAPH <graph_1> { <model_iri> <scalar_role_5> ?value . } } ;
DELETE WHERE { GRAPH <graph_2> { ?value <scalar_role_7> <model_iri> . } } ;
DELETE WHERE { GRAPH <graph_2> { <model_iri> <vector_role_3> ?value . } } ;
INSERT DATA {
  GRAPH <graph_1> { <model_iri> <scalar_role_5> <scalar_5_value> }
  GRAPH <graph_2> { 
    <scalar_7_value> <scalar_role_7> <model_iri>   .
    <model_iri> <vector_role_3> <vector_3_value_1> .
    <model_iri> <vector_role_3> <vector_3_value_2> .
  }
};
```

A field which has not been pulled before is assigned a special state depending on its origin, which grants special 
treatment during push.

- A `Model` created by `createNewModel` has its fields marked `NEW`, which indicates that a field (a) should be 
  pushed regardless of the current value, and (b) does not need a deletion update to clear the previous value in the 
  database.

- A `Model` created by `createHollowModel` has its fields marked `UNPULLED` which indicates that a field should not be 
  pushed regardless of the current value.

When a field is pushed or pulled—synchronised with the database—its `cleanValues` entry is set to the current value 
(the implementation is actually slightly more complicated than this). If it was previously `NEW` or `UNPULLED`, it thus 
loses that special state and adopts ordinary comparison-based change tracking for push behaviour. Any `Model`s 
returned by `load` methods will therefore have relevant fields (all, or those named to be pulled) active and 
change-tracking already.

A `Model` may be manually "cleaned"—its `cleanValues` set to match its current values—by calling `setClean()` on it, 
with optional specification of field names to clean. Similarly, it may also be forcefully "dirtied" with `setDirty()`, 
which sets the chosen `cleanValues` to yet another special state, `FORCE_PUSH`, which causes fields to always be 
written regardless of current value (*with* deletion, unlike the `NEW` state).

Note that `Model` fields are treated as their IRIs for change-tracking purposes; changes within a referenced `Model` 
will not trigger a push of the referencing object property, as this would not actually result in any modification to 
the database. Nor is there any recursive or propagative behaviour to `pushChanges(Model model)`; to push across multiple 
objects, call them individually or use the context-wide `pushChanges()`.

To delete an object, use `ModelContext.delete(Model model, boolean zealous)` to **flag** an `Model` for deletion. The 
deletion will only be actually executed on the next `ModelContext.pushAllChanges()`. The zealous argument 
determines the deletion mode; if true, this will delete all triples/quads linked to the object in the database; 
otherwise, only fields described by the `Model` will be deleted. The non-zealous update is identical to the 
deletion part of `pushChanges`, and the zealous update looks like:

```
DELETE WHERE { <model_iri> ?predicate ?value      } ;
DELETE WHERE { ?value      ?predicate <model_iri> } ;
```

### Model wrappers for ModelContext methods

Many `ModelContext` methods have wrapping methods in `Model`, such as `model.pushChanges()` being equivalent to
`model.getContext().pushChanges(model)`. The difference is entirely cosmetic.

## Defining a DatatypeModel

`DatatypeModel` is an interface for classes representing, decoding, and encoding custom RDF literals, with support for
polymorphic RDF datatype IRIs within the same `DatatypeModel`. Subclasses of `DatatypeModel` may be used for fields of a 
`Model`. A `DatatypeModel` need not strictly utilise RDF datatypes, however, and it is valid to implement a 
`DatatypeModel` for e.g. custom manipulation for special strings.

A `DatatypeModel` must implement the following:

- `constructor with arguments (String value, String datatype)`

  This is not explicitly described in the interface definition due to language limitations, but it is retrieved by
  reflection at runtime and used by the Framework. The `value` and `datatype` provided in the invocation are 
  respectively the `?value` and `DATATYPE(?value)` strings returned by a query to the database.

- `Node getNode()`

  Returns a Jena `Node` object encoding this object. This should be reversible with the constructor in the sense that if

    ```java
    Node node = obj1.getNode();
    MyDatatypeModel obj2 = new MyDatatypeModel(
        node.getLiteralLexicalForm(),
        node.getLiteralDatatypeUri()
    );
    ```

  then `obj1.equals(obj2)` should be `true`.

For an example, see `uk.ac.cam.cares.twa.cities.models.geo.GeometryType`.

## Notes on design and use

### Model inheritance and IRI-sharing

While it is an error to attempt to create an instance of the same class and IRI as an existing instance in a 
context, two models of the same IRI but different class are allowed. Lookups will work as normal, since all lookup 
methods take bot IRI and class as argument.

Similarly, it is legal and supported for a non-abstract `Model` to extend another non-abstract `Model`, and they 
will be consequently considered different classes. This means that it is possible to have the same object with 
duplicated properties in a single context: e.g. if John is instantiated as an `Employee` and also as a `Manager 
extends Employee`, then there will be two sets of `Employee` fields being tracked and managed separately.

This is not recommended. Two options are suggested:

- Always keep an object as the most derived class in the ontology. If an object is originally instantiated as a 
  superclass and then later identified as a subclass, the old instance can be deregistered with the `retire` method 
  on the `Model` or `ModelContext`. The disadvantage of this pattern is that if, for example, John is represented as 
  a `Manager extends Employee`, and his boss is loaded into the context, then his boss' `subordinates` field of type 
  `ArrayList<Employee>` will look up John's IRI as the *Employee* class, resulting in a hollow **Employee** of John 
  being created in parallel with the `Manager` John we are maintaining. It is easy for accidental duplication to 
  sneak in.

Alternatively, one can:

- Not use inheritance. Even if, in the OWL ontology, *Manager* is a subclass of *Employee*, define the `Manager` 
  Java class as only the fields that *Manager* adds to *Employee* and inherit directly from `Model`. Then, work with 
  both `Manager` and `Employee` of John instantiated in the context, and cross-lookup with e.g. `context.getModel
  (Manager.class, employee.getIri())` where necessary.

### Doubly described triples

This is also partially covered in the `ModelDemo` example. Many triples may be described from both ends, e.g. 
`subordinates` and `manager` are mutually inverse, or `parent` and `child`. Since `parent.getChildren()` and `child.
getParents()` are both useful, I do not consider it an antipattern to have triples thus "doubly described".

The problem comes in where we want to mutate the system. There are two approaches:

- Mutate the triple from both ends. If `john.manager = sam`, then `sam.subordinates.add(john)`. This can be 
  implemented via a function e.g. `Employee.reassign(Employee newManager)` which consistently unsubordinates an 
  employee from their previous manager, subordinates them to their new manager, and sets their manager to the new 
  manager.
  
  Advantages:
  - No need to worry about conflicting updates or internal inconsistencies in the system.
  - If the action is encapsulated as above, then the code is not significantly complicated.
  
  Disadvantages:
  - The performance of updates is very slightly worsened due to having to process the duplicated update.
  - Less granular control of update behaivour.

- Mutate the triple from a single end, and do not modify the other end. For example, only set *john.manager*. Since 
  *sam.subordinates* has not been modified, it will not trigger the updater, and there will be no conflicting update 
  written to the database.

  Advantages:
  - Slightly improved performance.
  - Somewhat shorter code on first glance.
  - More granular control of update behaviour.
  
  Disadvantages:
  - Difficult for the developer to keep track of as use cases grow larger.
  - Silent desynchronisation of data in Java and in the database: after pushing an update, `sam` believes that they 
    are synchronised, but in reality the database has changed.

The mentions of `control of update behaviour` may be confusing, as it may appear that there should be no difference 
in database-side function. This is not precisely true: for one, silent desynchronisation may ruin your day, as 
mentioned in the disadvantages of the second scheme. If `john` has quietly reassigned himself to `sam` without 
updating `sam.subordinates` and pushes, and then `jessica` reassigns herself to `sam` and *does* update `sam.
subordinates` and pushes (all of this discussion of agency being figurative; we are all operating within the same 
context and agent), then `sam` will push his `john`-free subordinate list to the database, and `john` will be abruptly 
orphaned.

More in general, this is most notably a problem with many-to-one relationships, and not strictly an intra-agent 
problem. *Adding something to a list causes the whole list to be written, potentially overwriting other changes, 
both from within your program and by other agents*. Take for example a knowledge graph keeping track of people's 
preferences. The models are simply `Person.likes.ArrayList<Fruit>` and `Fruit.isLikedBy.ArrayList<Person>`, 
respectively the predicates `ontofruit:likes` and `inverse(ontofruit:likes)` in the OWL ontology.
Each real-life person is responsible for maintaining their `Person` in the graph.

Jeff decides he likes bananas and wants to add his like in the graph. He **does not** want to do
```
Fruit banana = context.loadAll(Fruit.class, "banana");
Person jeff = context.getModel(Person.class, "jeff");
banana.isLikedBy.add(jeff);
banana.pushChanges();
```
because this code performs e.g.
```
DELETE WHERE { ?any ontofruit:likes <banana> }
INSERT DATA {
  <jeff> ontofruit:likes <banana> . 
  <alice> ontofruit:likes <banana> .
  <eve> ontofruit:likes <banana> .
}
```
and if between Jeff's pull and push, Eve removed her like of bananas, or Edith added a like of bananas, those 
changes would be overwritten. Instead, he should do
```
Person jeff = context.loadAll(Person.class, "jeff");
Fruit banana = context.getModel(Fruit.class, "banana");
jeff.likes.add(banana);
jeff.pushChanges();
```
because this will execute
```
DELETE WHERE { <jeff> ontofruit:likes ?any }
INSERT DATA {
  <jeff> ontofruit:likes <banana> .
  <jeff> ontofruit:likes <apple> .
  <jeff> ontofruit:likes <pear> .
}
```
and Jeff knows that this will not lead to overwriting anyone's changes, because everyone is supposed to be only 
responsible for their own likes. In this specific case, doing a bilateral mutation still lets you do selective 
updating in this fashion because we are calling `pushChanges` on a particular instance instead of
`context.pushAllChanges()`, but in a more complicated routine, that may not e always possible.

# Model Framework Developer Guide

## How it works

### High level

<object data="./API.pdf" type="application/pdf" width="700px" height="700px">
    <embed src="./API.pdf">
        <p>This browser does not support PDFs. An overview graphic of the API is at MetaModel.pdf.</p>
</object>

For each `Model` subclass, the first time an instance is initialised, a `MetaModel` is generated, which is shared by
all future instances. A `MetaModel` contains a `HashMap` with keys `FieldKey`, converted one-to-one from
`FieldAnnotation`, and values `FieldInterface`, collections of field read/write methods generated by reflection on
each model field.

This `HashMap` is used by `ModelContext` methods to interface between the `Model` and SPARQL data. For
example, during `pullAll`, the predicate, graph and direction of query response rows are converted into a `FieldKey`
and used to look up the matching `FieldInterface` in the `MetaModel` of the class. The value string and datatype
string of the query response row is then injected into the object to be populated with the `FieldInterface`.

### FieldKey

A `FieldKey` is a hashable, comparable object encoding the quad characterisation information in a `FieldAnnotation`. It
has fields:

- `predicate`: the full IRI of the predicate, copied or expanded from `FieldAnnotation.value`.
- `graphName`: short name of the graph, from `FieldAnnotation.graphName` if specified, else
  `ModelAnnotation.defaultGraphName`.
- `backward`: the same as `FieldAnnotation.backward`.

It serves as (a) a lookup key and (b) a sorting key for fields, the latter of which facilitates graph-based grouping 
in queries.

### FieldInterface

A `FieldInterface` is a class, not an interface in the Java language sense. One is created for each field with a
`FieldAnnotation`. During construction, it builds and stores a collection of functions to interact with its target
field based on the field type and annotation information. These are:

- Builtin methods fetched by reflection:
    - `getter`: the Lombok getter of the field.
    - `setter`: the Lombok setter of the field.
- Custom "outer-dependent" functions:
    - `listConstructor`: the constructor for an empty list, only assigned if the field is a list.
    - `putter`: the action for consuming an input value; for a list, this appends, otherwise, it sets (overwrites).
- Custom "inner-dependent" functions:
    - `parser`: converts string input (e.g. from a query) into the field's type.
    - `nodeGetter`: converts an object of the field's type into a Jena `Node`.
    - `minimiser`: converts an object of the field's type into a minimal representation for which
      if `nodeGetter(a).equals(nodeGetter(b))`, then `minimiser(a).equals(minimiser(b))`.

The methods exposed by `FieldInterface` wrap these functions for streamlined use by `Model`. They main ones are:

- `put`: wrapper for a composition of `putter`⋅`parser`.
- `clear`: sets the field to its default value, which is the output of `listConstructor` for a list, otherwise `null`.
- `getMinimised`: returns the output of `minimiser` on the field value, unless it is a list, in which case returns a
  list of the outputs of `minimiser` on each element.
- `getNodes`: returns the outputs of `nodeGetter` on elements of the field value; for non-lists, this has length 1.

### MetaModel

A `MetaModel` is created for each `Model` subclass the first time an instance thereof is created; all future instances
will then link back to the same `MetaModel`. Conceptually, it may be thought of as the collected output of 
reflection-based runtime annotation processing, which is stored for use across all instances.

The core element of each `MetaModel` is the `FieldKey`-indexed collection of `FieldInterface`s for its target class,
`TreeMap<FieldKey, FieldInterface> fieldMap`. This serves as the engine through which the `Model` base methods interact 
with the annotated fields declared by subclasses.

The other fields in `MetaModel` are `scalarFields` and `vectorFields`, which are simply the scalar (non-list)
and vector (list) entries in `fieldMap` extracted for convenience.

The use of `TreeMap` is deliberate so the entries are sorted by key.

### Tying it all together

`MetaModel`, `FieldKey` and `FieldInterface` are leveraged together in the main methods provided by the `Model` base
class.

- On `pullAll`, the graph database is queried for all quads containing the model instance's IRI as the subject or
  object. Each row of the response is processed as such:
  - The predicate, graph and direction of the quad are compiled into a `FieldKey`.
  - The `FieldKey` is looked up in `metaModel.fieldMap` to retrieve the corresponding `FieldInterface`.
  - The value and datatype of the counterparty in the quad is injected into the instance by `fieldInterface.put`. The
    conversion to the field's Java type and handling of lists is all compartmentalised inside `FieldInterface`.
  - A minimised copy of the new value is retrieved by `fieldInterface.getMinimised` and saved in `cleanValues`.
- On `pullPartial`, we:
  - Iterate through `metaModel.scalarFields` to build a combined query from the `FieldKey`s of requested fields.
  - Iterate through `metaModel.vectorFields` to build a separate query for each vector `FieldKey` requested.
  - Inject the response values into the `Model` through the respective `FieldInterface`s (and save minimised copies to 
    `cleanValues`)
- On `pushChanges`, we iterate through `metaModel.fieldMap`, and for each entry:
  - The current value (minimised), retrieved via the `FieldInterface`, is compared to the counterpart in `cleanValues`
    to determine if the field is dirty.
  - If so, updates to delete of the existing quad(s) and insert of the new quad(s) are built with
   `fieldInterface.getNode(s)`.
  - Minimised values are written to `cleanValues`.

The `loadAllWhere` and `loadPartial` methods use the same query builders as `pullAll` and `pullPartial`, but replace 
the `Model` IRI with a `?model` variable Node, and then append (a) search condition to describe `?model`, and (b) `ORDER 
BY ?model`. The response is decomposed into segments corresponding to different individuals, and each segment read 
in by the same response processors as `pullAll` and `pullPartial`. 

## Notes for future development

### How to add new types

#### Method 1: Direct addition to FieldInterface

Support for different types is implemented in the constructor of `FieldInterface`. Simply add an additional 
condition to the `innerType` interrogation section, capturing the type to be added and setting their `parser`, 
`nodeGetter` and `minimiser` functions.

The new type should also be added to the tests for the package. Add a field of the new type to `TestModel`, and add 
a new test to `FieldInterfaceTests` following the pattern of other types. `FieldInterfaceTests` is already nicely 
abstracted to make this quick and easy. The field counts in MetaModelTest must also be incremented to reflect the 
increased size of TestModel.

This method is appropriate for relatively common types such as date and time types, numeric types, etc.

#### Method 2: Creation of a DatatypeModel

See [Defining a DatatypeModel](#Defining a DatatypeModel). This is more appropriate if you need behaviour for a 
particular use case.

### Why does deletion work like that?

`DELETE WHERE` is used in place of `DELETE DATA` in the deletion parts of the change-pushing updates. This is, I 
think, justified by there being a change-tracking system: properties which have not modified will not be republished 
and overwrite changes by other agents or uses; modified properties will overwrite other changes; this is standard 
behaviour for many systems. Some alternative behaviours are:

- Properties which have been changed are not overwritten, and instead silently duplicated, keeping both the old and 
  new triple. This is undesirable as it will silently create violations of ontologies and necessitate clean-up agents or 
  routines. An advantage is that it will likely perform better than the current deletion algorithm, but the benefits 
  are slim.

- Trying to write to properties which have been changed in the database since last pull throws an exception. This 
  theoretically enables an almost superset of current behaviour, since the end-user developer may catch that 
  exception and in response do an override or pull and re-push. However, it is clunky and introduces a lot of new 
  degrees of complexity into both the Framework and the user experience; the one must not only interact with current 
  values and a simple clean/dirty state, but also micromanage congruence between the clean reference and the 
  database. Finally, querying every time we push is likely to be expensive, so an option will have to be included to 
  skip this step. All in all, such a solution would introduce significant complexity to enable a feature which few 
  would use due to the performance impact and limited use case.

It is possible there is an elegant solution I have not thought of. If so, feel free to implement it.
