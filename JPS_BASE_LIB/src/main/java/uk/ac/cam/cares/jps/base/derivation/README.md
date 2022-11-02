# Namespace
The namespaces used in this document:
```
: https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#
msm : http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#
time : http://www.w3.org/2006/time#
rdf : http://www.w3.org/1999/02/22-rdf-syntax-ns#
```

# A derivation instance
An instance consisting of more than one entity is wrapped around a derivation instance using the `belongsTo` property, in the form of:
```
<entity1> <:belongsTo> <Derivation>
<entity2> <:belongsTo> <Derivation>
```
There is no limit on the number of entities per derivation instance.

Inputs for a derivation is marked using the `isDerivedFrom` property, there is no limit on the number of inputs:
```
<Derivation> <:isDerivedFrom> <input1>
<Derivation> <:isDerivedFrom> <input2>
```
The inputs can be a part of another derivation instance, i.e. 
```
<input> <:belongsTo> <AnotherDerivation>
```
or an instance that is not part of a derivation. The DerivationClient uses the `isDerivedFrom` property to query the inputs for a derivation instance and supply these in the HTTP request for the agent responsible for this derivation.

To enable asynchronous operation, `Status` are introduced to mark the state of a derivation, i.e.
```
<Derivation> <:hasStatus> <status>
<status> <rdf:type> <:Status>
```
available status (subclasses of `Status`) are: `Requested`, `InProgress`, and `Finished`. A specific object property of `Finished` is `hasNewDerivedIRI` that is used to temporarily store any generated derived IRI during the derivation update, i.e.
```
<status> <rdf:type> <:Finished>
<status> <:hasNewDerivedIRI> <newEntity1>
<status> <:hasNewDerivedIRI> <newEntity2>
```
There is no limit on the number of new entities to be connected via `hasNewDerivedIRI` per derivation instance.

And lastly, each derivation requires an agent attached to it and the agent is marked using the `isDerivedUsing` property:
```
<Derivation> <:isDerivedUsing> <Agent>
<Agent> <msm:hasOperation> <Operation>
<Operation> <msm:hasHttpUrl> <URL>
```
Each agent is assumed to be a servlet and must have a URL. It is assumed that each agent has a single URL/operation, this may be extended if deemed necessary in the future.

Upon initialisation, each derivation has a unix timestamp attached using the W3C standard:
```
<Derivation> <time:hasTime> <time>
<time> <rdf:type> <time:Instant>
<time> <time:inTimePosition> <unix_time>
<unix_time> <rdf:type> <time:TimePosition>
<unix_time> <time:hasTRS> <http://dbpedia.org/resource/Unix_time>
<unix_time> <time:numericPosition> 123
```

Methods are provided in the `DerivationClient` to add and query these instances.

For synchronous agent responses, there are two types of derivations
1. Derivation without time series, initialised using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.createDerivation(List<String>, String, String, List<String>)`
   - e.g. createDerivation(entities, agentIRI, agentURL, inputs)
   - The entities wrapped around the derivation instance are deleted and replaced each time the agent creates new instances.
   - The created derivation instances have rdf:type as `Derivation`.

2. Derivation with time series, initialised using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.createDerivationWithTimeSeries(List<String>, String, String, List<String>)`
   - e.g. createDerivationWithTimeSeries(entities, agentIRI, agentURL, inputs)
   - It is assumed that the agents acting on these derivations add row(s) to some table(s), the entities under these derivations are not deleted and replaced.
   - The created derivation instances have rdf:type as `DerivationWithTimeSeries`.

3. Derivation with or without time series, created for generating new information, initialised using `createSyncDerivationForNewInfo(String, List<String>, String)` or `createSyncDerivationForNewInfo(String, String, List<String>, String)`
   - e.g. createSyncDerivationForNewInfo(agentIRI, inputsIRI, derivationType) when `agentURL` is unknown by the developer, in which case the agent HTTP endpoint will be queried by the framework
   - Or developer can provide such HTTP endpoint to the method and use createSyncDerivationForNewInfo(agentIRI, agentURL, inputsIRI, derivationType) *NOTE that developer MUST make sure the provided `agentIRI` and `agentURL` matches with each other.*
   - When calling this method, an HTTP request will be fired to the `agentURL` endpoint to request for computing new information with the provided inputs, i.e. the outputs of this derivation are to be generated. Therefore the developer MUST ensure the HTTP endpoint of the agent is up and running, also the computation is quick enough compared to the HTTP timeout setting. Moreover, *NONE* of the `inputsIRI` should be derived information of asynchronous derivations.
   - The returned type of this method is `uk.ac.cam.cares.jps.base.derivation.Derivation`, developer can get the IRI of the created derivation and list of IRIs of the generated outputs via:
     ```java
     // create a derivation instance and get its outputs with a rdf:type of interest <http://this_is_a_specific_rdfType>
     Derivation aDerivation = devClient.createSyncDerivationForNewInfo(agentIRI, inputsIRI, DerivationSparql.ONTODERIVATION_DERIVATION);
     String derivationIRI = aDerivation.getIri();
     List<String> outputsIRIsOfRdfType = aDerivation.getBelongsToIris("http://this_is_a_specific_rdfType");
     ```
   - This initialisation thus supports creating a graph of synchronous derivations on-the-fly without creating placeholder instances in the knowledge graph when only pure inputs exist:
     ```java
     // create a downstream derivation that takes the outputs of the derivation instance created just now as inputs
     Derivation downstreamDerivation = devClient.createSyncDerivationForNewInfo(anotherAgentIRI, outputsIRIsOfRdfType, DerivationSparql.ONTODERIVATION_DERIVATION);
     // again we retrieve its outputs with the interested rdf:type <http://specific_rdfType_of_downstream_derivation_outputs>
     List<String> outputsOfDownstreamDerivation = derivation.getBelongsToIris("http://specific_rdfType_of_downstream_derivation_outputs");

     // we can then combine these inputs to form yet another downstream derivation
     List<String> combinedInputsIRI = Stream.concat(outputsIRIsOfRdfType.stream(), outputsOfDownstreamDerivation.stream()).collect(Collectors.toList());
     Derivation yetAnotherDownstreamDerivation = devClient.createSyncDerivationForNewInfo(yetAnotherAgentIRI, combinedInputsIRI, DerivationSparql.ONTODERIVATION_DERIVATION);
     ```

For asynchronous agent operations, the derivation framework currently supports:
1. Derivation without time series, created given the input instances, initialised using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.createAsyncDerivation(List<String>, String, List<String>, boolean)`
   - e.g. createDerivation(entities, agentIRI, inputs, forUpdate)
   - The entities wrapped around the derivation instance are deleted and replaced each time the agent creates new instances.
   - The boolean value `forUpdate` is used to flag the purpose of the derivation instance been created:
     - `true`: this derivation is created for updating information, the derivation instance will be marked as `Requested` at its creation, also the timestamp will be initialised as 0
     - `false`: this derivation is created for mark up the information dependency only, the derivation instance will be left with no status at its creation, also the timestamp will be initialised as the current timestamp
   - It should be noted that this initialisation is different from the one that is used for synchronous agent responses, as here it assumes below triples about agent are known and exist in the knowledge graph already:
     ```
     <Agent> <msm:hasOperation> <Operation>
     <Operation> <msm:hasHttpUrl> <URL>
     ```
   - The created derivation instances have rdf:type as `DerivationAsyn`.

2. Derivation without time series, created given the upstream derivation, initialised using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.createAsyncDerivation(List<String>, String, String, boolean)`
   - e.g. createDerivation(entities, agentIRI, derivation, forUpdate)
   - This method takes the IRI of another derivation as input, the derivation framework matches the output instances of the provided upstream derivation with the input signature of the provided agentIRI that monitors the downstream derivation to be created. The matched instances are finally structured as a `List<String>` to be used for creating the downstream derivation in the same fashion as the method above.

3. Derivation without time series, created for generating new information, initialised using `createAsyncDerivationForNewInfo(String, List<String>)`
   - e.g. createAsyncDerivationForNewInfo(agentIRI, inputsAndDerivations)
   - This method creates derivation for generating new information, i.e. the outputs of this derivation are to be generated.
   - This initialisation supports creating new derivation that depends on upstream derivations which themselves can be created for generating new information, i.e. no outputs yet:
     ```
     <Derivation> <:isDerivedFrom> <UpstreamDerivation>
     ```
   - Once the upstream derivation finishes generating new information, e.g. generated `<newEntity1>` and `<newEntity2>`, the derivation framework will handle the reconnection of new information to the derivation structure in the knowledge graph:
     ```
     <newEntity1> <:belongsTo> <UpstreamDerivation>
     <newEntity2> <:belongsTo> <UpstreamDerivation>
     <Derivation> <:isDerivedFrom> <newEntity1>
     <Derivation> <:isDerivedFrom> <newEntity2>
     ```

# Timestamps
The DerivationClient compares the timestamp of the derivation instance with its input(s) to determine whether it's out-of-date. Timestamps for pure inputs can be added manually using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.addTimeInstanceCurrentTimestamp(String)`. This will attach a time instance with current timestamp. Or, a current timestamp will be automatically attached to the pure inputs if they don't have a timestamp already when marking up each derivation instance.

# DerivationAgent
To achieve one derivation agent that is able to handle both synchronous and asynchronous derivations, it is assumed that all agents extend the `uk.ac.cam.cares.jps.base.agent.DerivationAgent` class, which itself extends the `uk.ac.cam.cares.jps.base.agent.JPSAgent` class. Two constructor are provided in the class: `DerivationAgent()` (the default constructor) and `DerivationAgent(StoreClientInterface, String)`. The default constructor is required to allow the servlet to create the very first instance of `DerivationAgent` class before calling the `init()` method (which will be provided in the concreate agent implementation), see https://docs.oracle.com/javaee/6/tutorial/doc/bnafi.html. When extending the `DerivationAgent` class, the developer **MUST** provide both constructor in the same format as the two provided here and call the constructor of super class. For an example implementation, please refer to the links provided in [`Example`](#example).

`DerivationAgent` uses two data container classes `uk.ac.cam.cares.jps.base.derivation.DerivationInputs` and `uk.ac.cam.cares.jps.base.derivation.DerivationOutputs` as a more standardised way compared to JSONObject. The serialisation and deserialisation between JSONObject and DerivationInputs/DerivationOutputs are taken care of by the DerivationAgent. Under the hood, both DerivationInputs and DerivationOutputs use `Map<String, List<String>>` that takes the rdf:type as the key and the list of instance IRIs as the values:
```
{
  rdf_type1=[iri1],
  rdf_type2=[iri2, iri3, iri4,...]
}
```
These key-value pairs should follow the I/O signature defined in the OntoAgent instance of the specific agent. For more details on what OntoAgent OWL contains, please refer to [An agent composition framework for the J-Park Simulator - A knowledge graph for the process industry](https://doi.org/10.1016/j.compchemeng.2019.106577). **NOTE: (1) that the derivation agent needs the instantiated OntoAgent OWL to be placed in the same triple store where the derivation instances are created - an utility method is provided as `uk.ac.cam.cares.jps.base.derivation.DerivationClient.createOntoAgentInstance(String, String, List<String>, List<String>)` for this purpose, developer may call this method as part of agent's initialisation method to register the agent in triple store (the method doesn't add duplicated triples when re-starting agent's container); (2) all instances to be marked as inputs of derivation MUST have rdf:type, otherwise it will not be picked up by the agent for update.**

When developing a new derivation agent, developer only need to implement the agent logic that converts instances of DerivationInputs to DerivationOutputs by overriding `uk.ac.cam.cares.jps.base.agent.DerivationAgent.processRequestParameters(DerivationInputs, DerivationOutputs)`.

Upon receiving the inputs, developer may check the complete agent inputs by using `DerivationInputs.getInputs()`, or retrieve list of IRIs of specific rdf:type by using `DerivationInputs.getIris(String)`. Once the calculation is done, the new created instances are expected to be put in the instance of DerivationOutputs and provided as an argument to the method `uk.ac.cam.cares.jps.base.agent.DerivationAgent.processRequestParameters(DerivationInputs, DerivationOutputs)`. Developer can add the new created instances and new created triples to outputs by calling below methods:
 - `derivationOutputs.createNewEntity(String, String)`
 - `derivationOutputs.createNewEntityWithBaseUrl(String, String)`
 - `derivationOutputs.addTriple(TriplePattern)` _(note that this method can be used to add both IRI triple and literal triple as the s-p-o statement is now provided in TriplePattern format, the parsing will be handled behind-the-scenes)_
 - `derivationOutputs.addTriple(List<TriplePattern>)`
 - `derivationOutputs.addTriple(String, String, String)` _(different from `addTriple(TriplePattern)` and `addTriple(List<TriplePattern>)`, this method should be used when the object in the s-p-o statement is denoted by an IRI, so called referent, see https://www.w3.org/TR/rdf11-concepts/#dfn-referent)_
 - `derivationOutputs.addLiteral(String, String, String)` _(all `addLiteral` should be used when the object in the s-p-o statement is denoted by a literal, so called literal value, see https://www.w3.org/TR/rdf11-concepts/#dfn-literal-value)_
 - `derivationOutputs.addLiteral(String, String, Number)`
 - `derivationOutputs.addLiteral(String, String, Boolean)`
 - `derivationOutputs.addLiteral(String, String, String, String)` _(Can be used to add literal triples with custom data type, e.g., `<subject> <object> "48.13188#11.54965#1379714400"^^<http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon-time>`)_

For example, if your agent creates below information after calculation:

```
<newDerivedQuantity> <rdf:type> <Quantity> .
<newDerivedQuantity> <hasValue> <valueIRI> .
<valueIRI> <rdf:type> <QuantityValue> .
<valueIRI> <hasUnit> <unit> .
<valueIRI> <hasNumericalValue> 5 .
```

You can execute below lines in your codes (**NOTE: all provided IRIs must be complete IRIs, i.e., start with "http://" or "https://", NO prefix should be used**):

```java
derivationOutputs.createNewEntity("<newDerivedQuantity>", "<Quantity>");
derivationOutputs.createNewEntity("<valueIRI>", "<QuantityValue>");
```

or if you would like the `<newDerivedQuantity>` and `<valueIRI>` to be generated automatically, you can call:
```java
String newDerivedQuantity = derivationOutputs.createNewEntityWithBaseUrl("<baseUrl>", "<Quantity>");
String valueIRI = derivationOutputs.createNewEntityWithBaseUrl("<valueIRI>", "<QuantityValue>");
```

Assuming the `<baseUrl>` has value `http://baseUrl` and `<Quantity>` has value `http://example.com#Quantity`, the IRI `newDerivedQuantity` created by the method `createNewEntityWithBaseUrl` will be in the format of ```http://baseUrl/Quantity_UUID```, where the `UUID` will be generated using `java.util.UUID.randomUUID().toString()`.

For adding triples, you can directly use below functions:

```java
derivationOutputs.addTriple("<newDerivedQuantity>", "<hasValue>", "<valueIRI>");
derivationOutputs.addTriple("<valueIRI>", "<hasUnit>", "<unit>");
derivationOutputs.addLiteral("<valueIRI>", "<hasNumericalValue>", 5);
```

or if you prefer to use `org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern` and `org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri`, below lines have the same effect when adding triples:

```java
derivationOutputs.addTriple(Rdf.iri("<newDerivedQuantity>").has(Rdf.iri("<hasValue>"), Rdf.iri("<valueIRI>")));
derivationOutputs.addTriple(Rdf.iri("<valueIRI>").has(Rdf.iri("<hasUnit>"), Rdf.iri("<unit>")));
// NOTE below the literal is added using addTriple as the whole s-p-o statement is now provided in TriplePattern format
derivationOutputs.addTriple(Rdf.iri("<valueIRI>").has(Rdf.iri("<hasNumericalValue>"), 5));
```

The SPARQL update to write these triples and update the derivation will be handled by the derivation framework behind the scenes. The SPARQL update will be executed in a way that ensures NO duplicated information will be populated to the knowledge graph in the situation of concurrent HTTP request for updating the same piece of information generated by instances of normal `OntoDerivation:Derivation`. **NOTE: Developer should make sure that they DO NOT perform ANY SPARQL update by themselves in agent operation.**

## Agents (synchronous response)
### Derivation without time series
The HTTP request made by the DerivationClient will be a JSON object with the key `uk.ac.cam.cares.jps.base.derivation.DerivationClient.AGENT_INPUT_KEY`. Within this JSON object is a JSON object consisting of key-value pairs of IRIs marked using `isDerivedFrom` for the derivation being updated.
```json
{"agent_input":
  {
		"input_rdf_type1": [iri1],
		"input_rdf_type2": [iri2, iri3, iri4,...]
	}
}
```
Upon receiving these inputs, the DerivationAgent will serialise the JSON object within the `uk.ac.cam.cares.jps.base.derivation.DerivationClient.AGENT_INPUT_KEY` key to an instance of DerivationInputs and pass it to method `uk.ac.cam.cares.jps.base.agent.DerivationAgent.processRequestParameters(DerivationInputs, DerivationOutputs)`. The time instant immediately before passing the inputs to be processed will be recorded. The constructed instance of DerivationOutputs will be processed and a SPARQL update string will be formulated and fired by the agent by calling method `uk.ac.cam.cares.jps.base.derivation.DerivationClient.reconnectNewDerivedIRIs(List<TriplePattern>, Map<String, List<String>>, String, Long)`. This method is implemented in a way that is robust to concurrent HTTP requests for updating the same piece of information. If there's no error, the recorded timestamp will be included in the HTTP response with the `uk.ac.cam.cares.jps.base.derivation.DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY` key.

### Derivation with time series
The key difference for derivations with time series is that the DerivationClient does not delete and replace the instances, it only updates the timestamp upon calling the agent. Agents for these derivations should receive HTTP requests in the same manner, perform the necessary calculations, and record the results in a time series table. Unlike derivations without time series, the DerivationAgent does not construct/fire SPARQL update - it only return the timestamp stored in `uk.ac.cam.cares.jps.base.derivation.DerivationOutputs.RETRIEVED_INPUTS_TIMESTAMP_KEY`.

If there are no thrown exceptions, the DerivationClient will assume that the update is successful and update the timestamp of the derivation instance, also the new generated instances in cached data if applicable.

## Agents (asynchronous operation)
### Derivation without time series
By design, the derivation framework only marks the derivation as `Requested` at creation if it is for update or generating new information. It is agents' responsibility to use the methods provided by the DerivationClient to monitor the `Status` of derivations and conduct any job requested. The complete job input should be a JSON string with the key `uk.ac.cam.cares.jps.base.derivation.DerivationClient.AGENT_INPUT_KEY`. Within this JSON string are JSON objects of IRIs marked using `isDerivedFrom` for the derivation being updated. In case of multiple IRIs instantiated from the same rdf:type, they should be grouped together based on the I/O signiture declared in its OntoAgent instance.
```json
{"agent_input": 
  {
		"input_rdf_type1": [iri1],
		"input_rdf_type2": [iri2, iri3, iri4,...]
	}
}
```
The key of each JSON object within the JSON input string is decalred in the corresponding OntoAgent I/O specifications, i.e.
```
<Agent> <msm:hasOperation> <Operation>
<Operation> <msm:hasInput> <InputMessageContent>
<InputMessageContent> <msm:hasMandatoryPart> <InputMessagePart1>
<InputMessageContent> <msm:hasMandatoryPart> <InputMessagePart2>
<InputMessagePart1> <msm:hasType> <input_rdf_type1>
<InputMessagePart2> <msm:hasType> <input_rdf_type2>
```
Following this practice, the input IRI instances of any agent are mapped against their rdf:type, enabling standardised agent implementation utilising OntoAgent. 

Upon detecting the `Requested` derivation by the DerivationAgent, it will retrieve the inputs from the knowledge graph, record the timestamp of this operation, and switch the status of derivation to `InProgress`:
```
<Derivation> <:retrievedInputsAt> 123
<Derivation> <:hasStatus> <status>
<status> <rdf:type> <:InProgress>
```
The retrieved JSON object within the `uk.ac.cam.cares.jps.base.derivation.DerivationClient.AGENT_INPUT_KEY` key will then be serialised to an instance of DerivationInputs and passed to method `uk.ac.cam.cares.jps.base.agent.DerivationAgent.processRequestParameters(DerivationInputs, DerivationOutputs)`. The IRI of the new instances within the returned DerivationOutputs should be connected with the status of the derivation using objective property `hasNewDerivedIRI` upon job completion. The `Status` of the derivation should also be updated to `Finished`. There is no limit on the number of new instances.
```
<status> <rdf:type> <:Finished>
<status> <:hasNewDerivedIRI> <newEntity1>
<status> <:hasNewDerivedIRI> <newEntity2>
```
In the next periodical monitoring of the derivation status, if it is `Finished`, the agent is responsibe for deleting the old instances, reconnecting the new instances with the original derivation, cleaning up all the status, and finally updating the timestamp of the derivation. All these processing steps at the `Finished` status are taken care of by method `uk.ac.cam.cares.jps.base.derivation.DerivationClient.cleanUpFinishedDerivationUpdate(String)`.

# Derivation that is an input to another derivation
Note that this only applies to derivations without a time series. Care has to be taken if a derivation depends on an input that is part of another derivation, e.g.
```
<Derivation1> <:isDerivedFrom> <entity1>
<entity1> <:belongsTo> <Derivation2>
<Derivation2> <:isDerivedUsing> <Agent2>
```
For synchronous response case, if `Derivation2` is out-of-date and the DerivationClient calls `Agent2`, `Agent2` must create at least one instance with the same rdf:type as `entity1`. When the DerivationClient deletes `entity1`, it will search for the new instances created by `Agent2` with the same rdf:type and link it to `Derivation1`.

For asynchronous operation case, the DerivationClient only marks `Derivation2` as update `Requested` if it is out-of-date, but it does NOT send any request. It is `Agent2` who is responsible for monitoring the `Status` of `Derivation2` periodically and decides when to carry out the update. Similarly, reconnecting the new derived IRI, cleaning up the status, and updating the derivation timestamp are all deemed to be handled by `Agent2` by calling methods from DerivationClient.

# Validation 
Once the derivation instances are initialised using `createDerivation` and `createDerivationWithTimeSeries`, you should use the provided method `uk.ac.cam.cares.jps.base.derivation.DerivationClient.validateDerivation(String)` to check that the connection is valid. This method goes through all the inputs of the provided derivation, and all the subsequent inputs if the inputs are derivations. This makes sure that there are no circular dependencies, and each instance has a valid timestamp.

# Update
## Pure synchronous response
**Deprecated** The update method, `uk.ac.cam.cares.jps.base.derivation.DerivationClient.updateDerivation(String)`, is the core functionality provided by the DerivationClient. The argument should be the IRI of the derivation instance that you want to update, this will ensure all the dependencies of the derivation instance are updated before updating the given derivation.

**Recommended** Following the introduction of DerivationAgent/DerivationInputs/DerivationOutputs, it is now recommended to use the method `uk.ac.cam.cares.jps.base.derivation.DerivationClient.updatePureSyncDerivation(String)` to request update of the derivation instance if the derivation you want to update is a synchronous derivation, i.e. instance of `Derivation` or `DerivationWithTimeSeries`. Two more functions are provided to update a list of sync derivations `uk.ac.cam.cares.jps.base.derivation.DerivationClient.updatePureSyncDerivation(List<String>)`, and all sync derivations within a triple store `uk.ac.cam.cares.jps.base.derivation.DerivationClient.updateAllSyncDerivations()`.

## Asynchronous operation
**Deprecated** The update request method, `uk.ac.cam.cares.jps.base.derivation.DerivationClient.updateDerivationAsyn(String)`, is the core functionality provided by the DerivationClient working in asynchronous mode. The argument should be the IRI of the derivation instance that you want to update. This method will mark the derivation and all its dependencies as update `Requested` if it is determined as outdated. It should be noted that when working with asynchronous operation mode, the agent is expected to monitor the derivation that `isDerivedUsing` itself periodically and set up job for any update `Requested`. The desired agent behaviour should be set by the developer and you may refer to below section for a demo.

**Recommended** Following the introduction of DerivationAgent/DerivationInputs/DerivationOutputs, it is now recommended to use the method `uk.ac.cam.cares.jps.base.derivation.DerivationClient.updateMixedAsyncDerivation(String)` to request update of the derivation instance if the derivation you want to update is an asynchronous derivation, i.e. instance of `DerivationAsyn`.

## Mixed type - async derivations depend on sync derivations
**New** Following the introduction of DerivationAgent/DerivationInputs/DerivationOutputs, a new function `uk.ac.cam.cares.jps.base.derivation.DerivationClient.unifiedUpdateDerivation(String)` is provided to support updating a directed acyclic graph (DAG) that consists of async derivations depending on sync derivations. In this mode, it will mark the derivation and all its dependencies as update `Requested` if it is determined as outdated (including sync derivations). The agent is expected to monitor the derivation that `isDerivedUsing` itself periodically and check if any `Requested` asynchronous derivations. For those `Requested`, the agent checks the status of its upstream derivations and will wait if any of its immediate upstream asynchronous derivations are still outdated - the agent only acts when it determined all its immediate upstream asynchronous derivations are up-to-date. It will first request update of all its upstream sync derivations (if any), and then set up job for `Requested`. For more details, you may refer to below for a demo.

# Example
An example demonstrating the functionality of the derivation framework is provided in `TheWorldAvatar/Agents/DerivationExample`.

The asynchronous operation functionality and mixed type DAG support of the derivation framework are demonstrated in `TheWorldAvatar/Agents/DerivationAsynExample` for both new information creation (`createAsyncDerivationForNewInfo`/`createSyncDerivationForNewInfo`) and existing information update (`unifiedUpdateDerivation`) modes, please refer to its `README.md` for more details.
