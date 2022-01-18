# Namespace
The namespaces used in this document:
```
: https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#
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
<status> <rdf:type> <Status>
```
available status (subclasses of `Status`) are: `Requested`, `InProgress`, and `Finished`. A specific object property of `Finished` is `hasNewDerivedIRI` that is used to temporarily store any generated derived IRI during the derivation update, i.e.
```
<status> <rdf:type> <Finished>
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

2. Derivation with time series, initialised using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.createDerivationWithTimeSeries(List<String>, String, String, List<String>)`
  - e.g. createDerivationWithTimeSeries(entities, agentIRI, agentURL, inputs)
  - It is assumed that the agents acting on these derivations add row(s) to some table(s), the entities under these derivations are not deleted and replaced.

For asynchronous agent operations, the derivation framework currently supports only:
1. Derivation without time series, initialised using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.createDerivation(List<String>, String, List<String>)`
- e.g. createDerivation(entities, agentIRI, inputs)
- The entities wrapped around the derivation instance are deleted and replaced each time the agent creates new instances.
- It should be noted that this initialisation is different from the one that is used for synchronous agent responses, as here it assumes below triples about agent are known and exist in the knowledge graph already:
  ```
  <Agent> <msm:hasOperation> <Operation>
  <Operation> <msm:hasHttpUrl> <URL>
  ```

# Timestamps
The DerivationClient compares the timestamp of the derivation instance with its input(s) to determine whether it's out-of-date. Each derivation instance is initialised with a timestamp automatically, timestamps for inputs have to be added manually using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.addTimeInstance(String)`.

# Agents (synchronous response)
It is assumed that all agents extend the `uk.ac.cam.cares.jps.base.agent.JPSAgent` class, where the input and output are both a JSON object.
## Derivation without time series
The HTTP request made by the DerivationClient will be a JSON object with the `uk.ac.cam.cares.jps.base.derivation.DerivationClient.AGENT_INPUT_KEY` key. Within this JSON object is a JSON array consisting a list of IRIs marked using `isDerivedFrom` for the derivation being updated.
```json
{"agent_input": [iri_in1, iri_in2, iri_in3, ...]}
```
Upon receiving these inputs, the agent is expected to write new instances in the knowledge graph, the IRIs of the new instances should be included in the HTTP response with the `uk.ac.cam.cares.jps.base.derivation.DerivationClient.AGENT_OUTPUT_KEY` key. There is no limit on the number of new instances.
```json
{"agent_output": [iri_out1, iri_out2, iri_out3, ...]}
```

## Derivation with time series
The key difference for derivations with time series is that the DerivationClient does not delete and replace the instances, it only updates the timestamp upon calling the agent. Agents for these derivations should receive HTTP requests in the same manner, perform the necessary calculations, and record the results in a time series table. Unlike derivations without time series, the DerivationClient does not read the HTTP response from these agents.

If there are no thrown exceptions, the DerivationClient will assume that the update is successful and update the timestamp of the derivation instance.

# Agents (asynchronous operation)
Agents working in asynchronous operation are not restricted to extend the `uk.ac.cam.cares.jps.base.agent.JPSAgent` class. But the input/output of an agent is still expected to be a JSON string. 
## Derivation without time series
By design, the derivation framework only marks the derivation as `Requested` if it is out-of-date. It is agents' responsibility to use the methods provided by the DerivationClient to monitor the `Status` of derivations and conduct any job requested. The complete job input should be a JSON string with `uk.ac.cam.cares.jps.base.derivation.DerivationClient.AGENT_INPUT_KEY` key. Within this JSON string are JSON objects of IRIs marked using `isDerivedFrom` for the derivation being updated. In case of multiple IRIs instantiated from the same rdf:type, they should be grouped together based on the I/O signiture declared in its OntoAgent instance.
```json
{"agent_input": 
  {
		"input_rdf_type1": iri1,
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
Following this practice, the input IRI instances of agent are mapped against their rdf:type, enabling standardised agent implementation utilising OntoAgent. 

Upon receiving these inputs, the agent is expected to write new instances in the knowledge graph, the IRIs of the new instances should be connected with the status of the derivation using objective property `hasNewDerivedIRI` upon job completion. The `Statue` of the derivation should also be updated to `Finished`. There is no limit on the number of new instances.
```
<status> <rdf:type> <Finished>
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
## Synchronous response
The update method, `uk.ac.cam.cares.jps.base.derivation.DerivationClient.updateDerivation(String)`, is the core functionality provided by the DerivationClient. The argument should be the IRI of the derivation instance that you want to update, this will ensure all the dependencies of the derivation instance are updated before updating the given derivation.

## Asynchronous operation
The update request method, `uk.ac.cam.cares.jps.base.derivation.DerivationClient.updateDerivationAsyn(String)`, is the core functionality provided by the DerivationClient working in asynchronous mode. The argument should be the IRI of the derivation instance that you want to update. This method will mark the derivation and all its dependencies as update `Requested` if it is determined as outdated. It should be noted that when working with asynchronous operation mode, the agent is expected to monitor the derivation that `isDerivedUsing` itself periodically and set up job for any update `Requested`. The desired agent behaviour should be set by the developer and you may refer to below section for a demo. 

# Example
An example demonstrating the functionality of the DerivationClient is provided in `TheWorldAvatar/Agents/DerivationExample`.

The asynchronous operation functionality of the derivationClient is demonstrated in `TheWorldAvatar/Agents/DoEAgent`, please refer to its `README.md` for more details.