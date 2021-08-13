# Namespace
The namespaces used in this document:
```
: https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#
msm : http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#
time : http://www.w3.org/2006/time#
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

There are two types of derivations
1. Derivation without time series, initialised using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.createDerivation(List<String>, String, String, List<String>)`
  - e.g. createDerivation(entities, agentIRI, agentURL, inputs)
  - The entities wrapped around the derivation instance are deleted and replaced each time the agent creates new instances.

2. Derivation with time series, initialised using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.createDerivationWithTimeSeries(List<String>, String, String, List<String>)`
  - e.g. createDerivationWithTimeSeries(entities, agentIRI, agentURL, inputs)
  - It is assumed that the agents acting on these derivations add row(s) to the table(s), the entities under these derivations are not deleted and replaced.

# Timestamps
The DerivationClient compares the timestamp of the derivation instance with its input(s) to determine whether it's out-of-date. Each derivation instance is initialised with a timestamp automatically, timestamps for inputs have to be added manually using `uk.ac.cam.cares.jps.base.derivation.DerivationClient.addTimeInstance(String)`.

# Agents
It is assumed that all agents extend `the uk.ac.cam.cares.jps.base.agent.JPSAgent` class, where the input and output are both a JSON object.
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
The key difference for derivations with time series is that the DerivationClient does not delete and replace the instances, it only updates the timestamp upon calling the agent. Agents for these derivations should receive HTTP requests in the same manner and update the time series. Unlike derivations without time series, the DerivationClient does not read the HTTP response from these agents. 

If there are no thrown exceptions, the DerivationClient will assume that the update is successful and update the timestamp of the derivation instance.

# Derivation that is an input to another derivation
Note that this only applies to derivations without a time series. Care has to be taken if a derivation depends on an input that is part of another derivation, e.g.
```
<Derivation1> <:isDerivedFrom> <entity1>
<entity1> <:belongsTo> <Derivation2>
<Derivation2> <:isDerivedUsing> <Agent2>
```
If `Derivation2` is out-of-date and the DerivationClient calls `Agent2`, `Agent2` must create at least one instance with the same rdf:type as `entity1`. When the DerivationClient deletes `entity1`, it will search for the new instances created by `Agent2` with the same rdf:type and link it to `Derivation1`.

# Validation 
Once the derivation instances are initialised using `createDerivation` and `createDerivationWithTimeSeries`, you should use the provided method `uk.ac.cam.cares.jps.base.derivation.DerivationClient.validateDerivation(String)` to check that the connection is valid. This method goes through all the inputs of the provided derivation, and all the subsequent inputs if the inputs are derivations. This makes sure that there are no circular dependencies, and each instance has a valid timestamp.

# Update
The update method, `uk.ac.cam.cares.jps.base.derivation.DerivationClient.updateDerivation(String)`, is the core functionality provided by the DerivationClient. The argument should be the IRI of the derivation instance that you want to update, this will ensure all the dependencies of the derivation instance are updated before updating the given derivation.

# Example
An example demonstrating the functionality of the DerivationClient is provided in `TheWorldAvatar/DerivationExample`.