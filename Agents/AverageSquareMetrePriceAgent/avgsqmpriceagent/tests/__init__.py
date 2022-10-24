# #pcs = ['PE30 5DH', 'PE30 4XH', 'PE30 3NS', 'PE31 6XU', 'PE30 4GG', 'PE34 3LS']
# pc='PE30 5DH'

# kg_client = agent.sparql_client
# deriv_client = agent.jpsBaseLib_view.DerivationClient(agent.storeClient, 'https://www.theworldavatar.com/kg/derivation/')

# postcode = kg_client.get_postcode_iris([pc])[0]
# deriv_client.addTimeInstance(postcode)
# deriv_client.updateTimestamp(postcode)

# tx_iris = kg_client.get_tx_iris_for_postcodes([postcode])
# for tx in tx_iris:
# 	deriv_client.addTimeInstance(tx)
# 	deriv_client.updateTimestamp(tx)

# ppi = kg_client.get_ppi_iri(postcode)
# deriv_client.addTimeInstance(ppi)
# deriv_client.updateTimestamp(ppi)

# deriv_inputs = tx_iris + [postcode] + [ppi]

# deriv_iri = deriv_client.createAsyncDerivationForNewInfo(agent.agentIRI, deriv_inputs)



# 2022-10-24 06:39:24,672 (STDOUT) Next wakeup is due at 2022-10-24 06:39:34.665819+00:00 (in 9.997057 seconds)
# 2022-10-24 06:39:31,451 (STDOUT) Job "monitor_derivations (trigger: interval[0:00:10], next run at: 2022-10-24 06:39:34 UTC)" raised an exception
# Traceback (most recent call last):
#   File "/usr/local/lib/python3.9/site-packages/apscheduler/executors/base.py", line 125, in run_job
#     retval = job.func(*job.args, **job.kwargs)
#   File "/usr/local/lib/python3.9/site-packages/pyderivationagent/agent/derivation_agent.py", line 179, in inner
#     raise e
#   File "/usr/local/lib/python3.9/site-packages/pyderivationagent/agent/derivation_agent.py", line 164, in inner
#     func(self, *args, **kwargs)
#   File "/usr/local/lib/python3.9/site-packages/pyderivationagent/agent/derivation_agent.py", line 376, in monitor_async_derivations
#     self.derivationClient.cleanUpFinishedDerivationUpdate(derivation)
#   File "/usr/local/lib/python3.9/site-packages/py4j/java_gateway.py", line 1304, in __call__
#     return_value = get_return_value(
#   File "/usr/local/lib/python3.9/site-packages/py4j/protocol.py", line 326, in get_return_value
#     raise Py4JJavaError(
# py4j.protocol.Py4JJavaError: An error occurred while calling o7.cleanUpFinishedDerivationUpdate.
# : java.util.NoSuchElementException: No value present
#         at java.base/java.util.Optional.get(Optional.java:148)
#         at uk.ac.cam.cares.jps.base.derivation.DerivationSparql.getDerivationWithImmediateDownstream(DerivationSparql.java:2430)
#         at uk.ac.cam.cares.jps.base.derivation.DerivationClient.cleanUpFinishedDerivationUpdate(DerivationClient.java:841)
#         at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
#         at java.base/jdk.internal.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
#         at java.base/jdk.internal.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
#         at java.base/java.lang.reflect.Method.invoke(Method.java:566)
#         at py4j.reflection.MethodInvoker.invoke(MethodInvoker.java:244)
#         at py4j.reflection.ReflectionEngine.invoke(ReflectionEngine.java:357)
#         at py4j.Gateway.invoke(Gateway.java:282)
#         at py4j.commands.AbstractCommand.invokeMethod(AbstractCommand.java:132)
#         at py4j.commands.CallCommand.execute(CallCommand.java:79)
#         at py4j.GatewayConnection.run(GatewayConnection.java:238)
#         at java.base/java.lang.Thread.run(Thread.java:829)
