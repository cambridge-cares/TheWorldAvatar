# import os
# import time
# import pytest
# from pathlib import Path
# from configobj import ConfigObj
# from testcontainers.core.container import DockerContainer
#
# # get the jpsBaseLibGateWay instance from the jpsSingletons module
# import ukgasflows.jpsSingletons as jps
# import ukgasflows.output_flow_data as output
#
#
# @pytest.fixture()
# def get_sample_gasflow_history():
#     # Create sample test data as returned by get_gasflow_history() from triple store and postgres database
#     # FORMAT: {terminal name: [terminal IRI, measurement IRI, Java time series object], ...}
#
#     # Create a JVM module view and use it to import the required java classes
#     jpsBaseLib_view = jps.jpsBaseLibGW.createModuleView()
#     jps.jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.timeseries.*")
#
#     ts_data = {}
#     for i in range(1, 3):
#         # Create sample data of length n
#         n = 2
#         terminal_name = 'test_terminal'
#         terminal_iri = 'test_terminal_iri'
#         data_iri = 'test_data_iri'
#         values = [[float(j) for j in (range(n))]]
#         t = jpsBaseLib_view.java.time.Instant.now()
#         times = []
#         for k in range(n):
#             times.append(t.plusSeconds(k))
#
#         # Construct time series object from sample data
#         ts = jpsBaseLib_view.TimeSeries(times, [data_iri], values)
#         # Add to dictionary
#         ts_data[terminal_name + str(i) ] = [terminal_iri + str(i), data_iri + str(i), ts]
#
#     return ts_data
#
#
# def test_onSuccess(get_sample_gasflow_history):
#
#     converted_data = output.onSuccess(get_sample_gasflow_history)
#
#
#
