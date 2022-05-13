from tests.agents import RNGAgent, MaxValueAgent, MinValueAgent, DifferenceAgent

from flask import Flask
import logging
import sys

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)


def rng_default():
    return "RNG Agent"


def max_default():
    return "Max Agent"


def min_default():
    return "Min Agent"


def diff_default():
    return "Diff Agent"


RNGAGENT_SERVICE = 'http://www.asyncagent.com/resource/agents/Service__Random#Service'
MAXAGENT_SERVICE = 'http://www.asyncagent.com/resource/agents/Service__Max#Service'
MINAGENT_SERVICE = 'http://www.asyncagent.com/resource/agents/Service__Min#Service'
DIFFAGENT_SERVICE = 'http://www.asyncagent.com/resource/agents/Service__Diff#Service'
RNGAGENT_ENDPOINT = '/Random'
MAXAGENT_ENDPOINT = '/Max'
MINAGENT_ENDPOINT = '/Min'
DIFFAGENT_ENDPOINT = '/Diff'
DERIVATION_PERIODIC_TIMESCALE = 3
DERIVATION_INSTANCE_BASE_URL = 'http://www.asyncagent.com/triplestore/repository/'
SPARQL_QUERY_ENDPOINT = "http://kg.cmclinnovations.com:81/blazegraph/namespace/testontorxn/sparql"


def create_rng_app():
    rng_agent = RNGAgent(
        agent_iri=RNGAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=SPARQL_QUERY_ENDPOINT, agent_endpoint=RNGAGENT_ENDPOINT, app=Flask(
            __name__)
    )
    rng_agent.add_url_pattern('/rng_agent', 'rng_root',
                              rng_default, methods=['GET'])
    rng_agent.start_monitoring_derivations()

    return rng_agent.app


def create_max_app():
    max_agent = MaxValueAgent(
        agent_iri=MAXAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=SPARQL_QUERY_ENDPOINT, agent_endpoint=MAXAGENT_ENDPOINT, app=Flask(
            __name__)
    )
    max_agent.add_url_pattern('/max_agent', 'max_root',
                              max_default, methods=['GET'])
    max_agent.start_monitoring_derivations()

    return max_agent.app


def create_min_app():
    min_agent = MinValueAgent(
        agent_iri=MINAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=SPARQL_QUERY_ENDPOINT, agent_endpoint=MINAGENT_ENDPOINT, app=Flask(
            __name__)
    )
    min_agent.add_url_pattern('/min_agent', 'min_root',
                              min_default, methods=['GET'])
    min_agent.start_monitoring_derivations()

    return min_agent.app


def create_diff_app():
    diff_agent = DifferenceAgent(
        agent_iri=DIFFAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=SPARQL_QUERY_ENDPOINT, agent_endpoint=DIFFAGENT_ENDPOINT, app=Flask(
            __name__)
    )
    diff_agent.add_url_pattern(
        '/diff_agent', 'diff_root', diff_default, methods=['GET'])
    diff_agent.start_monitoring_derivations()

    return diff_agent.app


# def create_app():
#     # rng_agent = RNGAgent(
#     #     agent_iri=RNGAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
#     #     derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
#     #     kg_url=SPARQL_QUERY_ENDPOINT
#     # )
#     # rng_agent.add_url_pattern('/rng_agent', 'rng_root',
#     #                           rng_default, methods=['GET'])

#     # rng_agent.start_monitoring_derivations()

#     rng_agent, max_agent, min_agent, diff_agent = create_agent()
#     rng_agent.start_monitoring_derivations()
#     max_agent.start_monitoring_derivations()
#     min_agent.start_monitoring_derivations()
#     diff_agent.start_monitoring_derivations()

#     return rng_agent.app

# def create_app():
#     app = Flask(__name__)

#     return app


# if __name__ == '__main__':
#     rng_agent, max_agent, min_agent, diff_agent = create_agent()
#     arg = sys.argv[1]
#     if arg == 'rng':
#         rng_agent.start_monitoring_derivations()
#         rng_agent.app.run(port=5001)
#     elif arg == 'max':
#         max_agent.start_monitoring_derivations()
#         max_agent.app.run(port=5002)
#     elif arg == 'min':
#         min_agent.start_monitoring_derivations()
#         min_agent.app.run(port=5003)
#     elif arg == 'diff':
#         diff_agent.start_monitoring_derivations()
#         diff_agent.app.run(port=5004)
