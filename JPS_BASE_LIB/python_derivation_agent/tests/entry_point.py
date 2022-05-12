from agents_for_test import RNGAgent, MaxValueAgent, MinValueAgent, DifferenceAgent

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
DERIVATION_PERIODIC_TIMESCALE = 3
DERIVATION_INSTANCE_BASE_URL = 'http://www.asyncagent.com/triplestore/repository/'
SPARQL_QUERY_ENDPOINT = "http://kg.cmclinnovations.com:81/blazegraph/namespace/testontorxn/sparql"


def create_app():
    rng_agent = RNGAgent(
        agent_iri=RNGAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=SPARQL_QUERY_ENDPOINT
    )
    rng_agent.add_url_pattern('/rng_agent', 'rng_root',
                              rng_default, methods=['GET'])

    rng_agent.start_monitoring_derivations()

    max_agent = MaxValueAgent(
        agent_iri=MAXAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=SPARQL_QUERY_ENDPOINT, app=Flask(__name__)
    )
    max_agent.add_url_pattern('/max_agent', 'max_root',
                              max_default, methods=['GET'])

    max_agent.start_monitoring_derivations()

    min_agent = MinValueAgent(
        agent_iri=MINAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=SPARQL_QUERY_ENDPOINT, app=Flask(__name__)
    )
    min_agent.add_url_pattern('/min_agent', 'min_root',
                              min_default, methods=['GET'])

    min_agent.start_monitoring_derivations()

    diff_agent = DifferenceAgent(
        agent_iri=DIFFAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=SPARQL_QUERY_ENDPOINT, app=Flask(__name__)
    )
    diff_agent.add_url_pattern(
        '/diff_agent', 'diff_root', diff_default, methods=['GET'])

    diff_agent.start_monitoring_derivations()

    return rng_agent.app, max_agent.app, min_agent.app, diff_agent.app


if __name__ == '__main__':
    rng_app, max_app, min_app, diff_app = create_app()
    arg = sys.argv[1]
    if arg == 'rng':
        rng_app.run(port=5001)
    elif arg == 'max':
        max_app.run(port=5002)
    elif arg == 'min':
        min_app.run(port=5003)
    elif arg == 'diff':
        diff_app.run(port=5004)
