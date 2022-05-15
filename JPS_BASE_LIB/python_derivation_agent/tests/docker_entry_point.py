import tests.conftest as cft

def create_rng_app():
    rng_agent = cft.create_rng_agent(cft.BLAZEGRAPH_ENDPOINT_WITHIN_DOCKER)

    rng_agent.start_monitoring_derivations()

    return rng_agent.app


def create_max_app():
    max_agent = cft.create_max_agent(cft.BLAZEGRAPH_ENDPOINT_WITHIN_DOCKER)

    max_agent.start_monitoring_derivations()

    return max_agent.app


def create_min_app():
    min_agent = cft.create_min_agent(cft.BLAZEGRAPH_ENDPOINT_WITHIN_DOCKER)

    min_agent.start_monitoring_derivations()

    return min_agent.app


def create_diff_app():
    diff_agent = cft.create_diff_agent(cft.BLAZEGRAPH_ENDPOINT_WITHIN_DOCKER)

    diff_agent.start_monitoring_derivations()

    return diff_agent.app


def create_update_endpoint():
    update_agent = cft.create_update_endpoint(cft.BLAZEGRAPH_ENDPOINT_WITHIN_DOCKER)

    update_agent.add_url_pattern(
        '/update', 'update_derivation',
        update_agent.update_derivations, methods=['GET']
    )

    return update_agent.app
