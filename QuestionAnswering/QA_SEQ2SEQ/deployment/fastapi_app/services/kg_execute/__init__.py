import logging
import os


from .kg_client import KgClient


class UnexpectedDomainError(ValueError):
    def __init__(self, domain: str) -> None:
        super().__init__("Unexpected domain: " + domain)


logger = logging.getLogger(__name__)


class KgExecutor:
    def __init__(self):
        ontospecies_endpoint = os.getenv("ONTOSPECIES_ENDPOINT")
        logger.info(
            "Initialize KG client for OntoSpecies with the endpoint " +
            ontospecies_endpoint,
        )
        ontospecies_client = KgClient(ontospecies_endpoint)

        ontokin_endpoint = os.getenv("ONTOKIN_ENDPOINT")
        logger.info("Initialize KG client for OntoKin with the endpoint " + ontokin_endpoint)
        ontokin_client = KgClient(
            ontokin_endpoint,
            user=os.getenv("ONTOKIN_USERNAME"),
            pw=os.getenv("ONTOKIN_PASSWORD"),
        )

        ontocompchem_endpoint = os.getenv("ONTOCOMPCHEM_ENDPOINT")
        logger.info(
            "Initialize KG client for OntoCompChem with the endpoint " +
            ontocompchem_endpoint,
        )
        ontocompchem_client = KgClient(ontocompchem_endpoint)

        kingslynn_endpoint = os.getenv("KINGSLYNN_ENDPOINT")
        logger.info(
            "Initialize KG client for King's Lynn with the endpoint " +
            ontocompchem_endpoint,
        )
        kingslynn_client = KgClient(kingslynn_endpoint)

        self.domain2sparql = dict(
            ontospecies=ontospecies_client,
            ontokin=ontokin_client,
            ontocompchem=ontocompchem_client,
            kingslynn=kingslynn_client
        )


    def get_domains(self):
        return list(self.domain2sparql.keys())

    def query(self, domain: str, query: str):
        """Executes SPARQL query against a domain-specific KG.

        Raises:
            UnexpectedDomainError
        """
        if domain not in self.domain2sparql:
            raise UnexpectedDomainError(domain)
        return self.domain2sparql[domain].query(query)
