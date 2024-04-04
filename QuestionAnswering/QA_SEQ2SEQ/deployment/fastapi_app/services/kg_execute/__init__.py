import logging
from typing import Dict, Iterable

from .kg_client import KgClient


class UnexpectedDomainError(ValueError):
    def __init__(self, domain: str, allowed: Iterable[str]) -> None:
        super().__init__(
            "Unexpected domain: {domain}. Expects one of: {domains}".format(
                domain=domain, domains=", ".join(allowed)
            )
        )


logger = logging.getLogger(__name__)


class KgExecutor:
    def __init__(self, domain2sparql: Dict[str, KgClient]):
        self.domain2sparql = domain2sparql

    def query(self, domain: str, query: str):
        """Executes SPARQL query against a domain-specific KG.

        Raises:
            UnexpectedDomainError
        """
        if domain not in self.domain2sparql:
            raise UnexpectedDomainError(domain, self.domain2sparql.keys())
        return self.domain2sparql[domain].query(query)
