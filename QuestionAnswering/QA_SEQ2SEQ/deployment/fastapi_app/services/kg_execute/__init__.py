import logging
from typing import Dict

from .kg_client import IKgClient


class UnexpectedDomainError(ValueError):
    def __init__(self, domain: str) -> None:
        super().__init__("Unexpected domain: " + domain)


logger = logging.getLogger(__name__)


class KgExecutor:
    def __init__(self, domain2sparql: Dict[str, IKgClient]):
        self.domain2sparql = domain2sparql

    def query(self, domain: str, query: str):
        """Executes SPARQL query against a domain-specific KG.

        Raises:
            UnexpectedDomainError
        """
        if domain not in self.domain2sparql:
            raise UnexpectedDomainError(domain)
        return self.domain2sparql[domain].query(query)
