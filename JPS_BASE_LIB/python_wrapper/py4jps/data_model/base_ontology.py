from pydantic import BaseModel, Field, computed_field
import typing
from typing import Any, Optional, List, Union
import rdflib
from rdflib import Graph, URIRef
from rdflib.namespace import RDF, RDFS

import hashlib
import base64

from py4jps.data_model.utils import construct_rdf_type, init_instance_iri
from py4jps.data_model.iris import TWA_BASE_PREFIX
from py4jps.kg_operations import PySparqlClient


class BaseProperty(BaseModel):
    base_prefix: str = Field(default=TWA_BASE_PREFIX, frozen=True)
    namespace: str = Field(default=None, frozen=True)
    object: Any

    @computed_field
    @property
    def predicate_iri(self) -> str:
        return construct_rdf_type(self.base_prefix, self.namespace, self.__class__.__name__)

    def add_property_to_graph(self, subject: str, g: Graph):
        raise NotImplementedError("This is an abstract method.")


class BaseOntology(BaseModel):
    # two directions:
    # 1. firstly, from instance to triples
    #    - store all object properties as pydantic objects
    #    - store all data properties as pydantic objects
    #    - updating existing instance in kg
    # 2. from kg triples to instance
    #    - pull single instance (node) from kg given iri
    #    - arbitary recursive depth when pull triples from kg?
    # one can do: instance1 = BaseOntology.from_kg(iri) # will return class if exist, otherwise return an instance of BaseOntology with the clz field
    """The initialisation and validator sequence:
        (I) start to run BaseOntology.__init__(__pydantic_self__, **data) with **data as the raw input arguments;
        (II) run until super().__init__(**data), note data is updated within BaseOntology before sending to super().init(**data);
        (III) now within BaseModel __init__:
            (i) run root_validator (for those pre=True), in order of how the root_validators are listed in codes;
            (ii) in order of how the fields are listed in codes:
                (1) run validator (for those pre=True) in order of how the validators (for the same field) are listed in codes;
                (2) run validator (for those pre=False) in order of how the validators (for the same field) are listed in codes;
            (iii) (if we are instantiating a child class of BaseOntology) load default values in the child class (if they are provided)
                  and run root_validator (for those pre=False) in order of how the root_validators are listed in codes,
                  e.g. clz='clz provided in the child class' will be added to 'values' of the input argument of root_validator;
        (IV) end BaseModel __init__;
        (V) end BaseOntology __init__"""
    # NOTE Optional[DataProperty] can be used to indicate cardinality of 0..1
    # NOTE Optional[ObjectProperty] can be used to accmmodate the case where the object property is not instantiated
    base_prefix: str = Field(default=TWA_BASE_PREFIX, frozen=True)
    namespace: str = Field(default=None, frozen=True)
    rdfs_comment: str = Field(default=None)

    @computed_field
    @property
    def rdf_type(self) -> str:
        # see https://docs.pydantic.dev/dev/api/fields/#pydantic.fields.computed_field
        # TODO rdf_type should be a class attribute
        return construct_rdf_type(self.base_prefix, self.namespace, self.__class__.__name__)

    @computed_field
    @property
    def instance_iri(self) -> str:
        # see https://docs.pydantic.dev/dev/api/fields/#pydantic.fields.computed_field
        return init_instance_iri(self.base_prefix, self.__class__.__name__)

    def push_to_kg(self, sparql_client: PySparqlClient):
        """This method is for pushing new instances to the KG, or updating existing instance."""
        g = Graph()
        self.create_triples_for_kg(g)
        sparql_client.upload_graph(g)

    @classmethod
    def pull_from_kg(instance_iri: str, sparql_client: PySparqlClient):
        # TODO provide the basic implementation for pulling instance from KG
        # TODO do we only pull instance and its out-going edges, or also in-coming edges?
        # TODO what do we do with undefined properties in python class?
        pass

    def create_triples_for_kg(self, g):
        """This method is for creating triples as rdflib.Graph() for the knowledge graph.
        By default, it will create triples for the instance itself.
        One can overwrite this method and provide additional triples should they wish.
        In which case, please remember to call the super.create_triples_for_kg(g),
        unless one wants to provide completely different triples."""
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.rdf_type)))
        if self.rdfs_comment:
            g.add((URIRef(self.instance_iri), RDFS.comment, rdflib.Literal(self.rdfs_comment)))
        for f, prop in iter(self):
            if f not in ['base_prefix', 'namespace', 'rdfs_comment', 'instance_iri', 'rdf_type'] and bool(prop):
                if isinstance(prop, BaseProperty):
                    g = prop.add_property_to_graph(self.instance_iri, g)
                else:
                    raise TypeError(f"Type of {prop} is not supported for field {f}.")
        return g

    def _exclude_keys_for_compare_(self, *keys_to_exclude):
        list_keys_to_exclude = list(keys_to_exclude) if not isinstance(keys_to_exclude, list) else keys_to_exclude
        list_keys_to_exclude.append('instance_iri')
        list_keys_to_exclude.append('rdfs_comment')
        list_keys_to_exclude.append('namespace_for_init')
        return set(tuple(list_keys_to_exclude))

    def __eq__(self, other: Any) -> bool:
        return self.__hash__() == other.__hash__()

    def __hash__(self):
        return self._make_hash_sha256_(self.dict(exclude=self._exclude_keys_for_compare_()))

    def _make_hash_sha256_(self, o):
        # adapted from https://stackoverflow.com/a/42151923
        hasher = hashlib.sha256()
        hasher.update(repr(self._make_hashable_(o)).encode())
        return base64.b64encode(hasher.digest()).decode()

    def _make_hashable_(self, o):
        # adapted from https://stackoverflow.com/a/42151923

        if isinstance(o, (tuple, list)):
            return tuple((self._make_hashable_(e) for e in o))

        if isinstance(o, dict):
            # TODO below is a shortcut for the implementation, the specific _exclude_keys_for_compare_ of nested classes are not called
            # but for OntoCAPE_SinglePhase this is sufficient for the comparison (as 'instance_iri' and 'namespace_for_init' are excluded by default)
            # to do it properly, we might need recursion that calls all _exclude_keys_for_compare_ while iterate the nested classes
            for key in self._exclude_keys_for_compare_():
                if key in o:
                    o.pop(key)
            return tuple(sorted((k,self._make_hashable_(v)) for k,v in o.items()))

        if isinstance(o, (set, frozenset)):
            return tuple(sorted(self._make_hashable_(e) for e in o))

        return o


class ObjectProperty(BaseProperty):
    object: Union[List[BaseOntology], List[str], BaseOntology, str]
    # TODO from the test case in test_base_ontology.py, it seems best to provide a custom Type for the object field
    # so that the user can just provide a list of ontology class or a single ontology class

    def add_property_to_graph(self, subject: str, g: Graph):
        # NOTE the object can be a list of BaseOntology or str
        op = self.object if isinstance(self.object, list) else [self.object]
        for o in op:
            if isinstance(o, BaseOntology):
                g.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o.instance_iri)))
                # given that the KG only store triple once, we don't need to check if the triples already exist for object properties
                g = o.create_triples_for_kg(g)
            elif isinstance(o, str):
                g.add((URIRef(subject), URIRef(self.predicate_iri), URIRef(o)))
            else:
                raise TypeError(f"Type of {o} is not supported.")
        return g


class DataProperty(BaseProperty):

    def add_property_to_graph(self, subject: str, g: Graph):
        # TODO implement behaviour to overwrite the existing data property when pushing to KG
        if self.object:
            g.add((URIRef(subject), URIRef(self.predicate_iri), rdflib.Literal(self.object)))
        return g
