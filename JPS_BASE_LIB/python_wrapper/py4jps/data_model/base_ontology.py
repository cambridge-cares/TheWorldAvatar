from __future__ import annotations

from pydantic import BaseModel, Field, model_validator
from typing import Any, List, Union, TypeVar
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import RDF, RDFS

import hashlib
import base64

from py4jps.data_model.utils import construct_rdf_type, init_instance_iri
from py4jps.data_model.iris import TWA_BASE_PREFIX
from py4jps.kg_operations import PySparqlClient


T = TypeVar('T')

def as_range_of_object_property(t: T) -> List[Union[T, str]]:
    # TODO add validator to verify the length of the list as cardinality, also None as default value
    # TODO handle single instance as well
    return List[Union[t, str]]

def as_range_of_data_property(t: T) -> List[T]:
    # TODO add validator to verify the length of the list as cardinality, also None as default value
    # TODO handle single instance as well
    return List[t]

class BaseProperty(BaseModel):
    base_prefix: str = Field(default=TWA_BASE_PREFIX, frozen=True)
    namespace: str = Field(default=None, frozen=True)
    predicate_iri: str = Field(default=None)
    range: Any

    @model_validator(mode='after')
    def set_predicate_iri(self):
        if not bool(self.predicate_iri):
            self.predicate_iri = self.__class__.get_predicate_iri()
        return self

    @classmethod
    def get_predicate_iri(cls) -> str:
        return construct_rdf_type(cls.model_fields['base_prefix'].default, cls.model_fields['namespace'].default, cls.__name__)

    def add_property_to_graph(self, subject: str, g: Graph):
        raise NotImplementedError("This is an abstract method.")


class BaseOntology(BaseModel):
    # TODO think about how to make it easier to construct an instance easily by hand (or should it be fully automated?)
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
        (V) end BaseOntology __init__

    Example:
    class MyClass(BaseOntology):
        myObjectProperty: MyObjectProperty
        myDataProperty: MyDataProperty
    """
    # NOTE Optional[DataProperty] can be used to indicate cardinality of 0..1
    # NOTE Optional[ObjectProperty] can be used to accmmodate the case where the object property is not instantiated
    base_prefix: str = Field(default=TWA_BASE_PREFIX, frozen=True)
    namespace: str = Field(default=None, frozen=True)
    rdfs_comment: str = Field(default=None)
    rdf_type: str = Field(default=None)
    instance_iri: str = Field(default=None)

    @model_validator(mode='after')
    def set_rdf_type(self):
        if not bool(self.rdf_type):
            self.rdf_type = self.__class__.get_rdf_type()
        if not bool(self.instance_iri):
            self.instance_iri = init_instance_iri(self.base_prefix, self.__class__.__name__)
        return self

    @classmethod
    def get_rdf_type(cls) -> str:
        return construct_rdf_type(cls.model_fields['base_prefix'].default, cls.model_fields['namespace'].default, cls.__name__)

    def push_to_kg(self, sparql_client: PySparqlClient):
        """This method is for pushing new instances to the KG, or updating existing instance."""
        g = Graph()
        self.create_triples_for_kg(g)
        sparql_client.upload_graph(g)

    @classmethod
    def pull_from_kg(cls, iris: List[str], sparql_client: PySparqlClient, depth: int = 0) -> List[BaseOntology]:
        # TODO provide the basic implementation for pulling instance from KG
        # TODO only pull instance and its out-going edges
        # TODO what do we do with undefined properties in python class? - write a warning message or we can add them to extra_fields https://docs.pydantic.dev/latest/concepts/models/#extra-fields
        if isinstance(iris, str):
            iris = [iris]
        node_dct = sparql_client.get_outgoing_and_attributes(iris) # TODO return format: {iri: {predicate: [object]}}
        instance_lst = []
        # TODO handle object properties (where the recursion happens)

        # Here we handle data properties
        dps = cls.get_data_properties()
        for iri, props in node_dct.items():
            instance_lst.append(
                cls(
                    instance_iri=iri,
                    **{
                        dp_dct['field']: dp_dct['type'](range=props[dp_iri]) if dp_iri in props else None for dp_iri, dp_dct in dps.items()
                    }
                )
            )
        print("/////////////////////////////////////////////////////////////")
        print(instance_lst)
        print("/////////////////////////////////////////////////////////////")
        # for f in C.model_fields:
        #     if issubclass(C.model_fields[f].annotation, DataProperty):
        #         lst.append(C.model_fields[f].annotation(range=d[iri][C.model_fields[f].annotation.get_rdf_type()][0]))

        #     if issubclass(C.model_fields[f].annotation, ObjectProperty):
        #         print(d[iri][C.model_fields[f].annotation.get_rdf_type()])
        #         lst.append(C.model_fields[f].annotation(range=d[iri][C.model_fields[f].annotation.get_rdf_type()][0]))

        # for iri in iris:
        #     field_dct = {}
        #     for f in cls.model_fields:
        #         if issubclass(cls.model_fields[f].annotation, DataProperty):

        #     # for cls.model_fields[k] in cls.model_fields.values():
        #         if isinstance(cls.model_fields[k], ObjectProperty):
        #             if k in node_dct[iri]:
        #                 node_dct[iri][k] = [cls.pull_from_kg([iri], sparql_client, depth-1) for iri in node_dct[iri][k]]
        #     # TODO create a new instance
        #     lst.append[cls(**field_dct)]
        #     # for p, o in node_dct[iri].items():
        #     #     if isinstance(v, list):
        #     #         node_dct[iri][k] = [cls.pull_from_kg([iri], sparql_client, depth-1) for iri in v]
        # # TODO how to create a new instance of the class with the returned triples?
        # TODO add check for rdf_type
        return instance_lst

    @classmethod
    def get_object_properties(cls):
        return {
            field_info.annotation.get_predicate_iri():{
                'field': f, 'type': field_info.annotation
            } for f, field_info in cls.model_fields.items() if issubclass(field_info.annotation, ObjectProperty)
        }

    @classmethod
    def get_data_properties(cls):
        return {
            field_info.annotation.get_predicate_iri():{
                'field': f, 'type': field_info.annotation
            } for f, field_info in cls.model_fields.items() if issubclass(field_info.annotation, DataProperty)
        }

    def create_triples_for_kg(self, g):
        """This method is for creating triples as rdflib.Graph() for the knowledge graph.
        By default, it will create triples for the instance itself.
        One can overwrite this method and provide additional triples should they wish.
        In which case, please remember to call the super.create_triples_for_kg(g),
        unless one wants to provide completely different triples."""
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.rdf_type)))
        if self.rdfs_comment:
            g.add((URIRef(self.instance_iri), RDFS.comment, Literal(self.rdfs_comment)))
        for f, prop in iter(self):
            if f not in ['base_prefix', 'namespace', 'rdfs_comment', 'instance_iri', 'rdf_type'] and bool(prop):
                if isinstance(prop, BaseProperty) and bool(prop.range):
                    g = prop.add_property_to_graph(self.instance_iri, g)
                else:
                    raise TypeError(f"Type of {prop} is not supported for field {f} when creating KG triples for instance {self.dict()}.")
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
    range: as_range_of_object_property(BaseOntology)
    # TODO modify __init__ so that both instance of BaseOntology and str can be parsed as range

    def add_property_to_graph(self, subject: str, g: Graph):
        # NOTE the range can be a list of BaseOntology or str
        # The code will only enter the below if range is not None
        op = self.range if isinstance(self.range, list) else [self.range]
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
    # TODO whether str or specific type for the range, we can check during __init__
    def __init__(self, **data) -> None:
        if 'range' in data and not isinstance(data['range'], list):
            data['range'] = [data['range']]
        super().__init__(**data)

    def add_property_to_graph(self, subject: str, g: Graph):
        # TODO implement behaviour to overwrite the existing data property when pushing to KG
        dp = self.range if isinstance(self.range, list) else [self.range]
        for d in dp:
            try:
                g.add((URIRef(subject), URIRef(self.predicate_iri), Literal(d)))
            except Exception as e:
                raise TypeError(f"Type of {d} ({type(d)}) is not supported by rdflib as a data property for {self.predicate_iri}.", e)
        return g
