from collections import defaultdict
from functools import cache
import itertools
from typing import Annotated

from fastapi import Depends
from rdflib import SKOS

from constants.namespace import ONTOSPECIES
from model.web.comp_op import COMP_OP_2_SPARQL_SYMBOL
from model.kg.ontospecies import (
    GcAtom,
    OntospeciesChemicalClass,
    OntospeciesDissociationConstant,
    OntospeciesIdentifier,
    OntospeciesProperty,
    OntospeciesSpecies,
    OntospeciesSpeciesBase,
    OntospeciesSpeciesPartial,
    OntospeciesUse,
    PeriodictableElement,
    SpeciesIdentifierKey,
    SpeciesPropertyKey,
)
from model.web.ontospecies import SpeciesReturnFields, SpeciesRequest
from services.rdf_ogm import RDFStore
from services.rdf_stores.base import Cls2NodeGetter
from services.sparql import SparqlClient, get_ontospecies_endpoint, get_ontospecies_endpoint_v3

# TODO: ONTOSPECIES_V3 should be remove after merging
from pydantic.fields import FieldInfo
from types import NoneType, UnionType
from typing import (
    Any,
    TypeVar,
    get_origin,
    get_args,
)
from model.rdf_ogm import RDFEntity
from rdflib import URIRef
T = TypeVar("T", bound=RDFEntity)
def unpack_optional_type(annotation: type[Any]):
    if get_origin(annotation) is UnionType:
        args = get_args(annotation)
        return next(arg for arg in args if arg is not NoneType)
    else:
        return annotation

class OntospeciesRDFStore(Cls2NodeGetter, RDFStore):
    # TODO: ONTOSPECIES_V3 should be remove after merging
    def __init__(self, endpoint, ontospecies_endpoint_v3: str):
        super().__init__(endpoint)
        self.sparql_client_v3 = SparqlClient(ontospecies_endpoint_v3)
    
    @property
    def cls2getter(self):
        return {
            "pt:Element": self.get_elements_many,
            "gc:Atom": self.get_atoms_many,
            "os:Species": self.get_species_base_many,
            "os:Property": self.get_properties_many,
            "os:Identifier": self.get_identifiers_many,
            "os:ChemicalClass": self.get_chemical_classes_many,
            "os:Use": self.get_uses_many,
            "os:DissociationConstant": self.get_dissociation_constant_many,
        }

    def get_elements_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(PeriodictableElement, iris)

    def get_atoms_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(GcAtom, iris)

    def get_species_base_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntospeciesSpeciesBase, iris)

    def get_species_IRIs(self, req: SpeciesRequest):
        chemclass_patterns = [
            f"?Species os:hasChemicalClass/rdfs:subClassOf* <{iri}> ."
            for iri in req.chemical_class
        ]
        use_patterns = [f"?Species os:hasUse <{iri}> ." for iri in req.use]

        identifier_patterns = [
            f'?Species os:has{key}/os:value "{value}" .'
            for key, value in req.identifier.items()
        ]

        property_patterns = [
            pattern
            for key, conds in req.property.items()
            if conds
            for pattern in (
                f"?Species os:has{key}/os:value ?{key}Value .",
                "FILTER ( {} )".format(
                    " && ".join(
                        f"?{key}Value {COMP_OP_2_SPARQL_SYMBOL[op]} {rhs}"
                        for op, rhs in conds
                    )
                ),
            )
        ]

        query = """PREFIX os: <{os}>

SELECT ?Species
WHERE {{
    ?Species a os:Species .
    {clauses}
}}""".format(
            os=ONTOSPECIES,
            clauses="\n    ".join(
                itertools.chain(
                    chemclass_patterns,
                    use_patterns,
                    identifier_patterns,
                    property_patterns,
                )
            ),
        )

        # TODO: ONTOSPECIES_V3 should be remove after merging
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        _, bindings_v3 = self.sparql_client_v3.querySelectThenFlatten(query)
        bindings += bindings_v3
        return [binding["Species"] for binding in bindings]

    def get_species_many(self, iris: list[str] | tuple[str]):
        return [
            (
                OntospeciesSpecies(
                    IRI=species.IRI,
                    label=species.label,
                    IUPACName=species.IUPACName,
                    InChI=species.InChI,
                    altLabel=species.altLabel or list(),
                    ChemicalClass=species.ChemicalClass or list(),
                    Use=species.Use or list(),
                    Identifier=species.Identifier or dict(),
                    Property=species.Property or dict(),
                )
                if species
                else None
            )
            for species in self.get_species_partial_many(
                iris=iris,
                return_fields=SpeciesReturnFields(
                    alt_label=True,
                    chemical_class=True,
                    use=True,
                    identifier=[key for key in SpeciesIdentifierKey],
                    property=[key for key in SpeciesPropertyKey],
                ),
            )
        ]

    def get_species_partial_many(
        self, iris: list[str], return_fields: SpeciesReturnFields
    ):
        species_base = self.get_species_base_many(iris=iris)

        field_pred_pairs: list[tuple[str, str]] = [
            pair
            for pair in [
                (("altLabel", "skos:altLabel") if return_fields.alt_label else None),
                (
                    (
                        "chemicalClass",
                        "os:hasChemicalClass",
                    )  # no traversal of chemical class graphs
                    if return_fields.chemical_class
                    else None
                ),
                (("use", "os:hasUse") if return_fields.use else None),
                *((key.value, f"os:has{key}") for key in (return_fields.identifier)),
                *((key.value, f"os:has{key}") for key in (return_fields.property)),
            ]
            if pair
        ]

        if not field_pred_pairs:
            return [
                (
                    OntospeciesSpeciesPartial(
                        **base_model.model_dump(),
                    )
                    if base_model
                    else None
                )
                for base_model in species_base
            ]

        query = """PREFIX skos: <{skos}>
PREFIX os: <{os}>

SELECT DISTINCT ?Species ?field ?value
WHERE {{
    VALUES ?Species {{ {IRIs} }}
    {patterns}
}}""".format(
            skos=SKOS,
            os=ONTOSPECIES,
            IRIs=" ".join(f"<{iri}>" for iri in iris),
            patterns=" UNION ".join(
                f"""{{
    BIND ( "{field}" as ?field )
    ?Species {pred} ?value .
}}"""
                for field, pred in field_pred_pairs
            ),
        )

        # TODO: ONTOSPECIES_V3 should be remove after merging
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        _, bindings_v3 = self.sparql_client_v3.querySelectThenFlatten(query)
        bindings += bindings_v3
        field2iri2values: defaultdict[str, defaultdict[str, list[str]]] = defaultdict(
            lambda: defaultdict(list)
        )
        for binding in bindings:
            field2iri2values[binding["field"]][binding["Species"]].append(
                binding["value"]
            )

        speciesIRI_to_chemclassIRIs = field2iri2values["chemicalClass"]
        chemical_classes = (
            x
            for x in self.get_chemical_classes_many(
                list(itertools.chain(*speciesIRI_to_chemclassIRIs.values()))
            )
        )
        iri2chemclass = {
            iri: [x for x in [next(chemical_classes) for _ in chemclass_iris] if x]
            for iri, chemclass_iris in speciesIRI_to_chemclassIRIs.items()
        }

        speciesIRI_to_useIRIs = field2iri2values["use"]
        uses = (
            x
            for x in self.get_uses_many(
                list(itertools.chain(*speciesIRI_to_useIRIs.values()))
            )
        )
        iri2use = {
            iri: [x for x in [next(uses) for _ in use_iris] if x]
            for iri, use_iris in speciesIRI_to_useIRIs.items()
        }

        identifier_speciesIRI_to_nodeIRIs = {
            key: field2iri2values[key.value]
            for key in SpeciesIdentifierKey
            if key.value in field2iri2values
        }
        identifiers = (
            x
            for x in self.get_identifiers_many(
                list(
                    itertools.chain(
                        *(
                            itertools.chain(*speciesIRI_to_keyIRIs.values())
                            for speciesIRI_to_keyIRIs in identifier_speciesIRI_to_nodeIRIs.values()
                        )
                    )
                )
            )
        )
        identifier_iri2nodes = {
            key: {
                speciesIRI: [x for x in [next(identifiers) for _ in nodeIRIs] if x]
                for speciesIRI, nodeIRIs in speciesIRI_to_nodeIRIs.items()
            }
            for key, speciesIRI_to_nodeIRIs in identifier_speciesIRI_to_nodeIRIs.items()
        }
        iri2identifiers: defaultdict[
            str, dict[SpeciesIdentifierKey, list[OntospeciesIdentifier]]
        ] = defaultdict(dict)
        for key, iri2nodes in identifier_iri2nodes.items():
            for iri, nodes in iri2nodes.items():
                iri2identifiers[iri][key] = nodes

        property_speciesIRI_to_nodeIRIs = {
            key: field2iri2values[key.value]
            for key in SpeciesPropertyKey
            if key.value in field2iri2values
        }
        properties = (
            x
            for x in self.get_properties_many(
                list(
                    itertools.chain(
                        *(
                            itertools.chain(*speciesIRI_to_keyIRIs.values())
                            for speciesIRI_to_keyIRIs in property_speciesIRI_to_nodeIRIs.values()
                        )
                    )
                )
            )
        )
        property_iri2nodes = {
            key: {
                speciesIRI: [x for x in [next(properties) for _ in nodeIRIs] if x]
                for speciesIRI, nodeIRIs in speciesIRI_to_nodeIRIs.items()
            }
            for key, speciesIRI_to_nodeIRIs in property_speciesIRI_to_nodeIRIs.items()
        }
        iri2properties: defaultdict[
            str, dict[SpeciesPropertyKey, list[OntospeciesProperty]]
        ] = defaultdict(dict)
        for key, iri2nodes in property_iri2nodes.items():
            for iri, nodes in iri2nodes.items():
                iri2properties[iri][key] = nodes

        return [
            (
                OntospeciesSpeciesPartial(
                    **base_model.model_dump(),
                    altLabel=field2iri2values["altLabel"].get(base_model.IRI),
                    ChemicalClass=iri2chemclass.get(base_model.IRI),
                    Use=iri2use.get(base_model.IRI),
                    Identifier=iri2identifiers.get(base_model.IRI),
                    Property=iri2properties.get(base_model.IRI),
                )
                if base_model
                else None
            )
            for base_model in species_base
        ]

    def get_properties_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        if (
            sparql_client is None
            or (
                isinstance(sparql_client, SparqlClient)
                and sparql_client.sparql.endpoint == self.sparql_client.sparql.endpoint
            )
            or self.sparql_client.sparql.endpoint == sparql_client
        ):
            store = self
        else:
            store = RDFStore(
                sparql_client
                if isinstance(sparql_client, str)
                else sparql_client.sparql.endpoint
            )
        return store.get_many(OntospeciesProperty, iris)

    def get_identifiers_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntospeciesIdentifier, iris)

    def get_chemical_classes_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntospeciesChemicalClass, iris)

    def get_chemical_classes_all(self):
        return self.get_all(OntospeciesChemicalClass, ONTOSPECIES.ChemicalClass)

    def get_uses_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntospeciesUse, iris)

    def get_uses_all(self):
        return self.get_all(OntospeciesUse, ONTOSPECIES.Use)
    
    def get_dissociation_constant_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntospeciesDissociationConstant, iris)
    
    
    # TODO: ONTOSPECIES_V3 should be remove after merging
    # override _get_all() from parent class to include sparql_client_v3
    def _get_many(
        self, T: type[T], iris: list[str] | tuple[str], return_fields: list[str] | None
    ):
        return_fields = None if return_fields is None else set(return_fields)

        if not iris:
            empty_lst: list[T | None] = []
            return empty_lst

        query = """SELECT ?iri ?field ?value
WHERE {{
    VALUES ?iri {{ {iris} }}
    {triples}
}}""".format(
            iris=" ".join("<{iri}>".format(iri=iri) for iri in set(iris)),
            triples=" UNION ".join(
                """{{
    BIND ( "{field}" as ?field )
    ?iri {predicate} ?value .
}}""".format(
                    field=field,
                    predicate=metadata["path"].n3(),
                )
                for field, metadata in T.get_rdf_fields().items()
                if return_fields is None or field in return_fields
            ),
        )

        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        _, bindings_v3 = self.sparql_client_v3.querySelectThenFlatten(query)
        bindings += bindings_v3
        
        field2iri2values: dict[str, defaultdict[str, list[str]]] = defaultdict(
            lambda: defaultdict(list)
        )
        for binding in bindings:
            if "field" not in binding or "value" not in binding:
                continue
            field2iri2values[binding["field"]][binding["iri"]].append(binding["value"])

        def resolve_field_value_batch(field: str, info: FieldInfo):
            annotation = info.annotation
            if not annotation:
                pass
            else:       
                def list_values_process(t):
                    iri2values = field2iri2values[field]    
                    if issubclass(t, RDFEntity):
                        flattened = [v for values in iri2values.values() for v in values]
                        models = [x for x in self.get_many(t, flattened) if x]
                        count = 0
                        out: dict[str, list[RDFEntity]] = dict()
                        for iri, iri2values in iri2values.items():
                            out[iri] = models[count : count + len(iri2values)]
                            count += len(iri2values)
                        return out
                    else:
                        return iri2values
                    
                origin = get_origin(annotation)
                args = get_args(annotation) 
                   
                if origin == list:
                    return list_values_process(args[0])
                else:
                    for arg in args:
                        if get_origin(arg) is list:
                            return list_values_process(get_args(arg)[0])

            iri2values = field2iri2values[field]
            iri2value = {
                iri: values[0] if values else None for iri, values in iri2values.items()
            }

            if annotation:
                unpacked_type = unpack_optional_type(annotation)
                try:
                    if issubclass(unpacked_type, RDFEntity):
                        models = self.get_many(unpacked_type, iri2value.values())
                        return {
                            iri: model for iri, model in zip(iri2value.keys(), models)
                        }
                except:
                    pass
            return iri2value

        field2iri2data = {
            field: resolve_field_value_batch(field, info)
            for field, info in T.model_fields.items()
            if field != "IRI" and (return_fields is None or field in return_fields)
        }

        iri2field2data: defaultdict[
            str, dict[str, RDFEntity | str | list[RDFEntity] | list[str]]
        ] = defaultdict(dict)
        for field, iri2data in field2iri2data.items():
            for iri, data in iri2data.items():
                iri2field2data[iri][field] = data

        def resolve_field_value(
            model_fields: dict[str, FieldInfo],
            field2data: dict[str, RDFEntity | str | list[RDFEntity] | list[str]],
        ):
            if all(
                field in field2data for field in model_fields.keys() if field != "IRI"
            ):
                return field2data

            data = dict(field2data)
            for field, info in model_fields.items():
                if field == "IRI" or field in data:
                    continue
                annotation = info.annotation
                if annotation:
                    if get_origin(annotation) is list:
                        data[field] = list()
                    else:
                        try:
                            if NoneType in get_args(annotation):
                                data[field] = None
                        except:
                            pass
            return data

        def parse(data: dict):
            try:
                return T.model_validate(data)
            except:
                return None

        return [
            parse(
                {
                    **resolve_field_value(
                        model_fields=T.model_fields,
                        field2data=iri2field2data[iri],
                    ),
                    "IRI": iri,
                }
            )
            for iri in iris
        ]
        
    # TODO: ONTOSPECIES_V3 should be remove after merging
    # override _get_all() from parent class to include sparql_client_v3
    def get_all(self, T: type[T], type_iri: URIRef):
        query = f"""SELECT DISTINCT ?IRI
WHERE {{
    ?IRI rdf:type {type_iri.n3()} .
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        _, bindings_v3 = self.sparql_client_v3.querySelectThenFlatten(query)
        bindings += bindings_v3
        iris = [binding["IRI"] for binding in bindings]
        return [model for model in self.get_many(T, iris) if model]



# TODO: ONTOSPECIES_V3 should be remove after merging
@cache
def get_ontospecies_rdfStore(
    endpoint: Annotated[str, Depends(get_ontospecies_endpoint)],
    ontospecies_endpoint_v3: Annotated[str, Depends(get_ontospecies_endpoint_v3)]   
):
    return OntospeciesRDFStore(endpoint, ontospecies_endpoint_v3)
