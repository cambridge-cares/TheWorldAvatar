from collections import defaultdict
from typing import Sequence

from pydantic import TypeAdapter

from constants.prefixes import URI_OKIN
from model.orm.base import HasValueHasUnit
from model.orm.ontokin import (
    OkinArrheniusModel,
    OkinArrheniusModelBase,
    OkinCollider,
    OkinKineticModel,
    OkinLindemannModel,
    OkinMechanism,
    OkinMultiArrheniusModel,
    OkinPDepArrheniusModel,
    OkinReaction,
    OkinThreeBodyReactionModel,
    OkinTroeModel,
)
from services.kg import KgClient


class OntoKinORM:
    def __init__(self, bg_client: KgClient):
        self.bg_client = bg_client
        self.mechanism_lst_adapter = TypeAdapter(list[OkinMechanism])
        self.reaction_lst_adapter = TypeAdapter(list[OkinReaction])
        self.hasvalue_hasunit_lst_adapter = TypeAdapter(list[HasValueHasUnit])
        self.collider_lst_adapter = TypeAdapter(list[OkinCollider])

    def get_mechanisms(self, iris: Sequence[str]):
        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
PREFIX op: <http://www.theworldavatar.com/ontology/ontoprovenance/OntoProvenance.owl#>

SELECT ?IRI ?Provenance
WHERE {{
    VALUES ?IRI {{ {values} }}
    ?IRI okin:hasProvenance/(op:hasDOI|op:hasURL) ?Provenance .
}}""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
        )
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        data = self.mechanism_lst_adapter.validate_python(bindings)
        iri2datum = {datum.IRI: datum for datum in data}
        return [iri2datum.get(iri) for iri in iris]

    def get_reactions(self, iris: Sequence[str]):
        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT ?Reaction (SAMPLE(?Equation) AS ?Equation)
WHERE {{
    VALUES ?Reaction {{ {values} }}
    ?Reaction okin:hasEquation ?Equation .
}}
GROUP BY ?Reaction""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
        )
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        data = self.reaction_lst_adapter.validate_python(bindings)
        iri2datum = {datum.IRI: datum for datum in data}
        return [iri2datum.get(iri) for iri in iris]

    def _get_base_arrhenius_model_lsts(
        self, iris: Sequence[str], predicate: str | None = None
    ):
        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?IRI {{ {values} }}
    {path}
    ?ArrheniusModel
        okin:hasActivationEnergy [ okin:value ?ActivationEnergyValue ; okin:unit ?ActivationEnergyUnit ] ; 
        okin:hasArrheniusFactor [ okin:value ?ArrheniusFactorValue ; okin:unit ?ArrheniusFactorUnit ] ; 
        okin:hasTemperatureExponent/okin:value ?TemperatureExponentValue .
}}""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris),
            path=(
                "?IRI {predicate} ?ArrheniusModel .".format(predicate=predicate)
                if predicate
                else "BIND ( ?IRI AS ?ArrheniusModel )"
            ),
        )
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        activation_energies = self.hasvalue_hasunit_lst_adapter.validate_python(
            [
                {
                    k[len("ActivationEnergy") :]: v
                    for k, v in binding.items()
                    if k.startswith("ActivationEnergy")
                }
                for binding in bindings
            ]
        )
        arrhenius_factors = self.hasvalue_hasunit_lst_adapter.validate_python(
            [
                {
                    k[len("ArrheniusFactor") :]: v
                    for k, v in binding.items()
                    if k.startswith("ArrheniusFactor")
                }
                for binding in bindings
            ]
        )
        data = [
            {
                **binding,
                "ActivationEnergy": activation_energy,
                "ArrheniusFactor": arrhenius_factor,
            }
            for binding, activation_energy, arrhenius_factor in zip(
                bindings, activation_energies, arrhenius_factors
            )
        ]
        iri2lst: defaultdict[str, list[OkinArrheniusModelBase]] = defaultdict(list)
        for binding, datum in zip(bindings, data):
            iri2lst[binding["IRI"]].append(datum)
        return [iri2lst[iri] for iri in iris]

    def _get_base_arrhenius_models(
        self, iris: Sequence[str], predicate: str | None = None
    ):
        return [
            lst[0] if lst else None
            for lst in self._get_base_arrhenius_model_lsts(
                iris=iris, predicate=predicate
            )
        ]

    def get_arrhenius_models(self, iris: Sequence[str]):
        base_models = self._get_base_arrhenius_models(iris)
        return [
            (
                OkinArrheniusModel(IRI=iri, **base_model.model_dump())
                if base_model
                else None
            )
            for iri, base_model in zip(iris, base_models)
        ]

    def get_pdep_arrhenius_models(self, iris: Sequence[str]):
        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?IRI {{ {values} }}
    ?IRI okin:hasArrheniusModel ?ArrheniusModel .
    ?Pressure okin:isPressureConditionOf ?ArrheniusModel ; okin:value ?PressureValue .
}}""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
        )
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        iri2binding = {binding["IRI"]: binding for binding in bindings}
        pressure_data = [iri2binding.get(iri, {}) for iri in iri2binding]

        base_models = self._get_base_arrhenius_models(iris)

        return [
            (
                OkinPDepArrheniusModel(**pressure_datum, **base_model.model_dump())
                if pressure_datum and base_model
                else None
            )
            for pressure_datum, base_model in zip(pressure_data, base_models)
        ]

    def get_multi_arrhenius_models(self, iris: Sequence[str]):
        base_model_lsts = self._get_base_arrhenius_model_lsts(
            iris=iris, predicate="okin:hasArrheniusModel"
        )
        return [
            OkinMultiArrheniusModel(IRI=iri, ArrheniusModels=lst)
            for iri, lst in zip(iris, base_model_lsts)
        ]

    def _get_collider_lsts(self, iris: Sequence[str], predicate: str | None = None):
        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?IRI {{ {values} }}
    {path}
    ?Collider rdfs:label ?Label ; okin:hasEfficiency ?Efficiency .
}}""".format(
            values=" ".format("<{iri}>".format(iri=iri) for iri in iris),
            path=(
                "?IRI {predicate} ?Collider .".format(predicate=predicate)
                if predicate
                else "BIND ( ?IRI AS ?Collider )"
            ),
        )
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        data = self.collider_lst_adapter.validate_python(bindings)
        iri2lst: defaultdict[str, list[OkinCollider]] = defaultdict(list)
        for binding, datum in zip(bindings, data):
            iri2lst[binding["IRI"]].append(datum)
        return [iri2lst[iri] for iri in iris]

    def get_three_body_reaction_models(self, iris: Sequence[str]):
        collider_lsts = self._get_collider_lsts(iris=iris, predicate="okin:hasCollider")
        base_models = self._get_base_arrhenius_models(
            iris=iris, predicate="okin:hasArrheniusLowModel"
        )
        return [
            (
                OkinThreeBodyReactionModel(
                    IRI=iri, Colliders=collider_lst, ArrheniusLowModel=base_model
                )
                if base_model
                else None
            )
            for iri, collider_lst, base_model in zip(iris, collider_lsts, base_models)
        ]

    def get_lindemann_models(self, iris: Sequence[str]):
        collider_lsts = self._get_collider_lsts(iris=iris, predicate="okin:hasCollider")
        low_models = self._get_base_arrhenius_models(
            iris=iris, predicate="okin:hasArrheniusLowModel"
        )
        high_models = self._get_base_arrhenius_models(
            iris=iris, predicate="okin:hasArrheniusHighModel"
        )

        return [
            (
                OkinLindemannModel(
                    IRI=iri,
                    Colliders=collider_lst,
                    ArrheniusLowModel=low_model,
                    ArrheniusHighModel=high_model,
                )
                if low_model and high_model
                else None
            )
            for iri, collider_lst, low_model, high_model in zip(
                iris, collider_lsts, low_models, high_models
            )
        ]

    def get_troe_models(self, iris: Sequence[str]):
        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT *
WHERE {{
    VALUES ?IRI {{ {values} }}
    ?IRI
        okin:hasAlpha/okin:value ?Alpha ;
        okin:hasT1/okin:value ?T1 ;
        okin:hasT2/okin:value ?T2 ;
        okin:hasT3/okin:value ?T3 .
}}""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
        )
        _, bindings = self.bg_client.querySelectThenFlatten(query)
        iri2troeparams = {binding["IRI"]: binding for binding in bindings}
        troe_params_lst = [{"IRI": iri, **iri2troeparams.get(iri, {})} for iri in iris]

        collider_lsts = self._get_collider_lsts(iris=iris, predicate="okin:hasCollider")
        low_models = self._get_base_arrhenius_models(
            iris=iris, predicate="okin:hasArrheniusLowModel"
        )
        high_models = self._get_base_arrhenius_models(
            iris=iris, predicate="okin:hasArrheniusHighModel"
        )

        return [
            (
                OkinTroeModel(
                    Colliders=collider_lst,
                    ArrheniusLowModel=low_model,
                    ArrheniusHighModel=high_model,
                    **troe_params
                )
                if low_model and high_model and troe_params
                else None
            )
            for troe_params, collider_lst, low_model, high_model in zip(
                troe_params_lst, collider_lsts, low_models, high_models
            )
        ]

    def get_kinetic_models(self, iris: Sequence[str]):
        query = """SELECT * 
WHERE {{
    VALUES ?KineticModel {{ {values} }}
    ?KineticModel a ?Type .
}}""".format(
            values=" ".join("<{iri}>".format(iri=iri) for iri in iris)
        )

        _, bindings = self.bg_client.querySelectThenFlatten(query)

        type2iris: defaultdict[str, list[str]] = defaultdict(list)
        for binding in bindings:
            type2iris[binding["Type"]].append(binding["KineticModel"])

        iri2data: dict[str, OkinKineticModel] = dict()
        for type, same_type_iris in type2iris.items():
            if type == URI_OKIN + "ArrheniusModel":
                getter = self.get_arrhenius_models
            elif type == URI_OKIN + "PDepArrheniusModel":
                getter = self.get_pdep_arrhenius_models
            elif type == URI_OKIN + "MultiArrheniusModel":
                getter = self.get_multi_arrhenius_models
            elif type == URI_OKIN + "ThreeBodyReactionModel":
                getter = self.get_three_body_reaction_models
            elif type == URI_OKIN + "LindemannModel":
                getter = self.get_lindemann_models
            elif type == URI_OKIN + "TroeModel":
                getter = self.get_troe_models
            data = getter(iris=same_type_iris)
            iri2data.update({iri: datum for iri, datum in zip(same_type_iris, data)})

        return [iri2data.get(iri) for iri in iris]
