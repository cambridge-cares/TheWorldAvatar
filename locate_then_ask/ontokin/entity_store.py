from typing import Dict, List, Union
from constants.namespaces import OKIN
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontokin.model import (
    OCAPEProduct,
    OCAPEReactant,
    OKArrheniusCoefficient,
    OKMechanism,
    OKGasePhaseReaction,
    OKSpecies,
    OKThermoModel,
    OKTransportModel,
)


class OKEntityStore:
    def __init__(
        self,
        kg_endpoint: str = "http://theworldavatar.com/blazegraph/namespace/ontokin/sparql",
    ):
        self.kg_client = KgClient(kg_endpoint)
        self.iri2cls: Dict[
            str, Union[OKSpecies, OKGasePhaseReaction, OKMechanism]
        ] = dict()
        self.iri2entity: Dict[
            str, Union[OKSpecies, OKGasePhaseReaction, OKMechanism]
        ] = dict()

    def get_cls(self, entity_iri: str):
        if entity_iri not in self.iri2cls:
            query_template = """SELECT * WHERE {{ <{IRI}> a/rdfs:subClassOf* ?Type }}"""
            query = query_template.format(IRI=entity_iri)
            response_bindings = self.kg_client.query(query)["results"]["bindings"]
            types = [binding["Type"]["value"] for binding in response_bindings]
            if OKIN + "ReactionMechanism" in types:
                self.iri2cls[entity_iri] = OKMechanism
            elif OKIN + "GasPhaseReaction" in types:
                self.iri2cls[entity_iri] = OKGasePhaseReaction
            elif OKIN + "Species" in types:
                self.iri2cls[entity_iri] = OKSpecies
            else:
                raise ValueError(
                    "The provided entity {iri} does not posess any expected types.\n Actual types: {types}.\nExpected types: okin:ReactionMechanism, okin:GasPhaseReaction, okin:Species".format(
                        iri=entity_iri, types=types
                    )
                )
        return self.iri2cls[entity_iri]

    def get(self, entity_iri: str):
        if entity_iri not in self.iri2entity:
            cls = self.get_cls(entity_iri)
            if cls == OKMechanism:
                self.iri2entity[entity_iri] = self.create_mechanism(entity_iri)
            elif cls == OKGasePhaseReaction:
                self.iri2entity[entity_iri] = self.create_rxn(entity_iri)
            else:
                self.iri2entity[entity_iri] = self.create_species(entity_iri)
        return self.iri2entity[entity_iri]

    def create_species(self, entity_iri: str):
        label = self.retrieve_label(entity_iri)
        thermo_models = self.retrieve_species_thermo_models(entity_iri)
        transport_model = self.retrieve_species_transport_model(entity_iri)
        mechanism = self.retrieve_species_mechanism(entity_iri)
        return OKSpecies(
            iri=entity_iri,
            label=label,
            thermo_models=thermo_models,
            transport_model=transport_model,
            mechanism=mechanism,
        )

    def create_rxn(self, entity_iri: str):
        equation = self.retrieve_rxn_eqn(entity_iri)
        arrhenius_coeffs = self.retrieve_rxn_arrhenius_coeffs(entity_iri)
        reactants = self.retrieve_rxn_reactants(entity_iri)
        products = self.retrieve_rxn_products(entity_iri)
        mechanisms = self.retrieve_rxn_mechansim(entity_iri)
        return OKGasePhaseReaction(
            iri=entity_iri,
            equation=equation,
            arrhenius_coeffs=arrhenius_coeffs,
            reactants=reactants,
            products=products,
            mechanisms=mechanisms,
        )

    def create_mechanism(self, entity_iri: str):
        label = self.retrieve_label(entity_iri)
        species_iris = self.retrieve_mechanism_species_iris(entity_iri)
        return OKMechanism(iri=entity_iri, label=label, species_iris=species_iris)

    def retrieve_label(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT * WHERE {{
    <{IRI}> rdfs:label ?Label .
}}
LIMIT 1"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["Label"]["value"]

    def retrieve_species_thermo_models(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT * WHERE {{
    <{SpeciesIRI}> ontokin:hasThermoModel [
        ontokin:hasCoefficientValues ?CoefficientValues ;
        ontokin:hasNumberOfCoefficients ?NumberOfCoefficients ;
        ontokin:hasMaximumTemperature ?MaximumTemperature ;
        ontokin:hasMinimumTemperature ?MinimumTemperature ;
        ontokin:hasPressure ?Pressure
    ] .
}}"""
        query = query_template.format(SpeciesIRI=entity_iri)

        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        value_bindings = [
            {k: v["value"] for k, v in binding.items()} for binding in response_bindings
        ]

        return [
            OKThermoModel(
                coeff_values=binding["CoefficientValues"],
                coeff_num=int(binding["NumberOfCoefficients"]),
                max_temp=float(binding["MaximumTemperature"]),
                min_temp=float(binding["MinimumTemperature"]),
                pressure=float(binding["Pressure"]),
            )
            for binding in value_bindings
        ]

    def retrieve_species_transport_model(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT * WHERE {{
    <{SpeciesIRI}> ontokin:hasTransportModel [
        ontokin:hasDipoleMoment ?DipoleMoment ;
        ontokin:hasDipoleMomentUnits ?DipoleMomentUnits ;
        ontokin:hasLennardJonesDiameter ?LennardJonesDiameter ;
        ontokin:hasLennardJonesDiameterUnits ?LennardJonesDiameterUnits ;
        ontokin:hasLennardJonesWellDepth ?LennardJonesWellDepth ;
        ontokin:hasLennardJonesWellDepthUnits ?LennardJonesWellDepthUnits ;
        ontokin:hasPolarizability ?Polarizability ;
        ontokin:hasPolarizabilityUnits ?PolarizabilityUnits ;
        ontokin:hasRotationalRelaxationCollisionNumber ?RotationalRelaxationCollisionNumber ;
        ontokin:hasRotationalRelaxationCollisionNumberUnits ?RotationalRelaxationCollisionNumberUnits ;
        ontokin:hasSpeciesGeometry ?SpeciesGeometry ;
        ontokin:hasSpeciesGeometryTitle ?SpeciesGeometryTitle
    ] .
}}
LIMIT 1"""
        query = query_template.format(SpeciesIRI=entity_iri)

        response_bindings = self.kg_client.query(query)["results"]["bindings"]

        if len(response_bindings) == 0:
            return None

        binding = {k: v["value"] for k, v in response_bindings[0].items()}
        return OKTransportModel(
            dipole_momemnt=float(binding["DipoleMoment"]),
            dipole_moment_units=binding["DipoleMomentUnits"],
            LJ_diameter=float(binding["LennardJonesDiameter"]),
            LJ_diameter_units=binding["LennardJonesDiameterUnits"],
            LJ_well_depth=float(binding["LennardJonesWellDepth"]),
            LJ_well_depth_units=binding["LennardJonesWellDepthUnits"],
            polarizability=float(binding["Polarizability"]),
            polarizability_units=binding["PolarizabilityUnits"],
            rotational_relaxation_collision_num=float(
                binding["RotationalRelaxationCollisionNumber"]
            ),
            rotational_relaxation_collision_num_units=binding[
                "RotationalRelaxationCollisionNumberUnits"
            ],
            species_geometry=binding["SpeciesGeometry"],
            species_geometry_title=binding["SpeciesGeometryTitle"],
        )

    def retrieve_species_mechanism(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT * WHERE {{
    <{SpeciesIRI}> ontokin:belongsToPhase/ontokin:containedIn ?Mechanism .
}}
LIMIT 1"""
        query = query_template.format(SpeciesIRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]

        return self.get(entity_iri=response_bindings[0]["Mechanism"]["value"])

    def retrieve_rxn_eqn(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT * WHERE {{
    <{ReactionIRI}> ontokin:hasEquation ?Equation .
}}"""
        query = query_template.format(ReactionIRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return response_bindings[0]["Equation"]["value"]

    def retrieve_rxn_arrhenius_coeffs(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT * WHERE {{
    <{ReactionIRI}> ontokin:hasArrheniusCoefficient [
        ontokin:hasActivationEnergy ?ActivationEnergy ;
        ontokin:hasActivationEnergyUnits ?ActivationEnergyUnits ;
        ontokin:hasPreExponentialFactor ?PreExponentialFactor ;
        ontokin:hasPreExponentialFactorUnits ?PreExponentialFactorUnits ;
        ontokin:hasTemperatureExponent ?TemperatureExponent ;
        ontokin:hasTemperatureExponentUnits ?TemperatureExponentUnits
    ] .
}}"""
        query = query_template.format(ReactionIRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        value_bindings = [
            {k: v["value"] for k, v in binding.items()} for binding in response_bindings
        ]
        return [
            OKArrheniusCoefficient(
                activation_energy=float(binding["ActivationEnergy"]),
                activation_energy_units=binding["ActivationEnergyUnits"],
                preexponential_factor=float(binding["PreExponentialFactor"]),
                preexponential_factor_units=binding["PreExponentialFactorUnits"],
                temp_exponent=float(binding["TemperatureExponent"]),
                temp_exponent_units=binding["TemperatureExponentUnits"],
            )
            for binding in value_bindings
        ]

    def retrieve_rxn_reactants(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocape: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>

SELECT DISTINCT * WHERE {{
    <{ReactionIRI}> ontocape:hasReactant ?Reactant .
    ?Reactant rdfs:label ?ReactantLabel .
}}"""
        query = query_template.format(ReactionIRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        value_bindings = [{k: v["value"] for k, v in binding.items()} for binding in response_bindings]
        return [
            OCAPEReactant(iri=binding["Reactant"], label=binding["ReactantLabel"]["value"])
            for binding in response_bindings
        ]

    def retrieve_rxn_products(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocape: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>

SELECT DISTINCT * WHERE {{
    <{ReactionIRI}> ontocape:hasProduct ?Product .
    ?Product rdfs:label ?ProductLabel .
}}"""
        query = query_template.format(ReactionIRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        value_bindings = [{k: v["value"] for k, v in binding.items()} for binding in response_bindings]
        return [
            OCAPEProduct(iri=binding["Product"], label=binding["ProductLabel"])
            for binding in value_bindings
        ]

    def retrieve_rxn_mechansim(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT * WHERE {{
    <{ReactionIRI}> ontokin:belongsToPhase/ontokin:containedIn ?Mechanism .
}}"""
        query = query_template.format(ReactionIRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        mechanism_iris = [
            binding["Mechanism"]["value"] for binding in response_bindings
        ]
        return [self.get(iri) for iri in mechanism_iris]

    def retrieve_mechanism_species_iris(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT DISTINCT ?Species WHERE {{
    <{MechanismIRI}> ^ontokin:containedIn/^ontokin:belongsToPhase ?Species .
    ?Species a ontokin:Species .
}}"""
        query = query_template.format(MechanismIRI=entity_iri)
        response_bindings = self.kg_client.query(query)["results"]["bindings"]
        return [binding["Species"]["value"] for binding in response_bindings]
