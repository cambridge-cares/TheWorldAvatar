from typing import Dict
from locate_then_ask.kg_client import KgClient
from locate_then_ask.ontokin.model import (
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
        self.iri2species: Dict[str, OKSpecies] = dict()
        self.iri2rxn: Dict[str, OKGasePhaseReaction] = dict()
        self.iri2mechanism: Dict[str, OKMechanism] = dict()

    def get_species(self, entity_iri: str):
        if entity_iri not in self.iri2species:
            self.iri2species[entity_iri] = self.create_species(entity_iri)
        return self.iri2species[entity_iri]

    def get_rxn(self, entity_iri: str):
        if entity_iri not in self.iri2rxn:
            self.iri2rxn[entity_iri] = self.create_rxn(entity_iri)
        return self.iri2rxn[entity_iri]

    def get_mechanism(self, entity_iri: str):
        if entity_iri not in self.iri2mechanism:
            self.iri2mechanism[entity_iri] = self.create_mechanism(entity_iri)
        return self.iri2mechanism[entity_iri]

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
        mechanisms = self.retrieve_rxn_mechansim(entity_iri)
        return OKGasePhaseReaction(
            iri=entity_iri,
            equation=equation,
            arrhenius_coeffs=arrhenius_coeffs,
            mechanisms=mechanisms,
        )

    def create_mechanism(self, entity_iri: str):
        label = self.retrieve_label(entity_iri)
        return OKMechanism(iri=entity_iri, label=label)

    def retrieve_label(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT * WHERE {{
    <{IRI}> rdfs:label ?Label .
}}
LIMIT 1"""
        query = query_template.format(IRI=entity_iri)
        response_bindings = self.kg_client.query(query)
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

        response_bindings = self.kg_client.query(query)
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

        response_bindings = self.kg_client.query(query)

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
        response_bindings = self.kg_client.query(query)

        return self.get_mechanism(entity_iri=response_bindings[0]["Mechanism"]["value"])

    def retrieve_rxn_eqn(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT * WHERE {{
    <{ReactionIRI}> ontokin:hasEquation ?Equation .
}}"""
        query = query_template.format(ReactionIRI=entity_iri)
        response_bindings = self.kg_client.query(query)
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
        response_bindings = self.kg_client.query(query)
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

    def retrieve_rxn_mechansim(self, entity_iri: str):
        query_template = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT * WHERE {{
    <{ReactionIRI}> ontokin:belongsToPhase/ontokin:containedIn ?Mechanism .
}}"""
        query = query_template.format(ReactionIRI=entity_iri)
        response_bindings = self.kg_client.query(query)
        mechanism_iris = [
            binding["Mechanism"]["value"] for binding in response_bindings
        ]
        return [self.get_mechanism(iri) for iri in mechanism_iris]
