# import this postponed evaluation of annotations to enable circular and forward reference in ontology classes
# it should be used together with pydantic.BaseModel.update_forward_refs() method at the end of your data model script
# e.g. YourOntologyClass.update_forward_refs()
# for more details on python implementation, please see https://www.python.org/dev/peps/pep-0563/
# for more details on how this works with pydantic.BaseModel, please see https://pydantic-docs.helpmanual.io/usage/postponed_annotations/
# NOTE that this feature requires a python version >=3.7, which has already been defined in the setup.py
from __future__ import annotations

from rdflib import Graph, Literal, URIRef
from rdflib.namespace import RDF

import pydantic
from typing import Any, Optional, List, Dict, Union

from chemistry_and_robots.data_model.iris import *
from pyderivationagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import *

AVAILABLE_PERFORMANCE_INDICATOR_LIST = [
    ONTOREACTION_YIELD,
    ONTOREACTION_CONVERSION,
    ONTOREACTION_SPACETIMEYIELD,
    ONTOREACTION_RUNMATERIALCOST,
    ONTOREACTION_RUNMATERIALCOSTPERKILOGRAMPRODUCT,
    ONTOREACTION_ECOSCORE,
    ONTOREACTION_ENVIRONMENTALFACTOR,
]
OBJECT_RELATIONSHIP_PERFORMANCE_INDICATOR_RXN_EXP_DICT = {
    ONTOREACTION_YIELD:[ONTOREACTION_HASPERFORMANCEINDICATOR, ONTOREACTION_HASYIELD],
    ONTOREACTION_CONVERSION:[ONTOREACTION_HASPERFORMANCEINDICATOR, ONTOREACTION_HASCONVERSION],
    ONTOREACTION_SPACETIMEYIELD:[ONTOREACTION_HASPERFORMANCEINDICATOR, ONTOREACTION_HASSPACETIMEYIELD],
    ONTOREACTION_RUNMATERIALCOST:[ONTOREACTION_HASPERFORMANCEINDICATOR, ONTOREACTION_HASRUNMATERIALCOST],
    ONTOREACTION_RUNMATERIALCOSTPERKILOGRAMPRODUCT:[ONTOREACTION_HASPERFORMANCEINDICATOR, ONTOREACTION_HASRUNMATERIALCOSTPERKILOGRAMPRODUCT],
    ONTOREACTION_ECOSCORE:[ONTOREACTION_HASPERFORMANCEINDICATOR, ONTOREACTION_HASECOSCORE],
    ONTOREACTION_ENVIRONMENTALFACTOR:[ONTOREACTION_HASPERFORMANCEINDICATOR, ONTOREACTION_HASENVIRONMENTALFACTOR]
}

OBJECT_RELATIONSHIP_REACTION_CONDITION_RXN_EXP_DICT = {
    ONTOREACTION_STOICHIOMETRYRATIO:[ONTOREACTION_HASREACTIONCONDITION, ONTOREACTION_HASSTOICHIOMETRYRATIO],
    ONTOREACTION_REACTIONTEMPERATURE:[ONTOREACTION_HASREACTIONCONDITION, ONTOREACTION_HASRXNTEMPERATURE],
    ONTOREACTION_REACTIONSCALE:[ONTOREACTION_HASREACTIONCONDITION, ONTOREACTION_HASRXNSCALE],
    ONTOREACTION_REACTIONPRESSURE:[ONTOREACTION_HASREACTIONCONDITION, ONTOREACTION_HASRXNPRESSURE],
    ONTOREACTION_RESIDENCETIME:[ONTOREACTION_HASREACTIONCONDITION, ONTOREACTION_HASRESTIME],
}

ROUND_DECIMAL_PLACES_REACTION_CONDITION_RXN_EXP_DICT = {
    ONTOREACTION_STOICHIOMETRYRATIO: 2,
    ONTOREACTION_REACTIONTEMPERATURE: 0,
    ONTOREACTION_REACTIONSCALE: 2,
    ONTOREACTION_REACTIONPRESSURE: 0, # TODO [future work] double-check when adding pressure into optimisation
    ONTOREACTION_RESIDENCETIME: 2,
}

# NOTE here we start with unified unit provided above
AVAILABLE_PERFORMANCE_INDICATOR_UNIT_DICT = {
    ONTOREACTION_YIELD: [UNIFIED_YIELD_UNIT],
    ONTOREACTION_CONVERSION: [UNIFIED_CONVERSION_UNIT],
    ONTOREACTION_SPACETIMEYIELD: [UNIFIED_SPACETIMEYIELD_UNIT, SCALED_SPACETIMEYIELD_UNIT],
    ONTOREACTION_RUNMATERIALCOST: [UNIFIED_RUN_MATERIAL_COST_UNIT],
    ONTOREACTION_RUNMATERIALCOSTPERKILOGRAMPRODUCT: [UNIFIED_RUN_MATERIAL_COST_PER_KILOGRAM_PRODUCT_UNIT],
    ONTOREACTION_ECOSCORE: [UNIFIED_ECOSCORE_UNIT],
    ONTOREACTION_ENVIRONMENTALFACTOR: [UNIFIED_ENVIRONMENTFACTOR_UNIT]
}

UNIFIED_UNIT_FOR_PERFORMANCE_INDICATOR_DICT = {
    ONTOREACTION_YIELD: UNIFIED_YIELD_UNIT,
    ONTOREACTION_CONVERSION: UNIFIED_CONVERSION_UNIT,
    ONTOREACTION_SPACETIMEYIELD: UNIFIED_SPACETIMEYIELD_UNIT,
    ONTOREACTION_RUNMATERIALCOST: UNIFIED_RUN_MATERIAL_COST_UNIT,
    ONTOREACTION_RUNMATERIALCOSTPERKILOGRAMPRODUCT: UNIFIED_RUN_MATERIAL_COST_PER_KILOGRAM_PRODUCT_UNIT,
    ONTOREACTION_ECOSCORE: UNIFIED_ECOSCORE_UNIT,
    ONTOREACTION_ENVIRONMENTALFACTOR: UNIFIED_ENVIRONMENTFACTOR_UNIT
}

DISPLAY_UNIT_FOR_PERFORMANCE_INDICATOR_DICT = {
    ONTOREACTION_YIELD: UNIFIED_YIELD_UNIT,
    ONTOREACTION_CONVERSION: UNIFIED_CONVERSION_UNIT,
    ONTOREACTION_SPACETIMEYIELD: SCALED_SPACETIMEYIELD_UNIT,
    ONTOREACTION_RUNMATERIALCOST: UNIFIED_RUN_MATERIAL_COST_UNIT,
    ONTOREACTION_RUNMATERIALCOSTPERKILOGRAMPRODUCT: UNIFIED_RUN_MATERIAL_COST_PER_KILOGRAM_PRODUCT_UNIT,
    ONTOREACTION_ECOSCORE: UNIFIED_ECOSCORE_UNIT,
    ONTOREACTION_ENVIRONMENTALFACTOR: UNIFIED_ENVIRONMENTFACTOR_UNIT
}

# NOTE only classes/relationships that are actively used in OntoReaction are presented here for ALL OntoCAPE related concepts in this script
class OntoCAPE_ScalarValue(BaseOntology):
    clz: str = ONTOCAPE_SCALARVALUE
    # NOTE here instead of the actual class, str is used in hasUnitOfMeasure to host the concept IRI of om:Unit for simplicity
    # this is in line with the practice of class OM_Measure(BaseOntology)
    hasUnitOfMeasure: str
    numericalValue: float

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <scalar_value> <rdf:type> <OntoCAPE:ScalarValue>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        # <scalar_value> <hasUnitOfMeasure> <unit>
        g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_HASUNITOFMEASURE), URIRef(self.hasUnitOfMeasure)))
        # <scalar_value> <numericalValue> <num>
        g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_NUMERICALVALUE), Literal(self.numericalValue)))
        return g

class OntoCAPE_PhaseComponentConcentration(BaseOntology):
    clz: str = ONTOCAPE_PHASECOMPONENTCONCENTRATION
    hasValue: OntoCAPE_ScalarValue

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <phase_component_conc> <rdf:type> <OntoCAPE:PhaseComponentConcentration>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        # <phase_component_conc> <hasValue> <scalar_value>
        g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_HASVALUE), URIRef(self.hasValue.instance_iri)))
        g = self.hasValue.create_instance_for_kg(g)
        return g

    def convert_to_unit(self, unit: str) -> OntoCAPE_PhaseComponentConcentration:
        if unit == self.hasValue.hasUnitOfMeasure:
            return self
        else:
            raise NotImplementedError(f"Conversion of unit for OntoCAPE_PhaseComponentConcentration from <{self.hasValue.hasUnitOfMeasure}> to <{unit}> is not implemented yet")

class OntoCAPE_VolumeBasedConcentration(OntoCAPE_PhaseComponentConcentration):
    clz: str = ONTOCAPE_VOLUMEBASEDCONCENTRATION

class OntoCAPE_Molarity(OntoCAPE_VolumeBasedConcentration):
    clz: str = ONTOCAPE_MOLARITY

class OntoCAPE_Composition(BaseOntology):
    clz: str = ONTOCAPE_COMPOSITION
    comprisesDirectly: List[OntoCAPE_PhaseComponentConcentration]

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <composition> <rdf:type> <OntoCAPE:Composition>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        # <composition> <comprisesDirectly> <OntoCAPE:PhaseComponentConcentration>
        for pc_conc in self.comprisesDirectly:
            g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_COMPRISESDIRECTLY), URIRef(pc_conc.instance_iri)))
            g = pc_conc.create_instance_for_kg(g)
        return g

class OntoCAPE_PhaseComponent(BaseOntology):
    clz: str = ONTOCAPE_PHASECOMPONENT
    hasProperty: OntoCAPE_PhaseComponentConcentration
    representsOccurenceOf: str # NOTE here it should be pointing to OntoCAPE_ChemicalSpecie, but we simplified to use str for its IRI

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <phase_component> <rdf:type> <OntoCAPE:PhaseComponent>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        # <phase_component> <hasProperty> <OntoCAPE:PhaseComponentConcentration>
        g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_HASPROPERTY), URIRef(self.hasProperty.instance_iri)))
        g = self.hasProperty.create_instance_for_kg(g)
        # <phase_component> <representsOccurenceOf> <species>
        g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_REPRESENTSOCCURENCEOF), URIRef(self.representsOccurenceOf)))
        return g

class OntoCAPE_StateOfAggregation(BaseOntology):
    clz: str = ONTOCAPE_STATEOFAGGREGATION

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <state_of_aggregation> <rdf:type> <OntoCAPE:StateOfAggregation>
        # Only add if not predefined instances by OntoCAPE
        if self.instance_iri not in ONTOCAPE_PREDEFINED_PHASE:
            g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        return g

OntoCAPE_solid = OntoCAPE_StateOfAggregation(instance_iri=ONTOCAPE_SOLID)
OntoCAPE_liquid = OntoCAPE_StateOfAggregation(instance_iri=ONTOCAPE_LIQUID)
OntoCAPE_gaseous = OntoCAPE_StateOfAggregation(instance_iri=ONTOCAPE_GASEOUS)

class OntoCAPE_SinglePhase(BaseOntology):
    clz: str = ONTOCAPE_SINGLEPHASE
    hasStateOfAggregation: OntoCAPE_StateOfAggregation
    isComposedOfSubsystem: List[OntoCAPE_PhaseComponent]
    has_composition: OntoCAPE_Composition
    representsThermodynamicBehaviorOf: Union[str, OntoCAPE_Material] # NOTE here str is provided as an optional as it seems impossible to circular reference at instance level
    # TODO [future work] assess if has_physical_context is needed

    def _exclude_keys_for_compare_(self, *keys_to_exclude) -> Dict[str, Any]:
        return super()._exclude_keys_for_compare_('representsThermodynamicBehaviorOf', *keys_to_exclude)

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <single_phase> <rdf:type> <OntoCAPE:SinglePhase>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))

        # <single_phase> <hasStateOfAggregation> <state_of_aggregation>
        g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_HASSTATEOFAGGREGATION), URIRef(self.hasStateOfAggregation.instance_iri)))
        g = self.hasStateOfAggregation.create_instance_for_kg(g)

        # <single_phase> <isComposedOfSubsystem> <phase_component>
        for pc in self.isComposedOfSubsystem:
            g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_ISCOMPOSEDOFSUBSYSTEM), URIRef(pc.instance_iri)))
            g = pc.create_instance_for_kg(g)

        # <single_phase> <has_composition> <composition>
        g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_HAS_COMPOSITION), URIRef(self.has_composition.instance_iri)))
        g = self.has_composition.create_instance_for_kg(g)

        # <single_phase> <representsThermodynamicBehaviorOf> <material>
        if isinstance(self.representsThermodynamicBehaviorOf, OntoCAPE_Material):
            g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_REPRESENTSTHERMODYNAMICBEHAVIOROF), URIRef(self.representsThermodynamicBehaviorOf.instance_iri)))
        else:
            g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_REPRESENTSTHERMODYNAMICBEHAVIOROF), URIRef(self.representsThermodynamicBehaviorOf)))

        # TODO [future work] add triples related to has_physical_context after it's been implemented

        return g

class OntoCAPE_Material(BaseOntology):
    thermodynamicBehaviour: OntoCAPE_SinglePhase
    clz: str = ONTOCAPE_MATERIAL

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <material> <rdf:type> <OntoCAPE:Material>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        # <material> <thermodynamicBehaviour> <single_phase>
        g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_THERMODYNAMICBEHAVIOR), URIRef(self.thermodynamicBehaviour.instance_iri)))
        g = self.thermodynamicBehaviour.create_instance_for_kg(g)

        return g

    def contains_chemical_species(
        self,
        solute: str,
        solvent_as_constraint: List[str]=None,
        species_to_exclude: List[str]=None,
    ) -> bool:
        if bool(species_to_exclude):
            if not all([s not in [pc.representsOccurenceOf for pc in self.thermodynamicBehaviour.isComposedOfSubsystem] for s in species_to_exclude]):
                return False
        return self.if_contains_requested_chemical_species(solute, solvent_as_constraint)

    def if_contains_requested_chemical_species(
        self,
        solute: str,
        solvent_as_constraint: List[str]=None,
    ) -> bool:
        solute_list = solute.split('|')
        if bool(solvent_as_constraint):
            for sol in solvent_as_constraint:
                list_of_requested_species = solute_list + [sol]
                if all([s in [pc.representsOccurenceOf for pc in self.thermodynamicBehaviour.isComposedOfSubsystem] for s in list_of_requested_species]):
                    return True
                else:
                    continue
        else:
            return all(s in [pc.representsOccurenceOf for pc in self.thermodynamicBehaviour.isComposedOfSubsystem] for s in solute_list)
        return False

    def get_concentration_of_species(self, species_iri: str, target_unit: str=None) -> OntoCAPE_PhaseComponentConcentration:
        for pc in self.thermodynamicBehaviour.isComposedOfSubsystem:
            if pc.representsOccurenceOf == species_iri:
                if target_unit is None:
                    return pc.hasProperty
                else:
                    return pc.hasProperty.convert_to_unit(target_unit)
        raise ValueError(f"Species {species_iri} not found in InputChemical {self.instance_iri}")

    def existing_species_in_given_list(self, lst_species: list) -> List[str]:
        return [pc.representsOccurenceOf for pc in self.thermodynamicBehaviour.isComposedOfSubsystem if pc.representsOccurenceOf in lst_species]

    def identify_solute_in_list(self, lst_species: list) -> str:
        identified_solute = self.existing_species_in_given_list(lst_species)
        if len(identified_solute) == 1:
            return identified_solute[0]
        elif len(identified_solute) == 0:
            raise ValueError(f"No solute found in {self.instance_iri} given list of species {lst_species}")
        else:
            # TODO [future work] add more logic here to handle multiple solutes
            # currently, we just take the first solute in the list
            return identified_solute[0]
            # raise ValueError(f"Multiple solutes found in {self.instance_iri} given list of species {lst_species}")

class Chemical(OntoCAPE_Material):
    clz: str = ONTOREACTION_CHEMICAL

class InputChemical(Chemical):
    clz: str = ONTOREACTION_INPUTCHEMICAL
    recommended_reaction_scale: OM_Volume = None # TODO [future work] make proper design for this recommended

    def is_reactant_stream(self, reactant_species_iris: List[str]) -> bool:
        # It is a reactant stream if any of the species in the InputChemical appears in reactant_species_iris
        return any([pc.representsOccurenceOf in reactant_species_iris for pc in self.thermodynamicBehaviour.isComposedOfSubsystem])

class OutputChemical(Chemical):
    clz: str = ONTOREACTION_OUTPUTCHEMICAL

class OntoKin_Species(BaseOntology):
    clz: str = ONTOKIN_SPECIES
    hasUniqueSpecies: str # NOTE here we simplify the implementation by using str instead of the actual OntoSpecies:Species

class OntoKin_Reactant(OntoKin_Species):
    clz: str = ONTOKIN_REACTANT

class OntoKin_Product(OntoKin_Species):
    clz: str = ONTOKIN_PRODUCT

class Catalyst(OntoKin_Species):
    clz: str = ONTOREACTION_CATALYST

class Solvent(OntoKin_Species):
    clz: str = ONTOREACTION_SOLVENT

class Base(OntoKin_Species):
    clz: str = ONTOREACTION_BASE

class TargetProduct(OntoKin_Product):
    clz: str = ONTOREACTION_TARGETPRODUCT

class Impurity(OntoKin_Product):
    clz: str = ONTOREACTION_IMPURITY

class ChemicalReaction(BaseOntology):
    clz: str = ONTOREACTION_CHEMICALREACTION
    hasReactant: List[OntoKin_Species]
    hasProduct: List[OntoKin_Species]
    hasCatalyst: Optional[List[OntoKin_Species]]
    hasSolvent: Optional[List[OntoKin_Species]]
    hasBase: Optional[List[OntoKin_Species]]
    # NOTE here we simplify the implementation by using str instead of the actual OntoDoE:DesignOfExperiment
    # NOTE this is to prevent circular import error
    hasDoETemplate: Optional[str]

    def get_list_of_occurring_species(self) -> List[OntoKin_Species]:
        lst_of_species = []
        lst_of_species += self.hasReactant
        lst_of_species += self.hasProduct
        if self.hasCatalyst is not None:
            lst_of_species += self.hasCatalyst
        lst_of_species += self.hasSolvent
        if self.hasBase is not None:
            lst_of_species += self.hasBase
        return lst_of_species

    def get_list_of_reactant(self) -> List[str]:
        return [s.hasUniqueSpecies for s in self.hasReactant]

    def get_list_of_reactant_and_catalyst(self) -> List[str]:
        species = self.hasReactant + self.hasCatalyst if self.hasCatalyst is not None else self.hasReactant
        return [s.hasUniqueSpecies for s in species]

    def get_list_of_solvent(self) -> List[str]:
        if self.hasSolvent is None:
            return []
        return [s.hasUniqueSpecies for s in self.hasSolvent]

    def get_list_of_product(self) -> List[str]:
        return [s.hasUniqueSpecies for s in self.hasProduct]

    def get_list_of_base(self) -> List[str]:
        if self.hasBase is None:
            return []
        return [s.hasUniqueSpecies for s in self.hasBase]


class ReactionCondition(BaseOntology):
    objPropWithExp: List[str]
    hasValue: OM_Measure
    positionalID: Optional[str] = None
    translateToParameterSetting: Optional[str] # NOTE this doesn't belong to ontology, we just put it here for testing purpose
    # instead of the actual class, str is used to host the instance IRI of OntoReaction:InputChemical for simplicity
    # StoichiometryRatio indicatesMultiplicityOf InputChemical
    indicatesMultiplicityOf: Optional[str] = None
    # instead of the actual class, str is used to host the instance IRI of OntoReaction:InputChemical for simplicity
    # ReactionScale indicatesUsageOf InputChemical
    indicatesUsageOf: Optional[str] = None

    @pydantic.root_validator
    @classmethod
    def input_chemical_validation(cls, values):
        if values.get('clz') == ONTOREACTION_STOICHIOMETRYRATIO:
            if values.get('indicatesMultiplicityOf') == None:
                raise Exception(
                    'StoichiometryRatio <%s> is not indicatesMultiplicityOf any InputChemical, received values: %s.' % (values.get('instance_iri'), str(values))
                )
        elif values.get('clz') == ONTOREACTION_REACTIONSCALE:
            if values.get('indicatesUsageOf') == None:
                raise Exception(
                    'ReactionScale <%s> is not indicatesUsageOf any InputChemical, received values: %s.' % (values.get('instance_iri'), str(values))
                )
        return values

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # IRI-ise the IRI of ReactionCondition instance to be used by rdflib package
        con_iri = URIRef(self.instance_iri)

        # <reactionConditionIRI> <rdf:type> <clz> .
        g.add((con_iri, RDF.type, URIRef(self.clz)))

        # Add below triples following units of measure practices:
        # <reactionConditionIRI> <om:hasValue> <measureIRI> .
        g.add((con_iri, URIRef(OM_HASVALUE), URIRef(self.hasValue.instance_iri)))

        # Add triples for units of measure
        g = self.hasValue.create_instance_for_kg(g)

        # Only add positionalID if it exists
        if self.positionalID is not None:
            g.add((con_iri, URIRef(ONTODOE_POSITIONALID), Literal(self.positionalID)))
        # Also add indicatesMultiplicityOf/indicatesUsageOf if it's a OntoReaction:StoichiometryRatio/OntoReaction:ReactionScale
        if self.indicatesMultiplicityOf is not None:
            g.add((con_iri, URIRef(ONTOREACTION_INDICATESMULTIPLICITYOF), URIRef(self.indicatesMultiplicityOf)))
        if self.indicatesUsageOf is not None:
            g.add((con_iri, URIRef(ONTOREACTION_INDICATESUSAGEOF), URIRef(self.indicatesUsageOf)))

        return g

# TODO [future work] a design choice to be made: are below specific dataclass useful?
# @dataclass
# class ResidenceTime(ReactionCondition):
#     pass

# @dataclass
# class ReactionTemperature(ReactionCondition):
#     pass

# @dataclass
# class ReactionPressure(ReactionCondition):
#     pass

# @dataclass
# class StoichiometryRatio(ReactionCondition):
#     indicatesMultiplicityOf: str # indicatesMultiplicityOf: InputChemical

# @dataclass
# class ReactionScale(ReactionCondition):
#     indicatesUsageOf: str # indicatesUsageOf: InputChemical

class PerformanceIndicator(BaseOntology):
    rxn_exp_iri: str
    objPropWithExp: List[str]
    hasValue: Optional[OM_Measure]
    positionalID: Optional[int] = None
    yieldLimitingSpecies: Optional[str] = None

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # IRI-ise the IRI of PerformanceIndicator instance to be used by rdflib package
        perf_iri = URIRef(self.instance_iri)

        # <performanceIndicatorIRI> <rdf:type> <clz> .
        g.add((perf_iri, RDF.type, URIRef(self.clz)))

        # <rxn_exp_iri> <objPropWithExp> <performanceIndicatorIRI> .
        for obj in self.objPropWithExp:
            g.add((URIRef(self.rxn_exp_iri), URIRef(obj), perf_iri))

        # Only add OM triples if it exists
        if self.hasValue is not None:
            # Add below triples following units of measure practices:
            # <performanceIndicatorIRI> <om:hasValue> <measureIRI> .
            g.add((perf_iri, URIRef(OM_HASVALUE), URIRef(self.hasValue.instance_iri)))

            # Add triples for units of measure
            g = self.hasValue.create_instance_for_kg(g)

        # Only add positionalID if it exists
        if self.positionalID is not None:
            g.add((perf_iri, URIRef(ONTODOE_POSITIONALID), Literal(self.positionalID)))

        # Only add yieldLimitingSpecies if it exists
        if self.yieldLimitingSpecies is not None:
            g.add((perf_iri, URIRef(ONTOREACTION_YIELDLIMITINGSPECIES), URIRef(self.yieldLimitingSpecies)))

        return g

# TODO [future work] same design choice: are below specific dataclass useful?
# @dataclass
# class EnvironmentalFactor(PerformanceIndicator):
#     pass

# @dataclass
# class Yield(PerformanceIndicator):
#     pass

# @dataclass
# class EcoScore(PerformanceIndicator):
#     pass

# @dataclass
# class Conversion(PerformanceIndicator):
#     pass

# @dataclass
# class SpaceTimeYield(PerformanceIndicator):
#     pass

# @dataclass
# class RunMaterialCost(PerformanceIndicator):
#     pass

class ReactionExperiment(BaseOntology):
    hasReactionCondition: Optional[List[ReactionCondition]] = None
    hasPerformanceIndicator: Optional[List[PerformanceIndicator]] = None
    hasInputChemical: Optional[List[InputChemical]] = None
    hasOutputChemical: Optional[List[OutputChemical]] = None
    isAssignedTo: Optional[str] # NOTE here it should be pointing to OntoVapourtec:VapourtecR4Reactor, but we put str to simplify the implementation
    clz: str = ONTOREACTION_REACTIONEXPERIMENT
    isOccurenceOf: Optional[ChemicalReaction] = None

    @pydantic.root_validator
    @classmethod
    def if_exp_assigned(cls, values):
        if 'namespace_for_init' not in values: # means we are not creating a new reaction experiment that yet to be uploaded to the KG, i.e. we are pulling existing data from the KG
            if values.get('isAssignedTo') == None and values.get('hasOutputChemical') != None:
                raise Exception(
                    'The reaction experiment <%s> should already be assigned and conducted as it hasOutputChemical: %s' % (values.get('instance_iri'), str(values.get('hasOutputChemical')))
                )
        return values

    def get_reference_input_chemical(self) -> Optional[InputChemical]:
        if self.hasInputChemical is None:
            return None
        reaction_scale = self.get_reaction_scale()
        for input_chemical in self.hasInputChemical:
            if input_chemical.instance_iri == reaction_scale.indicatesUsageOf:
                return input_chemical
        return None

    def get_reaction_scale(self, positionalID: str=None) -> Optional[ReactionCondition]:
        if positionalID is None:
            reaction_scale = self.get_reaction_condition(ONTOREACTION_REACTIONSCALE)
            if reaction_scale is None:
                # relax the requirement for the reaction scale by allowing other positionalID
                # as the agent might not have the positionalID information
                # although in theory it should know...
                reaction_scale_list = [con for con in self.hasReactionCondition if con.clz == ONTOREACTION_REACTIONSCALE]
                if not bool(reaction_scale_list):
                    raise Exception(f"No reaction scale are identified for reaction experiment: {str(self.dict())}")
                if len(reaction_scale_list) > 1:
                    raise Exception(f"More than one reaction scale {[rs.instance_iri for rs in reaction_scale_list]} are identified for reaction experiment: {str(self.dict())}")
                reaction_scale = reaction_scale_list[0]
            return reaction_scale
        else:
            return self.get_reaction_condition(ONTOREACTION_REACTIONSCALE, positionalID)

    def get_stoichiometry_ratio_of_input_chemical(self, input_chemical: InputChemical) -> float:
        for reaction_condition in self.hasReactionCondition:
            if reaction_condition.clz == ONTOREACTION_STOICHIOMETRYRATIO and reaction_condition.indicatesMultiplicityOf == input_chemical.instance_iri:
                return reaction_condition.hasValue.hasNumericalValue

    def calculate_volumetric_flow_rate_for_input_chemical(
        self,
        total_flowrate_unit: str,
        total_flowrate_value: float,
        all_solute: List[str]
    ) -> Dict[str, OM_VolumetricFlowRate]:
        """Calculate the volumetric flow rate for a given input chemical."""
        dct_flow_ratio = self.compute_flow_ratio(all_solute)
        sum_flow_ratio = sum(dct_flow_ratio.values())

        return {input_chem.instance_iri:OM_VolumetricFlowRate(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(input_chem.instance_iri),
            hasValue=OM_Measure(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(input_chem.instance_iri),
                # NOTE the computed volumetric flow rate is in the unit of that of total_flowrate
                hasUnit=total_flowrate_unit,
                # NOTE the computed volumetric flow rate is rounded to 3 decimal places
                # this is to make it consistent with the FlowCommander
                # TODO [next iteration] store "3" as a constant somewhere with other decimal places of FlowCommander
                hasNumericalValue=round(dct_flow_ratio[input_chem.instance_iri] / sum_flow_ratio * total_flowrate_value, 3),
            )
        ) for input_chem in self.hasInputChemical}

    def compute_flow_ratio(self, all_solute: list) -> Dict[str, float]:
        dct_input_chem_solute = self.identify_solute_in_input_chemical(all_solute)
        ref_input_chemical = self.get_reference_input_chemical()
        conc_ref_chem = ref_input_chemical.get_concentration_of_species(dct_input_chem_solute[ref_input_chemical.instance_iri])
        return {input_chem.instance_iri:self.get_stoichiometry_ratio_of_input_chemical(
            input_chem) * conc_ref_chem.hasValue.numericalValue / input_chem.get_concentration_of_species(
                dct_input_chem_solute[input_chem.instance_iri]).convert_to_unit(
                    conc_ref_chem.hasValue.hasUnitOfMeasure).hasValue.numericalValue for input_chem in self.hasInputChemical}

    def identify_solute_in_input_chemical(self, all_solute: list) -> Dict[str, str]:
        return {input_chem.instance_iri:input_chem.identify_solute_in_list(all_solute) for input_chem in self.hasInputChemical}

    def get_reaction_condition(self, clz: str, positional_id: Optional[int]=None) -> Optional[ReactionCondition]:
        if self.hasReactionCondition is None: return None
        lst_rxn_cond = [rc for rc in self.hasReactionCondition if rc.clz == clz and rc.positionalID == positional_id]
        if len(lst_rxn_cond) > 1:
            raise Exception("ReactionCondition with rdf:type <%s> and positionalID <%s> is not uniquely identified: %s" % (
                clz, str(positional_id), str(lst_rxn_cond)
            ))
        return lst_rxn_cond[0] if len(lst_rxn_cond) == 1 else None

    def get_performance_indicator(self, clz: str, positional_id: Optional[int]=None) -> Optional[PerformanceIndicator]:
        if self.hasPerformanceIndicator is None: return None
        lst_perf_ind = [pi for pi in self.hasPerformanceIndicator if pi.clz == clz and pi.positionalID == positional_id]
        if len(lst_perf_ind) > 1:
            raise Exception("PerformanceIndicator with rdf:type <%s> and positionalID <%s> is not uniquely identified: %s" % (
                clz, str(positional_id), str(lst_perf_ind)
            ))
        return lst_perf_ind[0] if len(lst_perf_ind) == 1 else None

    def create_instance_for_kg(self, g: Graph) -> Graph:
        """This method creates an instance of ReactionExperiment for KG. NOTE that this method should be called when suggesting the new reaction experiment, and before its execution."""
        # check if information is correct and complete
        if self.hasReactionCondition is None:
            raise Exception(f"None of ReactionCondition is set up for {self.instance_iri}")

        if self.hasPerformanceIndicator is not None:
            raise Exception(f"PerformanceIndicator is already set for {self.instance_iri}: {self.hasPerformanceIndicator}")

        if self.hasInputChemical is None:
            raise Exception(f"None of InputChemical is set up for {self.instance_iri}")

        if self.hasOutputChemical is not None:
            raise Exception(f"OutputChemical is already set for {self.instance_iri}: {self.hasOutputChemical}")

        if self.isOccurenceOf is None:
            raise Exception(f"OntoReaction:ChemicalReaction is not set up for {self.instance_iri}")

        if self.isAssignedTo is not None:
            raise Exception(f"OntoVapourtec:VapourtecR4Reactor is already assigned for {self.instance_iri}: {self.isAssignedTo}")

        # Add below triples:
        # <reactionExperimentIRI> <rdf:type> <OntoReaction:ReactionExperiment> .
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(ONTOREACTION_REACTIONEXPERIMENT)))

        # <reactionExperimentIRI> <OntoReaction:hasReactionCondition> <reactionConditionIRI> .
        for con in self.hasReactionCondition:
            # Attach the ReactionCondition instance to the OntoReaction:ReactionExperiment instance
            for rela in con.objPropWithExp:
                g.add((URIRef(self.instance_iri), URIRef(rela), URIRef(con.instance_iri)))

            # Following unit of measure practices, add <reactionConditionIRI> <om:hasPhenomenon> <reactionVariationIRI> .
            g.add((URIRef(con.instance_iri), URIRef(OM_HASPHENOMENON), URIRef(self.instance_iri)))

            # Add all other triples of the ReactionCondition instance
            g = con.create_instance_for_kg(g)

        # <reactionExperimentIRI> <OntoReaction:hasInputChemical> <inputChemicalIRI> .
        # NOTE that the InputChemical instance should exist in KG already before suggesting this experiment
        # NOTE therefore, here we only add the connection between the ReactionExperiment and InputChemical instances
        for input_chemical in self.hasInputChemical:
            g.add((URIRef(self.instance_iri), URIRef(ONTOREACTION_HASINPUTCHEMICAL), URIRef(input_chemical.instance_iri)))

        # <reactionExperimentIRI> <OntoReaction:isOccurenceOf> <OntoReaction:ChemicalReaction> .
        g.add((URIRef(self.instance_iri), URIRef(ONTOREACTION_ISOCCURENCEOF), URIRef(self.isOccurenceOf.instance_iri)))

        return g

class ReactionVariation(ReactionExperiment):
    isVariationOf: Optional[ReactionExperiment] = None
    clz: str = ONTOREACTION_REACTIONVARIATION

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # check if information is complete
        if self.isVariationOf is None:
            raise Exception(
                "An instance of ReactionExperiment should be provided before creating ReactionVariation instance for KG."
            )
        if self.hasReactionCondition is None:
            raise Exception(
                "At least one instance of ReactionCondition should be provided before creating ReactionVariation instance for KG."
            )
        if self.hasInputChemical is None:
            raise Exception(
                "At least one instance of InputChemical should be provided before creating ReactionVariation instance for KG."
            )

        # IRI-ise the ReactionExperiment instance to be used by rdflib package
        rxn_iri = URIRef(self.isVariationOf.instance_iri)
        rxnvar_iri = URIRef(self.instance_iri)

        # Add below triples:
        # <reactionVariationIRI> <rdf:type> <OntoReaction:ReactionVariation> .
        # <reactionVariationIRI> <OntoReaction:isVariationOf> <reactionExperimentIRI> .
        g.add((rxnvar_iri, RDF.type, URIRef(ONTOREACTION_REACTIONVARIATION)))
        g.add((rxnvar_iri, URIRef(ONTOREACTION_ISVARIATIONOF), rxn_iri))

        for con in self.hasReactionCondition:
            # Attach the ReactionCondition instance to the OntoReaction:ReactionVariation instance
            # As we are stating the <reactionVariationIRI> <OntoReaction:isVariationOf> <reactionExperimentIRI>
            # we are using the list of the same object properties between the <reactionExperimentIRI> and the instance of ReactionCondition
            for rela in con.objPropWithExp:
                g.add((rxnvar_iri, URIRef(rela), URIRef(con.instance_iri)))
            
            # Following unit of measure practices, add <reactionConditionIRI> <om:hasPhenomenon> <reactionVariationIRI> .
            g.add((URIRef(con.instance_iri), URIRef(OM_HASPHENOMENON), rxnvar_iri))

            # Add all other triples of the ReactionCondition instance
            g = con.create_instance_for_kg(g)
        
        # NOTE PerformanceIndicator should NOT be presented in the new suggested ReactionVariation before its execution
        # NOTE Thus here we omit the creation of such triples in the KG
        # for perf in self.hasPerformanceIndicator:
        #     # Attach the PerformanceIndicator instance to the OntoReaction:ReactionVariation instance
        #     # As we are stating the <reactionVariationIRI> <OntoReaction:isVariationOf> <reactionExperimentIRI>
        #     # we are using the list of the same object properties between the <reactionExperimentIRI> and the instance of PerformanceIndicator
        #     for rela in perf.objPropWithExp:
        #         g.add((rxnvar_iri, URIRef(rela), URIRef(perf.instance_iri)))
            
        #     # Following unit of measure practices, add <performanceIndicatorIRI> <om:hasPhenomenon> <reactionVariationIRI> .
        #     g.add((URIRef(perf.instance_iri), URIRef(OM_HASPHENOMENON), rxnvar_iri))

        #     # Add all other triples of the PerformanceIndicator instance
        #     g = perf.create_instance_for_kg(g)
        
        # InputChemical and OutputChemical should NOT be collected as part of the triples when collecting for derivation outputs
        # But their links with ReactionVariation should be established here, i.e. <hasInputChemical>
        for input_chemical in self.hasInputChemical:
            g.add((rxnvar_iri, URIRef(ONTOREACTION_HASINPUTCHEMICAL), URIRef(input_chemical.instance_iri)))

        return g


#########################################
## Put all update_forward_refs() below ##
#########################################
OntoCAPE_SinglePhase.update_forward_refs()
