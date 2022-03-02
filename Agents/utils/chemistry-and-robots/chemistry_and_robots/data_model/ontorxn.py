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

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import *

# TODO add below IRIs to pyasyncagent.data_model.iris
ONTOCAPE_SCALARVALUE = ONTOCAPE_SYSTEM + 'ScalarValue'
ONTOCAPE_HASVALUE = ONTOCAPE_SYSTEM + 'hasValue'
ONTOCAPE_LIQUID = ONTOCAPE_PHASESYSTEM + 'liquid'
ONTOCAPE_HASUNITOFMEASURE = ONTOCAPE_SYSTEM + 'hasUnitOfMeasure'
ONTOCAPE_NUMERICALVALUE = ONTOCAPE_SYSTEM + 'numericalValue'
ONTORXN_ISASSIGNEDTO = ONTORXN + 'isAssignedTo'
ONTORXN_HASEQUIPMENTSETTINGS = ONTORXN + 'hasEquipmentSettings'
ONTOKIN_SPECIES = ONTOKIN + 'Species'
ONTOKIN_REACTANT = ONTOKIN + 'Reactant'
ONTOCAPE_HASREACTANT = ONTOCAPE_REACTIONMECHANISM + 'hasReactant'
ONTOCAPE_HASPRODUCT = ONTOCAPE_REACTIONMECHANISM + 'hasProduct'
ONTOSPECIES_HASUNIQUESPECIES = ONTOSPECIES + 'hasUniqueSpecies'
# TODO delete ONTORXN_CONDUCTEDIN from pyasyncagent.data_model.iris and OntoRxn TBox

# NOTE only classes/relationships that are actively used in OntoRxn are presented here for ALL OntoCAPE related concepts in this script
class OntoCAPE_ScalarValue(BaseOntology):
    clz: str = ONTOCAPE_SCALARVALUE
    # NOTE here instead of the actual class, str is used in hasUnitOfMeasure to host the concept IRI of om:Unit for simplicity
    # this is in line with the practice of class OM_Measure(BaseOntology)
    hasUnitOfMeasure: str
    numericalValue: float

class OntoCAPE_PhaseComponentConcentration(BaseOntology):
    clz: str = ONTOCAPE_PHASECOMPONENTCONCENTRATION
    hasValue: OntoCAPE_ScalarValue

class OntoCAPE_VolumeBasedConcentration(OntoCAPE_PhaseComponentConcentration):
    clz: str = ONTOCAPE_VOLUMEBASEDCONCENTRATION

class OntoCAPE_Molarity(OntoCAPE_VolumeBasedConcentration):
    clz: str = ONTOCAPE_MOLARITY

class OntoCAPE_Composition(BaseOntology):
    clz: str = ONTOCAPE_COMPOSITION
    comprisesDirectly: List[OntoCAPE_PhaseComponentConcentration]

class OntoCAPE_PhaseComponent(BaseOntology):
    clz: str = ONTOCAPE_PHASECOMPONENT
    hasProperty: OntoCAPE_PhaseComponentConcentration
    representsOccurenceOf: str # NOTE here it should be pointing to OntoCAPE_ChemicalSpecie, but we simplified to use str for its IRI

class OntoCAPE_StateOfAggregation(BaseOntology):
    clz: str = ONTOCAPE_STATEOFAGGREGATION

OntoCAPE_liquid = OntoCAPE_StateOfAggregation(instance_iri=ONTOCAPE_LIQUID)

class OntoCAPE_SinglePhase(BaseOntology):
    clz: str = ONTOCAPE_SINGLEPHASE
    hasStateOfAggregation: OntoCAPE_StateOfAggregation
    isComposedOfSubsystem: List[OntoCAPE_PhaseComponent]
    has_composition: OntoCAPE_Composition
    representsThermodynamicBehaviorOf: Union[str, OntoCAPE_Material] # NOTE here str is provided as an optional as it seems impossible to circular reference at instance level
    # TODO assess if has_physical_context is needed

    def _exclude_keys_for_compare_(self, *keys_to_exclude) -> Dict[str, Any]:
        return super()._exclude_keys_for_compare_('representsThermodynamicBehaviorOf', *keys_to_exclude)

class OntoCAPE_Material(BaseOntology):
    thermodynamicBehaviour: OntoCAPE_SinglePhase
    clz: str = ONTOCAPE_MATERIAL

class InputChemical(OntoCAPE_Material):
    clz: str = ONTORXN_INPUTCHEMICAL

class OutputChemical(OntoCAPE_Material):
    clz: str = ONTORXN_OUTPUTCHEMICAL

class OntoKin_Species(BaseOntology):
    clz: str = ONTOKIN_SPECIES
    hasUniqueSpecies: str # NOTE here we simplify the implementation by using str instead of the actual OntoSpecies:Species

class OntoKin_Reactant(OntoKin_Species):
    clz: str = ONTOKIN_REACTANT

class OntoKin_Product(OntoKin_Species):
    clz: str = ONTOKIN_PRODUCT

class Catalyst(OntoKin_Species):
    clz: str = ONTORXN_CATALYST

class Solvent(OntoKin_Species):
    clz: str = ONTORXN_SOLVENT

class TargetProduct(OntoKin_Product):
    clz: str = ONTORXN_TARGETPRODUCT

class Impurity(OntoKin_Product):
    clz: str = ONTORXN_IMPURITY

class OntoCAPE_ChemicalReaction(BaseOntology):
    clz: str = ONTOCAPE_CHEMICALREACTION
    hasReactant: List[OntoKin_Species]
    hasProduct: List[OntoKin_Species]
    hasCatalyst: Optional[List[OntoKin_Species]]
    hasSolvent: Optional[List[OntoKin_Species]]

class ReactionCondition(BaseOntology):
    objPropWithExp: List[str]
    hasValue: OM_Measure
    positionalID: Optional[int] = None
    translateToParameterSetting: Optional[str] # NOTE here we put str to simplify the implementation, should be ontolab.ParameterSetting
    # instead of the actual class, str is used to host the instance IRI of OntoRxn:InputChemical for simplicity
    # StoichiometryRatio indicatesMultiplicityOf InputChemical
    indicatesMultiplicityOf: Optional[str] = None
    # instead of the actual class, str is used to host the instance IRI of OntoRxn:InputChemical for simplicity
    # ReactionScale indicateUsageOf InputChemical
    indicateUsageOf: Optional[str] = None

    @pydantic.root_validator
    @classmethod
    def input_chemical_validation(cls, values):
        if values.get('clz') == ONTORXN_STOICHIOMETRYRATIO:
            if values.get('indicatesMultiplicityOf') == None:
                raise Exception(
                    'StoichiometryRatio <%s> is not indicatesMultiplicityOf any InputChemical.' % (values.get('instance_iri'))
                )
        elif values.get('clz') == ONTORXN_REACTIONSCALE:
            if values.get('indicateUsageOf') == None:
                raise Exception(
                    'ReactionScale <%s> is not indicateUsageOf any InputChemical.' % (values.get('instance_iri'))
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
        # Also add indicatesMultiplicityOf/indicatesUsageOf if it's a OntoRxn:StoichiometryRatio/OntoRxn:ReactionScale
        if self.indicatesMultiplicityOf is not None:
            g.add((con_iri, URIRef(ONTORXN_INDICATESMULTIPLICITYOF), URIRef(self.indicatesMultiplicityOf)))
        if self.indicateUsageOf is not None:
            g.add((con_iri, URIRef(ONTORXN_INDICATESUSAGEOF), URIRef(self.indicateUsageOf)))
        
        return g

# TODO a design choice to be made: are below specific dataclass useful?
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
#     indicateUsageOf: str # indicateUsageOf: InputChemical

class PerformanceIndicator(BaseOntology):
    objPropWithExp: List[str]
    hasValue: Optional[OM_Measure]
    positionalID: Optional[int] = None

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # IRI-ise the IRI of PerformanceIndicator instance to be used by rdflib package
        perf_iri = URIRef(self.instance_iri)

        # <performanceIndicatorIRI> <rdf:type> <clz> .
        g.add((perf_iri, RDF.type, URIRef(self.clz)))
        
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

        return g

# TODO same design choice: are below specific dataclass useful?
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
    clz: str = ONTORXN_REACTIONEXPERIMENT
    isOccurenceOf: Optional[OntoCAPE_ChemicalReaction] = None

    @pydantic.root_validator
    @classmethod
    def if_exp_assigned(cls, values):
        if 'namespace_for_init' not in values: # means we are not creating a new reaction experiment that yet to be uploaded to the KG, i.e. we are pulling existing data from the KG
            if values.get('isAssignedTo') == None and values.get('hasOutputChemical') != None:
                raise Exception(
                    'The reaction experiment <%s> should already be assigned and conducted as it hasOutputChemical: %s' % (values.get('instance_iri'), str(values.get('hasOutputChemical')))
                )
        return values

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # check if information is complete
        if self.hasReactionCondition is None:
            raise Exception(
                "At least one instance of ReactionCondition should be provided before creating ReactionExperiment instance for KG."
            )
        if self.hasPerformanceIndicator is None:
            raise Exception(
                "At least one instance of PerformanceIndicator should be provided before creating ReactionExperiment instance for KG."
            )

        # IRI-ise the ReactionExperiment instance to be used by rdflib package
        rxnvar_iri = URIRef(self.instance_iri)

        # Add below triples:
        # <reactionExperimentIRI> <rdf:type> <OntoRxn:ReactionExperiment> .
        g.add((rxnvar_iri, RDF.type, URIRef(ONTORXN_REACTIONEXPERIMENT)))
        
        for con in self.hasReactionCondition:
            # Attach the ReactionCondition instance to the OntoRxn:ReactionVariation instance
            # As we are stating the <reactionVariationIRI> <OntoRxn:isVariationOf> <reactionExperimentIRI>
            # we are using the list of the same object properties between the <reactionExperimentIRI> and the instance of ReactionCondition
            for rela in con.objPropWithExp:
                g.add((rxnvar_iri, URIRef(rela), URIRef(con.instance_iri)))
            
            # Following unit of measure practices, add <reactionConditionIRI> <om:hasPhenomenon> <reactionVariationIRI> .
            g.add((URIRef(con.instance_iri), URIRef(OM_HASPHENOMENON), rxnvar_iri))

            # Add all other triples of the ReactionCondition instance
            g = con.create_instance_for_kg(g)
        
        for perf in self.hasPerformanceIndicator:
            # Attach the PerformanceIndicator instance to the OntoRxn:ReactionVariation instance
            # As we are stating the <reactionVariationIRI> <OntoRxn:isVariationOf> <reactionExperimentIRI>
            # we are using the list of the same object properties between the <reactionExperimentIRI> and the instance of PerformanceIndicator
            for rela in perf.objPropWithExp:
                g.add((rxnvar_iri, URIRef(rela), URIRef(perf.instance_iri)))
            
            # Following unit of measure practices, add <performanceIndicatorIRI> <om:hasPhenomenon> <reactionVariationIRI> .
            g.add((URIRef(perf.instance_iri), URIRef(OM_HASPHENOMENON), rxnvar_iri))

            # Add all other triples of the PerformanceIndicator instance
            g = perf.create_instance_for_kg(g)
        
        # TODO add support for creating InputChemical and OutputChemical

        return g

class ReactionVariation(ReactionExperiment):
    isVariationOf: Optional[ReactionExperiment] = None
    clz: str = ONTORXN_REACTIONVARIATION

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
        if self.hasPerformanceIndicator is None:
            raise Exception(
                "At least one instance of PerformanceIndicator should be provided before creating ReactionVariation instance for KG."
            )

        # IRI-ise the ReactionExperiment instance to be used by rdflib package
        rxn_iri = URIRef(self.isVariationOf.instance_iri)
        rxnvar_iri = URIRef(self.instance_iri)

        # Add below triples:
        # <reactionVariationIRI> <rdf:type> <OntoRxn:ReactionVariation> .
        # <reactionVariationIRI> <OntoRxn:isVariationOf> <reactionExperimentIRI> .
        # <reactionExperimentIRI> <OntoRxn:hasVariation> <reactionVariationIRI> .
        g.add((rxnvar_iri, RDF.type, URIRef(ONTORXN_REACTIONVARIATION)))
        g.add((rxnvar_iri, URIRef(ONTORXN_ISVARIATIONOF), rxn_iri))
        g.add((rxn_iri, URIRef(ONTORXN_HASVARIATION), rxnvar_iri))
        
        for con in self.hasReactionCondition:
            # Attach the ReactionCondition instance to the OntoRxn:ReactionVariation instance
            # As we are stating the <reactionVariationIRI> <OntoRxn:isVariationOf> <reactionExperimentIRI>
            # we are using the list of the same object properties between the <reactionExperimentIRI> and the instance of ReactionCondition
            for rela in con.objPropWithExp:
                g.add((rxnvar_iri, URIRef(rela), URIRef(con.instance_iri)))
            
            # Following unit of measure practices, add <reactionConditionIRI> <om:hasPhenomenon> <reactionVariationIRI> .
            g.add((URIRef(con.instance_iri), URIRef(OM_HASPHENOMENON), rxnvar_iri))

            # Add all other triples of the ReactionCondition instance
            g = con.create_instance_for_kg(g)
        
        for perf in self.hasPerformanceIndicator:
            # Attach the PerformanceIndicator instance to the OntoRxn:ReactionVariation instance
            # As we are stating the <reactionVariationIRI> <OntoRxn:isVariationOf> <reactionExperimentIRI>
            # we are using the list of the same object properties between the <reactionExperimentIRI> and the instance of PerformanceIndicator
            for rela in perf.objPropWithExp:
                g.add((rxnvar_iri, URIRef(rela), URIRef(perf.instance_iri)))
            
            # Following unit of measure practices, add <performanceIndicatorIRI> <om:hasPhenomenon> <reactionVariationIRI> .
            g.add((URIRef(perf.instance_iri), URIRef(OM_HASPHENOMENON), rxnvar_iri))

            # Add all other triples of the PerformanceIndicator instance
            g = perf.create_instance_for_kg(g)
        
        # TODO add support for creating InputChemical and OutputChemical
        for input_chemical in self.hasInputChemical:
            g.add((rxnvar_iri, URIRef(ONTORXN_HASINPUTCHEMICAL), URIRef(input_chemical.instance_iri)))

        return g


#########################################
## Put all update_forward_refs() below ##
#########################################
OntoCAPE_SinglePhase.update_forward_refs()
