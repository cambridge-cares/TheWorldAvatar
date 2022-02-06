from rdflib import Graph, Literal, URIRef
from rdflib.namespace import RDF

import pydantic
from typing import Any, Optional, List, Dict

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

class InstanceIRIInitialisationError(Exception):
    def __init__(self, *args: object) -> None:
        super().__init__(*args)

class BaseOntology(pydantic.BaseModel):
    instance_iri: str = None
    clz: str = None
    namespace_for_init: str = None

    def __init__(__pydantic_self__, **data: Any) -> None:
        if data['instance_iri'] == INSTANCE_IRI_TO_BE_INITIALISED:
            if data['namespace_for_init'] is None:
                raise InstanceIRIInitialisationError(f"A namespace should be provided for initialising a/an {data['clz'] if 'clz' in data else __pydantic_self__.__class__.__fields__['clz'].default} instance.")
            else:
                if 'clz' not in data:
                    data['instance_iri'] = initialiseInstanceIRI(data['namespace_for_init'], __pydantic_self__.__class__.__fields__['clz'].default)
                else:
                    data['instance_iri'] = initialiseInstanceIRI(data['namespace_for_init'], data['clz'])
        super().__init__(**data)

class InputChemical(BaseOntology):
    pass

class OutputChemical(BaseOntology):
    pass

class OM_Measure(BaseOntology):
    clz: str = OM_MEASURE
    # instead of the actual class, str is used to host the concept IRI of om:Unit for simplicity
    hasUnit: str
    hasNumericalValue: float

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # IRI-ise the IRI of OM:Measure instance to be used by rdflib package
        measure_iri = URIRef(self.instance_iri)

        # Add below triples following units of measure practices:
        # <measureIRI> <rdf:type> <om:Measure> .
        # <measureIRI> <om:hasUnit> <unit> .
        # <measureIRI> <om:hasNumericalValue> <val> .
        g.add((measure_iri, RDF.type, URIRef(OM_MEASURE)))
        g.add((measure_iri, URIRef(OM_HASUNIT), URIRef(self.hasUnit)))
        g.add((measure_iri, URIRef(OM_HASNUMERICALVALUE), Literal(self.hasNumericalValue)))
        
        return g


class ReactionCondition(BaseOntology):
    objPropWithExp: List[str]
    hasValue: OM_Measure
    positionalID: Optional[int] = None
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
    clz: str = ONTORXN_REACTIONEXPERIMENT

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

        return g
