import pydantic
from rdflib import Graph, URIRef, RDF, RDFS, OWL, XSD, Literal
from typing import List, Optional

from chemistry_and_robots.data_model.iris import *
from pyderivationagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import BaseOntology
from chemistry_and_robots.data_model.ontoreaction import *

import chemistry_and_robots.data_model.unit_conversion as unit_conv

class Strategy(BaseOntology):
    clz: str = ONTODOE_STRATEGY

    def create_instance_for_kg(self, g: Graph):
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        return g

class TSEMO(Strategy):
    clz: str = ONTODOE_TSEMO
    # this refers to the realisation of TSEMO algorithm in Summit python package
    # below are default value in Summit python package
    # more details, please visit: https://gosummit.readthedocs.io/en/latest/strategies.html#tsemo
    nRetries: int = 10
    nSpectralPoints: int = 1500
    nGenerations: int = 100
    populationSize: int = 100

    def create_instance_for_kg(self, g: Graph):
        g = super().create_instance_for_kg(g)
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_NRETRIES), Literal(self.nRetries)))
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_NSPECTRALPOINTS), Literal(self.nSpectralPoints)))
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_NGENERATIONS), Literal(self.nGenerations)))
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_POPULATIONSIZE), Literal(self.populationSize)))
        return g

class LHS(Strategy):
    clz: str = ONTODOE_LHS
    seed: int
    # TODO [future work] add support for object property <hasCriterion> <OntoDoE:Criterion>

class DesignVariable(BaseOntology):
    clz: str = ONTODOE_DESIGNVARIABLE

    def create_instance_for_kg(self, g: Graph):
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        return g

class ContinuousVariable(DesignVariable):
    clz: str = ONTODOE_CONTINUOUSVARIABLE
    name: str=None # NOTE this is not part of OntoDoE ontology, but it is used for working with Summit python package
    upperLimit: float
    lowerLimit: float
    positionalID: Optional[str]
    refersTo: OM_Quantity

    def create_instance_for_kg(self, g: Graph):
        g = super().create_instance_for_kg(g)
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_UPPERLIMIT), Literal(self.upperLimit)))
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_LOWERLIMIT), Literal(self.lowerLimit)))
        if self.positionalID is not None:
            g.add((URIRef(self.instance_iri), URIRef(ONTODOE_POSITIONALID), Literal(self.positionalID)))

        # <continuous_variable> <OntoDoE:refersTo> <quantity>
        # <quantity> <rdf:type> <QuantityType>
        # <quantity> <OntoDoE:hasUnit> <unit>
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_REFERSTO), URIRef(self.refersTo.instance_iri)))
        g.add((URIRef(self.refersTo.instance_iri), RDF.type, URIRef(self.refersTo.clz)))
        g.add((URIRef(self.refersTo.instance_iri), URIRef(OM_HASUNIT), URIRef(self.refersTo.hasUnit)))

        return g

    @pydantic.root_validator
    @classmethod
    def upper_and_lower_limit(cls, values):
        # validate the upper and lower limit
        if values.get('upperLimit') <= values.get('lowerLimit'):
            raise Exception(
                'ContinuousVariable <%s> has an UpperLimit %s that is smaller then its LowerLimit %s.' 
                % (values.get('instance_iri'), values.get('upperLimit'), values.get('lowerLimit')))
        return values

    @pydantic.root_validator
    @classmethod
    def refers_to_quantity_unit(cls, values):
        # validate the unit exist for the OM:Quantity that refersTo
        if values.get('refersTo').hasUnit is None:
            raise Exception(f"ContinuousVariable {values.get('instance_iri')} refersTo an OM:Quantity {values.get('refersTo').instance_iri} that has no unit.")
        return values

class FixedParameter(BaseOntology):
    clz: str = ONTODOE_FIXEDPARAMETER
    positionalID: Optional[str]
    refersTo: OM_Quantity

    def create_instance_for_kg(self, g: Graph):
        if self.positionalID is not None:
            g.add((URIRef(self.instance_iri), URIRef(ONTODOE_POSITIONALID), Literal(self.positionalID)))

        # <fixed_parameter> <OntoDoE:refersTo> <quantity>
        # <quantity> <rdf:type> <QuantityType>
        # <quantity> <OntoDoE:hasUnit> <unit>
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_REFERSTO), URIRef(self.refersTo.instance_iri)))
        g.add((URIRef(self.refersTo.instance_iri), RDF.type, URIRef(self.refersTo.clz)))
        g.add((URIRef(self.refersTo.instance_iri), URIRef(OM_HASVALUE), URIRef(self.refersTo.hasValue.instance_iri)))
        g = self.refersTo.hasValue.create_instance_for_kg(g)

        return g

class CategoricalVariable(DesignVariable):
    clz: str = ONTODOE_CATEGORICALVARIABLE
    pass

class Domain(BaseOntology):
    clz: str = ONTODOE_DOMAIN
    hasDesignVariable: List[DesignVariable]
    hasFixedParameter: Optional[List[FixedParameter]]

    def create_instance_for_kg(self, g: Graph):
        # create instance for Domain
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))

        # create instance for each DesignVariable
        for design_variable in self.hasDesignVariable:
            design_variable.create_instance_for_kg(g)
            g.add((URIRef(self.instance_iri), URIRef(ONTODOE_HASDESIGNVARIABLE), URIRef(design_variable.instance_iri)))

        # create instance for each FixedParameter
        if self.hasFixedParameter is not None:
            for fixed_parameter in self.hasFixedParameter:
                fixed_parameter.create_instance_for_kg(g)
                g.add((URIRef(self.instance_iri), URIRef(ONTODOE_HASFIXEDPARAMETER), URIRef(fixed_parameter.instance_iri)))

        return g

    def filter_reaction_experiment_as_beliefs(self, rxn_exp_list: List[ReactionExperiment]) -> List[ReactionExperiment]:
        # return empty list if no reaction experiment is provided
        if not bool(rxn_exp_list):
            return []

        filtered_rxn_exp_list = []
        for rxn_exp in rxn_exp_list:
            _skip = False
            for var in self.hasDesignVariable:
                if isinstance(var, ContinuousVariable):
                    _con = rxn_exp.get_reaction_condition(var.refersTo.clz, var.positionalID)
                    if _con is None:
                        _skip = True
                        break
                    _dq = unit_conv.DimensionalQuantity(
                        hasUnit=_con.hasValue.hasUnit,
                        hasNumericalValue=_con.hasValue.hasNumericalValue,
                    ).convert_to(var.refersTo.hasUnit)
                    if not (var.lowerLimit <= _dq.hasNumericalValue <= var.upperLimit):
                        # the reaction experiment does not satisfy the domain, so skip it
                        _skip = True
                        break
                elif isinstance(var, CategoricalVariable):
                # TODO [future work]: implement
                    raise NotImplementedError(f"Design variable type {type(var)} is not implemented yet.")
                else:
                    raise NotImplementedError(f"Design variable type {type(var)} is not implemented yet.")

            # check for fixed parameters if the same, only if the reaction experiment is not skipped
            if not _skip and self.hasFixedParameter is not None:
                # all fixed parameters must be the same
                for fixed in self.hasFixedParameter:
                    _con_fixed = rxn_exp.get_reaction_condition(fixed.refersTo.clz, fixed.positionalID)
                    if _con_fixed is None:
                        _skip = True
                        break
                    _dq_fixed = unit_conv.DimensionalQuantity(
                        hasUnit=_con_fixed.hasValue.hasUnit,
                        hasNumericalValue=_con_fixed.hasValue.hasNumericalValue,
                    ).convert_to(fixed.refersTo.hasValue.hasUnit)
                    if not _dq_fixed.hasNumericalValue == fixed.refersTo.hasValue.hasNumericalValue:
                        # the reaction experiment does not satisfy the domain, so skip it
                        _skip = True
                        break

            if not _skip:
                filtered_rxn_exp_list.append(rxn_exp)

        return filtered_rxn_exp_list

class SystemResponse(BaseOntology):
    clz: str = ONTODOE_SYSTEMRESPONSE
    name: str=None # NOTE this is not part of OntoDoE ontology, but it is used for working with Summit python package
    maximise: bool
    positionalID: Optional[str]
    # instead of the actual class, str is used to host the concept IRI of om:Quantity for simplicity
    refersTo: str

    def create_instance_for_kg(self, g: Graph):
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_MAXIMISE), Literal(self.maximise)))
        if self.positionalID is not None:
            g.add((URIRef(self.instance_iri), URIRef(ONTODOE_POSITIONALID), Literal(self.positionalID)))
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_REFERSTO), URIRef(self.refersTo)))
        return g

class HistoricalData(BaseOntology):
    clz: str = ONTODOE_HISTORICALDATA
    refersTo: Optional[List[ReactionExperiment]]
    numOfNewExp: int = 1

    def create_instance_for_kg(self, g: Graph):
        # create instance for HistoricalData
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))

        # only add connection if previous data is available
        if bool(self.refersTo): # if not None and not empty list
            # add connection to each ReactionExperiment
            # NOTE here we don't collect triples for each ReactionExperiment, we only make the connection
            for reaction_experiment in self.refersTo:
                g.add((URIRef(self.instance_iri), URIRef(ONTODOE_REFERSTO), URIRef(reaction_experiment.instance_iri)))

        # add number of new experiments
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_NUMOFNEWEXP), Literal(self.numOfNewExp)))

        return g

class DesignOfExperiment(BaseOntology):
    clz: str = ONTODOE_DESIGNOFEXPERIMENT
    usesStrategy: Strategy
    hasDomain: Domain
    hasSystemResponse: List[SystemResponse]
    utilisesHistoricalData: HistoricalData
    proposesNewExperiment: Optional[ReactionExperiment]
    designsChemicalReaction: str # NOTE this should be pointing to OntoCAPE:ChemicalReaction instance, here simplified

    def create_instance_for_kg(self, g: Graph):
        # create an instance of DesignOfExperiment
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))

        # add the strategy
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_USESSTRATEGY), URIRef(self.usesStrategy.instance_iri)))
        g = self.usesStrategy.create_instance_for_kg(g)

        # add the domain
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_HASDOMAIN), URIRef(self.hasDomain.instance_iri)))
        g = self.hasDomain.create_instance_for_kg(g)

        # add the system response
        for system_response in self.hasSystemResponse:
            g.add((URIRef(self.instance_iri), URIRef(ONTODOE_HASSYSTEMRESPONSE), URIRef(system_response.instance_iri)))
            g = system_response.create_instance_for_kg(g)

        # add the historical data
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_UTILISESHISTORICALDATA), URIRef(self.utilisesHistoricalData.instance_iri)))
        g = self.utilisesHistoricalData.create_instance_for_kg(g)

        # designsChemicalReaction
        g.add((URIRef(self.instance_iri), URIRef(ONTODOE_DESIGNSCHEMICALREACTION), URIRef(self.designsChemicalReaction)))

        # NOTE the proposed new experiment is not added here, as this method should be called when the new experiment is not yet suggested

        return g
