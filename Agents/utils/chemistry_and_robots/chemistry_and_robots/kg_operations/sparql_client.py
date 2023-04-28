# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
from datetime import datetime
from typing import Tuple
from rdflib import Graph
from rdflib import XSD
from pathlib import Path
import collections
import requests
from requests import status_codes
import uuid
import time
import os

from pyderivationagent.kg_operations import PySparqlClient
from pyderivationagent.data_model import *

from chemistry_and_robots.data_model import *
from chemistry_and_robots.hardware import hplc

from chemistry_and_robots.kg_operations import dict_and_list as dal

from chemistry_and_robots.resources import TBOX_PATH_DICT
from chemistry_and_robots import __version__

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


class ChemistryAndRobotsSparqlClient(PySparqlClient):
    def get_ontology_tbox_version(self, tbox_namespace):
        if not tbox_namespace:
            raise Exception("No TBox namespace provided.")
        if TBOX_PATH_DICT.get(tbox_namespace) is None:
            raise Exception(f"TBox namespace {tbox_namespace} is NOT packaged in chemistry_and_robots=={__version__}.")
        g = Graph().parse(TBOX_PATH_DICT.get(tbox_namespace), format='xml')
        results = g.query(f'SELECT ?v WHERE {{ <{tbox_namespace}> <{OWL_VERSION}> ?v. }}')
        for row in results:
            return str(row['v'])

    def upload_ontology_tbox(self, tbox_namespace, alternative_url=None):
        try:
            query = f'SELECT * WHERE {{ <{tbox_namespace}> <{OWL_VERSION}> ?v. }}'
            res = self.performQuery(query)
        except Exception as ex:
            logger.error("Unable to retrieve TBox version from KG.")
            raise Exception("Unable to retrieve TBox version from KG.") from ex

        # Upload TBox if not already instantiated
        if not res:
            if not alternative_url:
                # Upload TBox from local file by default
                self.uploadOntology(TBOX_PATH_DICT.get(tbox_namespace))
            else:
                # Upload TBox from alternative URL if provided
                temp_fp = '_tmp.owl'
                url_to_upload = alternative_url
                try:
                    try:
                        content = requests.get(url_to_upload)
                        if content.status_code != 200:
                            raise Exception(f'HTTP error code for retrieving owl file: {content.status_code}')
                    except Exception as ex:
                        raise Exception(f"Unable to retrieve {url_to_upload} from TWA server.") from ex
                    with open(temp_fp, 'w') as f:
                        f.write(content.text)
                        self.uploadOntology(temp_fp)
                    os.remove(temp_fp)
                except Exception as ex:
                    raise Exception(f"Unable to initialise knowledge graph with {tbox_namespace} TBox ({url_to_upload}).") from ex

    def get_all_chemical_reaction_iri(self):
        query = f"""SELECT ?chem_rxn WHERE {{ ?chem_rxn a <{ONTOREACTION_CHEMICALREACTION}>. }}"""
        response = self.performQuery(query)
        return [list(res.values())[0] for res in response]

    def get_all_rxn_exp_given_chem_rxn(self, chem_rxn_iri: str):
        chem_rxn_iri = trimIRI(chem_rxn_iri)
        query = f"""SELECT DISTINCT ?rxn_exp
                    WHERE {{
                        ?rxn_exp <{ONTOREACTION_ISVARIATIONOF}>*/<{ONTOREACTION_ISOCCURENCEOF}> <{chem_rxn_iri}>.
                    }}
                """
        response = self.performQuery(query)
        return [list(res.values())[0] for res in response]

    def get_all_rxn_exp_with_target_perfind_given_chem_rxn(self, chem_rxn_iri: str, target_perfind_iri_or_lst: Union[str, list]):
        chem_rxn_iri = trimIRI(chem_rxn_iri)
        if not isinstance(target_perfind_iri_or_lst, list):
            target_perfind_iri_or_lst = [target_perfind_iri_or_lst]
        target_perfind_iri_or_lst = trimIRI(target_perfind_iri_or_lst)
        # NOTE here we force all rxn_exp to have all target_perfind_iri_or_lst
        # NOTE the reaction condition side of selection will be done in other places, e.g. ROGI agent
        _q = ""
        for i in range(len(target_perfind_iri_or_lst)):
            _q = _q + f"""?rxn_exp ?p_{i} ?o_{i}. ?o_{i} a <{target_perfind_iri_or_lst[i]}>. """
        query = f"""SELECT DISTINCT ?rxn_exp
                    WHERE {{
                        ?rxn_exp <{ONTOREACTION_ISVARIATIONOF}>*/<{ONTOREACTION_ISOCCURENCEOF}> <{chem_rxn_iri}>.
                        {_q}
                    }}
                """
        response = self.performQuery(query)
        return [list(res.values())[0] for res in response]

    def collect_triples_for_new_experiment(self, doe: DesignOfExperiment, newExp: List[ReactionExperiment]) -> Graph:
        """
            This method is used to collect the suggested new experiments as triples.
            It first serialises the ReactionVariation/ReactionExperiment instance to rdflib.Graph().
            It then add the triple that connects the OntoDoE:DesignOfExperiment instance and the new created OntoReaction:ReactionExperiment/ReactionVariation instance.

            Arguments:
                doe - instance of dataclass OntoDoE.DesignOfExperiment
                newExp - a list of instance of dataclass OntoReaction.ReactionExperiment
        """
        # (1) first serialise ReactionVariation/ReactionExperiment instance to rdflib.Graph()
        # All information should already be prepared and added to the instance
        # Method create_instance_for_kg will write all information to rdflib Graph on-the-fly
        g = Graph()
        # NOTE although here we loop through the list of OntoReaction:ReactionVariation/ReactionExperiment
        # NOTE in theory, the len(newExp) should be 1 (as we decided to make DoE Agent only suggest 1 experiment per derivation)
        # NOTE the loop is added for the future development
        for exp in newExp:
            g = exp.create_instance_for_kg(g)
            # (2) add triple that connects the OntoDoE:DesignOfExperiment with OntoReaction:ReactionVariation/ReactionExperiment
            g.add((URIRef(doe.instance_iri), URIRef(ONTODOE_PROPOSESNEWEXPERIMENT), URIRef(exp.instance_iri)))

        return g

    def get_doe_instance(self, doe_iri) -> DesignOfExperiment:
        doe_iri = trimIRI(doe_iri)
        query_1 = f"""SELECT ?strategy ?domain ?hist_data ?chem_rxn
                      WHERE {{
                          <{doe_iri}> <{ONTODOE_USESSTRATEGY}> ?strategy;
                                      <{ONTODOE_HASDOMAIN}> ?domain;
                                      <{ONTODOE_UTILISESHISTORICALDATA}> ?hist_data;
                                      <{ONTODOE_DESIGNSCHEMICALREACTION}> ?chem_rxn.
                      }}"""
        response_1 = self.performQuery(query_1)
        if len(response_1) > 1:
            raise Exception("OntoDoE:DesignOfExperiment instance <%s> is associated with multiple entries of OntoDoE:Strategy/Domain/HistoricalData/OntoReaction:ChemicalReaction: %s" % (
                doe_iri, str(response_1)))
        elif len(response_1) < 1:
            raise Exception("OntoDoE:DesignOfExperiment instance <%s> is NOT associated with any entries of OntoDoE:Strategy/Domain/HistoricalData/OntoReaction:ChemicalReaction" % (doe_iri))
        else:
            r = response_1[0]

        query_2 = f"""SELECT ?sys_res WHERE {{ <{doe_iri}> <{ONTODOE_HASSYSTEMRESPONSE}> ?sys_res. }}"""
        response_2 = self.performQuery(query_2)

        doe_instance = DesignOfExperiment(
            instance_iri=doe_iri,
            usesStrategy=self.getDoEStrategy(r['strategy']),
            hasDomain=self.getDoEDomain(r['domain']),
            hasSystemResponse=self.getSystemResponses([list(res.values())[0] for res in response_2]),
            utilisesHistoricalData=self.getDoEHistoricalData(r['hist_data']),
            proposesNewExperiment=self.getNewExperimentFromDoE(doe_iri),
            designsChemicalReaction=r['chem_rxn'],
        )
        return doe_instance

    def getDoEDomain(self, domain_iri: str) -> Domain:
        """
            This method retrieves information given instance iri of OntoDoE:Domain.

            Arguments:
                domain_iri - iri of OntoDoE:Domain instance
        """
        # Delete "<" and ">" around the IRI
        domain_iri = trimIRI(domain_iri)
        domain_instance = Domain(
            instance_iri=domain_iri,
            hasDesignVariable=self.getDesignVariables(domain_iri),
            hasFixedParameter=self.get_fixed_parameters(domain_iri),
        )
        return domain_instance

    def get_fixed_parameters(self, domain_iri: str) -> Optional[List[FixedParameter]]:
        domain_iri = trimIRI(domain_iri)
        query = f"""SELECT ?param ?quantity ?clz ?measure ?unit ?value ?id
                    WHERE {{
                        <{domain_iri}> <{ONTODOE_HASFIXEDPARAMETER}> ?param.
                        ?param <{ONTODOE_REFERSTOQUANTITY}> ?quantity .
                        ?quantity a ?clz; <{OM_HASVALUE}> ?measure.
                        ?measure <{OM_HASUNIT}> ?unit; <{OM_HASNUMERICALVALUE}> ?value.
                        OPTIONAL {{?param <{ONTODOE_POSITIONALID}> ?id . }}
                    }}"""
        response = self.performQuery(query)

        list_param = []
        for res in response:
            list_param.append(FixedParameter(
                instance_iri=res['param'],
                positionalID=res['id'] if 'id' in res else None,
                refersToQuantity=OM_Quantity(
                    instance_iri=res['quantity'],
                    clz=res['clz'],
                    hasValue=OM_Measure(
                        instance_iri=res['measure'],
                        hasUnit=res['unit'],
                        hasNumericalValue=res['value']
                    )
                )
            ))

        return list_param if len(list_param) > 0 else None

    def getDesignVariables(self, domain_iri: str) -> List[DesignVariable]:
        """
            This methods retrieves all the design variables pointed by the iri of the given instance of dataclass OntoDoE.Domain.

            Arguments:
                domain_iri - iri of OntoDoE:Domain instance
        """
        # Delete "<" and ">" around the IRI
        domain_iri = trimIRI(domain_iri)

        # Prepare query string
        query_continuous = f"""SELECT DISTINCT ?var ?quantity ?clz ?unit ?id ?lower ?upper
                WHERE {{
                    <{domain_iri}> <{ONTODOE_HASDESIGNVARIABLE}> ?var .
                    ?var a <{ONTODOE_CONTINUOUSVARIABLE}> .
                    ?var <{ONTODOE_REFERSTOQUANTITY}> ?quantity .
                    ?quantity a ?clz; <{OM_HASUNIT}> ?unit.
                    OPTIONAL {{?var <{ONTODOE_POSITIONALID}> ?id . }}
                    OPTIONAL {{?var <{ONTODOE_LOWERLIMIT}> ?lower . }}
                    OPTIONAL {{?var <{ONTODOE_UPPERLIMIT}> ?upper . }}
                }}"""

        # Perform query
        response = self.performQuery(query_continuous)

        # Construct list of design variables
        list_var = []
        for res in response:
            list_var.append(
                ContinuousVariable(
                    instance_iri=res['var'],
                    name=getShortName(res['var']), # NOTE this is not part of OntoDoE ontology, but it is used for working with Summit python package
                    upperLimit=res['upper'],
                    lowerLimit=res['lower'],
                    positionalID=res['id'] if 'id' in res else None,
                    refersToQuantity=OM_Quantity(
                        instance_iri=res['quantity'],
                        clz=res['clz'],
                        hasUnit=res['unit']
                    )
                )
            )

        # Query for categorical variables
        query_categorical = f"""
            SELECT DISTINCT ?var ?cat ?quantity ?clz ?measure ?unit ?value ?id
            WHERE {{
                <{domain_iri}> <{ONTODOE_HASDESIGNVARIABLE}> ?var .
                ?var a <{ONTODOE_CATEGORICALVARIABLE}>.
                ?var <{ONTODOE_HASLEVEL}> ?cat.
                ?var <{ONTODOE_REFERSTOQUANTITY}> ?quantity .
                ?quantity a ?clz; <{OM_HASVALUE}> ?measure.
                ?measure <{OM_HASUNIT}> ?unit; <{OM_HASNUMERICALVALUE}> ?value.
                OPTIONAL {{?var <{ONTODOE_POSITIONALID}> ?id . }}
            }}
            """
        response = self.performQuery(query_categorical)
        _var = dal.get_unique_values_in_list_of_dict(response, 'var')
        for _v in _var:
            # NOTE we assume that each categorical variable only refers to ONE quantity
            _info_v = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'var', _v) # info for a single categorical variable
            list_var.append(
                CategoricalVariable(
                    instance_iri=_v,
                    name=getShortName(_v), # NOTE this is not part of OntoDoE ontology, but it is used for working with Summit python package
                    hasLevel=dal.get_unique_values_in_list_of_dict(_info_v, 'cat'),
                    refersToQuantity=OM_Quantity(
                        instance_iri=_info_v[0]['quantity'],
                        clz=_info_v[0]['clz'],
                        hasValue=OM_Measure(
                            instance_iri=_info_v[0]['measure'],
                            hasUnit=_info_v[0]['unit'],
                            hasNumericalValue=_info_v[0]['value']
                        )
                    ),
                    positionalID=_info_v[0].get('id')
                )
            )
        return list_var

    def getSystemResponses(self, systemResponse_iris) -> List[SystemResponse]:
        """
            This methods retrieves all the system responses given iris of OntoDoE:SystemResponses.

            Arguments:
                systemResponse_iris - a list of OntoDoE:SystemResponse iri
        """
        # Delete "<" and ">" around the IRI
        systemResponse_iris = trimIRI(systemResponse_iris)
        
        # Add safeguard in case the input systemResponse_iris is not a list
        if not isinstance(systemResponse_iris, list):
            systemResponse_iris = [systemResponse_iris]
        
        list_sys = []
        for sys_ins in systemResponse_iris:
            # Prepare query string
            query = """SELECT DISTINCT ?clz ?id ?maximise \
                    WHERE { \
                    <%s> <%s> ?clz . 
                    OPTIONAL {<%s> <%s> ?id} \
                    OPTIONAL {<%s> <%s> ?maximise} \
                    }""" % (sys_ins, ONTODOE_REFERSTOQUANTITY, sys_ins, ONTODOE_POSITIONALID, sys_ins, ONTODOE_MAXIMISE)
            # Perform query
            response = self.performQuery(query)
            list_sys.append(
                SystemResponse(
                    instance_iri=sys_ins,
                    name=getShortName(sys_ins), # NOTE this is not part of OntoDoE ontology, but it is used for working with Summit python package
                    maximise=response[0]['maximise'],
                    positionalID=response[0]['id'] if 'id' in response[0] else None,
                    refersToQuantity=response[0]['clz']
                )
            )
            
        return list_sys

    def getDoEHistoricalData(self, historicalData_iri: str) -> HistoricalData:
        """
            This method retrieves information given instance iri of OntoDoE:HistoricalData.

            Arguments:
                historicalData_iri - iri of OntoDoE:HistoricalData instance
        """
        # Delete "<" and ">" around the IRI
        historicalData_iri = trimIRI(historicalData_iri)

        query = f"""SELECT DISTINCT ?exp ?numOfNewExp
                WHERE {{
                    OPTIONAL {{<{historicalData_iri}> <{ONTODOE_REFERSTOEXPERIMENT}> ?exp .}}
                    OPTIONAL {{<{historicalData_iri}> <{ONTODOE_NUMOFNEWEXP}> ?numOfNewExp .}}
                }}"""

        response = self.performQuery(query)

        num_ = [res['numOfNewExp'] for res in response if 'numOfNewExp' in res]
        if num_.count(num_[0]) != len(num_):
            raise Exception("There are multiple instances of numOfNewExp associated with OntoDoE:HistoricalData instance <" + historicalData_iri + ">: " + collections.Counter(num_).keys)

        rxnexp_iris = [res['exp'] for res in response if 'exp' in res]

        historicalData_instance = HistoricalData(
            instance_iri=historicalData_iri,
            numOfNewExp=int(num_[0]) if bool(num_) else HistoricalData.__fields__.get('numOfNewExp').default,
            refersToExperiment=self.getReactionExperiment(rxnexp_iris) if bool(rxnexp_iris) else None,
        )

        return historicalData_instance

    def getReactionExperiment(self, rxnexp_iris: Union[str, list]) -> List[ReactionExperiment]:
        """
            This method retrieves information given a list of instance iri of OntoReaction:ReactionExperiment.

            Arguments:
                rxnexp_iris - iri of OntoReaction:ReactionExperiment instance, can be either str of one instance, or a list of instances
        """

        if not bool(rxnexp_iris):
            return []

        if not isinstance(rxnexp_iris, list):
            rxnexp_iris = [rxnexp_iris]

        query = f"""SELECT DISTINCT ?rxn ?rxn_type ?variation_of ?assigned_reactor
                    WHERE {{
                        VALUES ?rxn_iri {{ <{'> <'.join(rxnexp_iris)}> }}
                        VALUES ?rxn_type {{ <{ONTOREACTION_REACTIONEXPERIMENT}> <{ONTOREACTION_REACTIONVARIATION}> }}
                        ?rxn_iri <{ONTOREACTION_ISVARIATIONOF}>? ?rxn.
                        ?rxn a ?rxn_type.
                        OPTIONAL {{ ?rxn <{ONTOREACTION_ISVARIATIONOF}> ?variation_of. }}
                        OPTIONAL {{ ?rxn <{ONTOREACTION_ISASSIGNEDTO}> ?assigned_reactor. }}
                    }}"""
        response_of_all_rxn = self.performQuery(query)
        # Process the response of the query to construct a few dictionaries
        dict_exp = {} # key: reaction_experiment_to_query, value: ReactionExperiment instance
        dict_rxn_types = {ONTOREACTION_REACTIONEXPERIMENT: [], ONTOREACTION_REACTIONVARIATION: []}
        for res in response_of_all_rxn:
            if res['rxn_type'] == ONTOREACTION_REACTIONEXPERIMENT:
                dict_rxn_types[ONTOREACTION_REACTIONEXPERIMENT].append(res['rxn'])
            elif res['rxn_type'] == ONTOREACTION_REACTIONVARIATION:
                dict_rxn_types[ONTOREACTION_REACTIONVARIATION].append(res['rxn'])
        dict_assigned_reactor = {res['rxn']:res['assigned_reactor'] if 'assigned_reactor' in res else None for res in response_of_all_rxn}
        dict_variation_of = {res['rxn']:res['variation_of'] for res in response_of_all_rxn if 'variation_of' in res}
        # Construct a list of all reaction experiment/variations IRIs to query
        rxnexp_iris_to_query = rxnexp_iris.copy()
        rxnexp_iris_to_query.extend([res['variation_of'] for res in response_of_all_rxn if 'variation_of' in res])
        rxnexp_iris_to_query = list(set(rxnexp_iris_to_query))

        # Query the rest of the information
        dict_rxn_condition = self.getExpReactionCondition(rxnexp_iris_to_query)
        dict_perf_indicator = self.getExpPerformanceIndicator(rxnexp_iris_to_query)
        dict_input_chemical = self.get_input_chemical_of_rxn_exp(rxnexp_iris_to_query)
        dict_output_chemical = self.get_output_chemical_of_rxn_exp(rxnexp_iris_to_query)
        dict_chemical_reaction = self.get_chemical_reaction(rxnexp_iris_to_query)

        # Construct a list of ReactionExperiment/ReactionVariation instances to be returned
        list_exp = []
        # First process the reaction experiment instances
        for exp_iri in dict_rxn_types[ONTOREACTION_REACTIONEXPERIMENT]:
            dict_exp[exp_iri] = ReactionExperiment(
                instance_iri=exp_iri,
                hasReactionCondition=dict_rxn_condition[exp_iri],
                hasPerformanceIndicator=dict_perf_indicator[exp_iri],
                hasInputChemical=dict_input_chemical[exp_iri],
                hasOutputChemical=dict_output_chemical[exp_iri],
                isAssignedTo=dict_assigned_reactor[exp_iri],
                isOccurenceOf=dict_chemical_reaction[exp_iri],
            )

        # Then fill in the list to return
        for rxnexp_iri in rxnexp_iris:
            if rxnexp_iri in dict_rxn_types[ONTOREACTION_REACTIONEXPERIMENT]:
                list_exp.append(dict_exp[rxnexp_iri])
            else:
                list_exp.append(
                    ReactionVariation(
                        instance_iri=rxnexp_iri,
                        hasReactionCondition=dict_rxn_condition[rxnexp_iri],
                        hasPerformanceIndicator=dict_perf_indicator[rxnexp_iri],
                        hasInputChemical=dict_input_chemical[rxnexp_iri],
                        hasOutputChemical=dict_output_chemical[rxnexp_iri],
                        isAssignedTo=dict_assigned_reactor[rxnexp_iri],
                        isOccurenceOf=dict_chemical_reaction[rxnexp_iri],
                        isVariationOf=dict_exp[dict_variation_of[rxnexp_iri]],
                    )
                )
        return list_exp

    def get_chemical_reaction(self, rxnexp_iris: Union[str, list]) -> Dict[str, ChemicalReaction]:
        if not isinstance(rxnexp_iris, list):
            rxnexp_iris = [rxnexp_iris]
        rxnexp_iris = trimIRI(rxnexp_iris)
        query_mapping = f"""SELECT DISTINCT ?rxn ?chem_rxn
                            WHERE {{
                                VALUES ?rxn {{ <{'> <'.join(rxnexp_iris)}> }}
                                ?rxn <{ONTOREACTION_ISVARIATIONOF}>?/<{ONTOREACTION_ISOCCURENCEOF}> ?chem_rxn.
                            }}"""
        response_mapping = self.performQuery(query_mapping)
        dict_rxn_chem_mapping = {}
        for r in response_mapping:
            rxn_iri = r['rxn']
            chem_rxn_iri = r['chem_rxn']
            if rxn_iri not in dict_rxn_chem_mapping:
                dict_rxn_chem_mapping[rxn_iri] = chem_rxn_iri
            else:
                raise Exception(f"OntoReaction:ReactionExperiment instance <{rxn_iri}> is found to be occurence of multiple OntoReaction:ChemicalReaction instance: {dal.get_sublist_in_list_of_dict_matching_key_value(response_mapping, 'rxn', rxn_iri)}")

        lst_unique_chem_rxn = dal.get_unique_values_in_list_of_dict(response_mapping, 'chem_rxn')
        query = f"""{PREFIX_RDF}
                    SELECT DISTINCT ?chem_rxn ?reactant ?reac_type ?reac_species ?product ?prod_type ?prod_species
                    ?catalyst ?cata_type ?cata_species ?solvent ?solv_type ?solv_species ?base ?base_type ?base_species
                    ?doe_template
                    WHERE {{
                        VALUES ?reac_type {{<{ONTOKIN_SPECIES}> <{ONTOKIN_REACTANT}>}}.
                        VALUES ?prod_type {{<{ONTOKIN_SPECIES}> <{ONTOKIN_PRODUCT}> <{ONTOREACTION_TARGETPRODUCT}> <{ONTOREACTION_IMPURITY}>}}.
                        VALUES ?chem_rxn {{ <{'> <'.join(lst_unique_chem_rxn)}> }}
                        ?chem_rxn <{ONTOCAPE_HASREACTANT}> ?reactant; <{ONTOCAPE_HASPRODUCT}> ?product.
                        ?reactant rdf:type ?reac_type; <{ONTOSPECIES_HASUNIQUESPECIES}> ?reac_species.
                        ?product rdf:type ?prod_type; <{ONTOSPECIES_HASUNIQUESPECIES}> ?prod_species.
                        OPTIONAL{{
                            VALUES ?cata_type {{<{ONTOKIN_SPECIES}> <{ONTOREACTION_CATALYST}>}}.
                            ?chem_rxn <{ONTOREACTION_HASCATALYST}> ?catalyst.
                            ?catalyst rdf:type ?cata_type; <{ONTOSPECIES_HASUNIQUESPECIES}> ?cata_species.
                        }}
                        OPTIONAL{{
                            VALUES ?solv_type {{<{ONTOKIN_SPECIES}> <{ONTOREACTION_SOLVENT}>}}.
                            ?chem_rxn <{ONTOREACTION_HASSOLVENT}> ?solvent.
                            ?solvent rdf:type ?solv_type; <{ONTOSPECIES_HASUNIQUESPECIES}> ?solv_species.
                        }}
                        OPTIONAL{{
                            VALUES ?base_type {{<{ONTOKIN_SPECIES}> <{ONTOREACTION_BASE}>}}.
                            ?chem_rxn <{ONTOREACTION_HASBASE}> ?base.
                            ?base rdf:type ?base_type; <{ONTOSPECIES_HASUNIQUESPECIES}> ?base_species.
                        }}
                        OPTIONAL{{?chem_rxn <{ONTODOE_HASDOETEMPLATE}> ?doe_template.}}
                    }}"""
        response_of_all_chem_rxn = self.performQuery(query)
        dict_chem_rxn = {} # key: chem_rxn_iri, value: ChemicalReaction

        for iri in lst_unique_chem_rxn:
            response = dal.get_sublist_in_list_of_dict_matching_key_value(response_of_all_chem_rxn, 'chem_rxn', iri)

            chem_rxn = ChemicalReaction(
                instance_iri=iri,
                hasReactant=self.get_ontokin_species_from_chem_rxn(response, 'reactant', 'reac_type', 'reac_species'),
                hasProduct=self.get_ontokin_species_from_chem_rxn(response, 'product', 'prod_type', 'prod_species'),
                hasCatalyst=self.get_ontokin_species_from_chem_rxn(response, 'catalyst', 'cata_type', 'cata_species'),
                hasSolvent=self.get_ontokin_species_from_chem_rxn(response, 'solvent', 'solv_type', 'solv_species'),
                hasBase=self.get_ontokin_species_from_chem_rxn(response, 'base', 'base_type', 'base_species'),
                hasDoETemplate=dal.get_the_unique_value_in_list_of_dict(response, 'doe_template'),
            )

            dict_chem_rxn[iri] = chem_rxn

        dict_exp_chem_rxn = {}
        for exp in rxnexp_iris:
            dict_exp_chem_rxn[exp] = dict_chem_rxn[dict_rxn_chem_mapping[exp]]

        return dict_exp_chem_rxn

    def get_chemical_reaction_given_iri(self, chem_rxn_iri: str) -> ChemicalReaction:
        chem_rxn_iri = trimIRI(chem_rxn_iri)
        query = f"""{PREFIX_RDF}
                SELECT DISTINCT ?chem_rxn ?reactant ?reac_type ?reac_species ?product ?prod_type ?prod_species
                ?catalyst ?cata_type ?cata_species ?solvent ?solv_type ?solv_species ?base ?base_type ?base_species
                    ?doe_template
                WHERE {{
                    VALUES ?reac_type {{<{ONTOKIN_SPECIES}> <{ONTOKIN_REACTANT}>}}.
                    VALUES ?prod_type {{<{ONTOKIN_SPECIES}> <{ONTOKIN_PRODUCT}> <{ONTOREACTION_TARGETPRODUCT}> <{ONTOREACTION_IMPURITY}>}}.
                    VALUES ?chem_rxn {{<{chem_rxn_iri}>}}.
                    ?chem_rxn <{ONTOCAPE_HASREACTANT}> ?reactant; <{ONTOCAPE_HASPRODUCT}> ?product.
                    ?reactant rdf:type ?reac_type; <{ONTOSPECIES_HASUNIQUESPECIES}> ?reac_species.
                    ?product rdf:type ?prod_type; <{ONTOSPECIES_HASUNIQUESPECIES}> ?prod_species.
                    OPTIONAL{{
                        VALUES ?cata_type {{<{ONTOKIN_SPECIES}> <{ONTOREACTION_CATALYST}>}}.
                        ?chem_rxn <{ONTOREACTION_HASCATALYST}> ?catalyst.
                        ?catalyst rdf:type ?cata_type; <{ONTOSPECIES_HASUNIQUESPECIES}> ?cata_species.
                    }}
                    OPTIONAL{{
                        VALUES ?solv_type {{<{ONTOKIN_SPECIES}> <{ONTOREACTION_SOLVENT}>}}.
                        ?chem_rxn <{ONTOREACTION_HASSOLVENT}> ?solvent.
                        ?solvent rdf:type ?solv_type; <{ONTOSPECIES_HASUNIQUESPECIES}> ?solv_species.
                    }}
                    OPTIONAL{{
                        VALUES ?base_type {{<{ONTOKIN_SPECIES}> <{ONTOREACTION_BASE}>}}.
                        ?chem_rxn <{ONTOREACTION_HASBASE}> ?base.
                        ?base rdf:type ?base_type; <{ONTOSPECIES_HASUNIQUESPECIES}> ?base_species.
                    }}
                    OPTIONAL{{?chem_rxn <{ONTODOE_HASDOETEMPLATE}> ?doe_template.}}
                }}"""
        response = self.performQuery(query)

        chem_rxn = ChemicalReaction(
            instance_iri=chem_rxn_iri,
            hasReactant=self.get_ontokin_species_from_chem_rxn(response, 'reactant', 'reac_type', 'reac_species'),
            hasProduct=self.get_ontokin_species_from_chem_rxn(response, 'product', 'prod_type', 'prod_species'),
            hasCatalyst=self.get_ontokin_species_from_chem_rxn(response, 'catalyst', 'cata_type', 'cata_species'),
            hasSolvent=self.get_ontokin_species_from_chem_rxn(response, 'solvent', 'solv_type', 'solv_species'),
            hasBase=self.get_ontokin_species_from_chem_rxn(response, 'base', 'base_type', 'base_species'),
            hasDoETemplate=dal.get_the_unique_value_in_list_of_dict(response, 'doe_template'),
        )

        return chem_rxn

    def locate_possible_input_chemical(
        self,
        solute: str,
        solvent_as_constraint: List[str]=None,
        species_to_exclude: List[str]=None,
        list_vapourtec_rs400_iri: Union[str, list]=None,
        list_of_labs_as_constraint: list=None,
        is_ref_chemical: bool=False,
    ) -> Optional[InputChemical]:
        list_vapourtec_rs400 = self.get_vapourtec_rs400(
            list_vapourtec_rs400_iri=list_vapourtec_rs400_iri,
            list_of_labs_as_constraint=list_of_labs_as_constraint
        )
        for vapourtec_rs400 in list_vapourtec_rs400:
            material = vapourtec_rs400.has_access_to_chemical_species(
                solute=solute,
                solvent_as_constraint=solvent_as_constraint,
                species_to_exclude=species_to_exclude,
            )
            if material is None:
                continue
            else:
                # TODO [future work, nice-to-have] return a new created instance of InputChemical (new IRIs) with the same phase composition
                # this will make it robust against ppl deleting chemical amount related data in lab when they are consumed
                return InputChemical(
                    instance_iri=material.instance_iri,
                    thermodynamicBehaviour=material.thermodynamicBehaviour,
                    recommended_reaction_scale=vapourtec_rs400.recommendedReactionScale if is_ref_chemical else None,
                )
        return None

    def get_ontokin_species_from_chem_rxn(self, list_of_dict: List[Dict], species_role: str, species_type: str, ontospecies_key: str) -> List[OntoKin_Species]:
        """Example: species_role='reactant', species_type='reac_type', species_key='reac_species'"""
        list_species = []
        if not dal.check_if_key_in_list_of_dict(list_of_dict, species_role):
            return None
        else:
            wanted_info = dal.keep_wanted_keys_from_list_of_dict(list_of_dict, [species_role, species_type, ontospecies_key])
            wanted_info = dal.remove_duplicate_dict_from_list_of_dict(wanted_info)
            unique_species_iri = dal.get_unique_values_in_list_of_dict(wanted_info, species_role)
            for u_s in unique_species_iri:
                u_s_info = dal.get_sublist_in_list_of_dict_matching_key_value(wanted_info, species_role, u_s)
                if len(u_s_info) > 1:
                    raise Exception("Multiple OntoSpecies:Species identified given one instance of %s <%s>: %s" % (species_role ,u_s, str(u_s_info)))
                elif len(u_s_info) < 1:
                    raise Exception("No unique OntoSpecies:Species identified for instance of %s <%s>." % (species_role, u_s))
                else:
                    u_s_info = u_s_info[0]
                if u_s_info[species_type] == ONTOKIN_SPECIES:
                    species = OntoKin_Species(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOKIN_REACTANT:
                    species = OntoKin_Reactant(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOKIN_PRODUCT:
                    species = OntoKin_Product(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOREACTION_TARGETPRODUCT:
                    species = TargetProduct(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOREACTION_IMPURITY:
                    species = Impurity(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOREACTION_CATALYST:
                    species = Catalyst(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOREACTION_SOLVENT:
                    species = Solvent(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                elif u_s_info[species_type] == ONTOREACTION_BASE:
                    species = Base(instance_iri=u_s_info[species_role],hasUniqueSpecies=u_s_info[ontospecies_key])
                else:
                    raise Exception("Species type (%s) NOT supported for: %s" % (u_s_info[species_type], str(u_s_info)))
                list_species.append(species)

            return list_species

    def get_input_chemical_of_rxn_exp(self, rxnexp_iri: str) -> Dict[str, List[InputChemical]]:
        if not isinstance(rxnexp_iri, list):
            rxnexp_iri = [rxnexp_iri]
        rxnexp_iri = trimIRI(rxnexp_iri)
        return self.get_chemical(rxnexp_iri, ONTOREACTION_HASINPUTCHEMICAL)

    def get_output_chemical_of_rxn_exp(self, rxnexp_iri: str) -> Dict[str, List[OutputChemical]]:
        if not isinstance(rxnexp_iri, list):
            rxnexp_iri = [rxnexp_iri]
        rxnexp_iri = trimIRI(rxnexp_iri)
        return self.get_chemical(rxnexp_iri, ONTOREACTION_HASOUTPUTCHEMICAL)

    def get_chemical(self, subject_iris, predicate_iri, desired_type: str=None) -> Dict[str, List[Chemical]]:
        if not isinstance(subject_iris, list):
            subject_iris = [subject_iris]
        subject_iris = trimIRI(subject_iris)
        predicate_iri = trimIRI(predicate_iri)

        if predicate_iri == ONTOREACTION_HASINPUTCHEMICAL:
            ocm_query_key = 'input_chemical'
        elif predicate_iri == ONTOREACTION_HASOUTPUTCHEMICAL:
            ocm_query_key = 'output_chemical'
        else:
            ocm_query_key = 'chemical'

        query = f"""{PREFIX_RDF}
                SELECT ?subject ?{ocm_query_key} ?single_phase ?state_of_aggregation ?composition ?phase_component ?chemical_species ?phase_component_concentration ?concentration_type ?value ?unit ?num_val
                WHERE {{
                    VALUES ?subject {{ <{'> <'.join(subject_iris)}> }}
                    ?subject <{predicate_iri}> ?{ocm_query_key} .
                    ?{ocm_query_key} <{ONTOCAPE_THERMODYNAMICBEHAVIOR}> ?single_phase .
                    ?single_phase rdf:type <{ONTOCAPE_SINGLEPHASE}> .
                    ?single_phase <{ONTOCAPE_HASSTATEOFAGGREGATION}> ?state_of_aggregation; <{ONTOCAPE_HAS_COMPOSITION}> ?composition; <{ONTOCAPE_ISCOMPOSEDOFSUBSYSTEM}> ?phase_component .
                    ?composition <{ONTOCAPE_COMPRISESDIRECTLY}> ?phase_component_concentration .
                    ?phase_component <{ONTOCAPE_REPRESENTSOCCURENCEOF}> ?chemical_species .
                    ?phase_component <{ONTOCAPE_HASPROPERTY}> ?phase_component_concentration .
                    ?phase_component_concentration rdf:type ?concentration_type; <{ONTOCAPE_HASVALUE}> ?value .
                    ?value <{ONTOCAPE_HASUNITOFMEASURE}> ?unit; <{ONTOCAPE_NUMERICALVALUE}> ?num_val.
                }}"""

        response_of_all_subject = self.performQuery(query)

        dict_chemical = {}
        for subject_iri in subject_iris:
            response = dal.get_sublist_in_list_of_dict_matching_key_value(response_of_all_subject, 'subject', subject_iri)
            # NOTE here we make that if nothing found, return None
            if len(response) == 0:
                logger.warning(f"Nothing found when quering: {subject_iri} {predicate_iri} ?{ocm_query_key}")
                dict_chemical[subject_iri] = None

            else:
                lst_chemical = []

                # generate different list for each OntoCAPE:Material
                unique_chemical_iri = dal.get_unique_values_in_list_of_dict(response, ocm_query_key)
                list_list_chemical = []
                for iri in unique_chemical_iri:
                    list_list_chemical.append([res for res in response if iri == res[ocm_query_key]])

                for list_om in list_list_chemical:
                    chemical_iri = dal.get_unique_values_in_list_of_dict(list_om, ocm_query_key)[0] # here we are sure this is the unique value of OntoCAPE:Material

                    # validate that the list of responses are only referring to one instance of OntoCAPE_SinglePhase, one instance of OntoCAPE_StateOfAggregation and one instance of OntoCAPE_Composition, otherwise raise an Exception
                    unique_single_phase_iri = dal.get_unique_values_in_list_of_dict(list_om, 'single_phase')
                    if len(unique_single_phase_iri) > 1:
                        raise Exception("Multiple thermodynamicBehavior OntoCAPE:SinglePhase identified (<%s>) in one instance of OntoReaction:InputChemical/OntoReaction:OutputChemical/OntoCAPE:Material %s is currently NOT supported." % ('>, <'.join(unique_single_phase_iri), chemical_iri))
                    elif len(unique_single_phase_iri) < 1:
                        raise Exception("No instance of thermodynamicBehavior OntoCAPE:SinglePhase was identified given instance of OntoReaction:InputChemical/OntoReaction:OutputChemical/OntoCAPE:Material: %s" % chemical_iri)
                    else:
                        unique_single_phase_iri = unique_single_phase_iri[0]

                    unique_state_of_aggregation_iri = dal.get_unique_values_in_list_of_dict(list_om, 'state_of_aggregation')
                    if len(unique_state_of_aggregation_iri) > 1:
                        raise Exception("Multiple hasStateOfAggregation OntoCAPE:StateOfAggregation identified (<%s>) in one instance of OntoCAPE:SinglePhase %s is currently NOT supported." % ('>, <'.join(unique_state_of_aggregation_iri), unique_single_phase_iri))
                    elif len(unique_state_of_aggregation_iri) < 1:
                        raise Exception("No instance of hasStateOfAggregation OntoCAPE:StateOfAggregation was identified given instance of OntoCAPE:SinglePhase: %s" % (unique_single_phase_iri))
                    else:
                        if unique_state_of_aggregation_iri[0] == ONTOCAPE_LIQUID:
                            state_of_aggregation = OntoCAPE_liquid
                        else:
                            # TODO [future work] add support for other phase (solid, gas)
                            pass

                    unique_composition_iri = dal.get_unique_values_in_list_of_dict(list_om, 'composition')
                    if len(unique_composition_iri) > 1:
                        raise Exception("Multiple has_composition OntoCAPE:Composition identified (<%s>) in one instance of OntoCAPE:SinglePhase %s is currently NOT supported." % ('>, <'.join(unique_composition_iri), unique_single_phase_iri))
                    elif len(unique_composition_iri) < 1:
                        raise Exception("No instance of has_composition OntoCAPE:Composition was identified given instance of OntoCAPE:SinglePhase: %s" % (unique_single_phase_iri))
                    else:
                        unique_composition_iri = unique_composition_iri[0]

                    # secondly, get a list of OntoCAPE_PhaseComponent to be added to the OntoCAPE_SinglePhase instance
                    list_phase_component = []
                    list_phase_component_concentration = []
                    for r in list_om:
                        if 'concentration_type' in r:
                            if r['concentration_type'] == OntoCAPE_Molarity.__fields__['clz'].default:
                                concentration = OntoCAPE_Molarity(instance_iri=r['phase_component_concentration'],hasValue=OntoCAPE_ScalarValue(instance_iri=r['value'],numericalValue=r['num_val'],hasUnitOfMeasure=r['unit']))
                            else:
                                # TODO [future work] add support for other type of OntoCAPE_PhaseComponentConcentration
                                raise NotImplementedError("Support for <%s> as OntoCAPE_PhaseComponentConcentration is NOT implemented yet." % r['concentration_type'])
                            list_phase_component_concentration.append(concentration)
                        else:
                            raise Exception("Concentration is not defined for")

                        phase_component = OntoCAPE_PhaseComponent(instance_iri=r['phase_component'],representsOccurenceOf=r['chemical_species'],hasProperty=concentration)
                        list_phase_component.append(phase_component)

                    composition = OntoCAPE_Composition(
                        instance_iri=unique_composition_iri,
                        comprisesDirectly=list_phase_component_concentration
                    )
                    single_phase = OntoCAPE_SinglePhase(
                        instance_iri=unique_single_phase_iri,
                        hasStateOfAggregation=state_of_aggregation,
                        isComposedOfSubsystem=list_phase_component,
                        has_composition=composition,
                        representsThermodynamicBehaviorOf=chemical_iri
                    )

                    if desired_type is None:
                        if predicate_iri == ONTOREACTION_HASINPUTCHEMICAL:
                            ocm_instance = InputChemical(instance_iri=chemical_iri,thermodynamicBehaviour=single_phase)
                        elif predicate_iri == ONTOREACTION_HASOUTPUTCHEMICAL:
                            ocm_instance = OutputChemical(instance_iri=chemical_iri,thermodynamicBehaviour=single_phase)
                        else:
                            ocm_instance = Chemical(instance_iri=chemical_iri,thermodynamicBehaviour=single_phase)
                    elif desired_type == ONTOREACTION_INPUTCHEMICAL:
                        ocm_instance = InputChemical(instance_iri=chemical_iri,thermodynamicBehaviour=single_phase)
                    elif desired_type == ONTOREACTION_OUTPUTCHEMICAL:
                        ocm_instance = OutputChemical(instance_iri=chemical_iri,thermodynamicBehaviour=single_phase)
                    else:
                        ocm_instance = Chemical(instance_iri=chemical_iri,thermodynamicBehaviour=single_phase)

                    lst_chemical.append(ocm_instance)

                dict_chemical[subject_iri] = lst_chemical

        return dict_chemical

    def construct_query_for_autosampler(self, given_autosampler_iri: str = None) -> str:
        # TODO this query will return nothing if the autosampler is an empty rack on line "?site <{ONTOVAPOURTEC_HOLDS}> ?vial; <{ONTOVAPOURTEC_LOCATIONID}> ?loc."
        query = f"""{PREFIX_RDF}
                SELECT ?autosampler ?autosampler_manufacturer ?autosampler_power_supply
                ?sample_loop_volume ?sample_loop_volume_value ?sample_loop_volume_unit ?sample_loop_volume_num_val
                ?site ?loc ?vial ?fill_level ?fill_level_om_value ?fill_level_unit ?fill_level_num_val
                ?warn_level ?warn_level_om_value ?warn_level_unit ?warn_level_num_val
                ?max_level ?max_level_om_value ?max_level_unit ?max_level_num_val ?chemical_amount ?contains_unidentified_component
                WHERE {{
                    {'VALUES ?autosampler { <%s> }' % trimIRI(given_autosampler_iri) if given_autosampler_iri is not None else ""}
                    ?autosampler rdf:type <{ONTOVAPOURTEC_AUTOSAMPLER}>;
                                 <{DBPEDIA_MANUFACTURER}> ?autosampler_manufacturer;
                                 <{ONTOLAB_HASPOWERSUPPLY}> ?autosampler_power_supply;
                                 <{ONTOVAPOURTEC_SAMPLELOOPVOLUME}> ?sample_loop_volume.
                    ?sample_loop_volume <{OM_HASVALUE}> ?sample_loop_volume_value.
                    ?sample_loop_volume_value <{OM_HASUNIT}> ?sample_loop_volume_unit;
                                              <{OM_HASNUMERICALVALUE}> ?sample_loop_volume_num_val.
                    ?autosampler <{ONTOVAPOURTEC_HASSITE}> ?site.
                    ?site <{ONTOVAPOURTEC_HOLDS}> ?vial; <{ONTOVAPOURTEC_LOCATIONID}> ?loc.
                    OPTIONAL {{
                        ?vial <{ONTOLAB_HASFILLLEVEL}> ?fill_level.
                        ?fill_level <{OM_HASVALUE}> ?fill_level_om_value.
                        ?fill_level_om_value <{OM_HASUNIT}> ?fill_level_unit;
                                             <{OM_HASNUMERICALVALUE}> ?fill_level_num_val.
                    }}
                    OPTIONAL {{
                        ?vial <{ONTOLAB_HASWARNINGLEVEL}> ?warn_level.
                        ?warn_level <{OM_HASVALUE}> ?warn_level_om_value.
                        ?warn_level_om_value <{OM_HASUNIT}> ?warn_level_unit;
                                             <{OM_HASNUMERICALVALUE}> ?warn_level_num_val.
                    }}
                    OPTIONAL {{
                        ?vial <{ONTOLAB_HASMAXLEVEL}> ?max_level.
                        ?max_level <{OM_HASVALUE}> ?max_level_om_value.
                        ?max_level_om_value <{OM_HASUNIT}> ?max_level_unit;
                                            <{OM_HASNUMERICALVALUE}> ?max_level_num_val.
                    }}
                    OPTIONAL {{
                        ?vial <{ONTOLAB_ISFILLEDWITH}> ?chemical_amount.
                        # NOTE below is made optional to accommodate the situation where the chemical amount is generated at the end of reaction
                        # NOTE but not characterised by HPLCPostPro yet, thus we don't have information about the concentation, hence not known if contains unidentified component
                        OPTIONAL {{?chemical_amount <{ONTOLAB_CONTAINSUNIDENTIFIEDCOMPONENT}> ?contains_unidentified_component.}}
                    }}
                }}"""
        return query

    # TODO add support if none of the autosampler site holds anything (i.e. an empty rack with no vial)
    # the current implementation will throw an error
    def get_all_autosampler_with_fill(self, given_autosampler_iri: str = None) -> List[AutoSampler]:
        query = self.construct_query_for_autosampler(given_autosampler_iri)
        response = self.performQuery(query)
        unique_autosampler_list = dal.get_unique_values_in_list_of_dict(response, 'autosampler')

        list_autosampler = []
        for specific_autosampler in unique_autosampler_list:
            info_of_specific_autosampler = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'autosampler', specific_autosampler) if given_autosampler_iri is None else response

            try:
                unique_specific_autosampler_manufacturer_iri = dal.get_the_unique_value_in_list_of_dict(info_of_specific_autosampler, 'autosampler_manufacturer')
            except Exception as e:
                logger.error(f"dbpedia:manufacturer is not correctly defined for {specific_autosampler}", exc_info=True)
                raise e

            try:
                unique_specific_autosampler_power_supply_iri = dal.get_the_unique_value_in_list_of_dict(info_of_specific_autosampler, 'autosampler_power_supply')
            except Exception as e:
                logger.error(f"OntoLab:hasPowerSupply is not correctly defined for {specific_autosampler}", exc_info=True)
                raise e

            try:
                unique_sample_loop_volume_iri = dal.get_the_unique_value_in_list_of_dict(info_of_specific_autosampler, 'sample_loop_volume')
            except Exception as e:
                logger.error(f"OntoVapourtec:sampleLoopVolume is not correctly defined for {specific_autosampler}", exc_info=True)
                raise e

            try:
                unique_sample_loop_volume_value_iri = dal.get_the_unique_value_in_list_of_dict(info_of_specific_autosampler, 'sample_loop_volume_value')
            except Exception as e:
                logger.error(f"OM:Measure of OntoVapourtec:sampleLoopVolume is not correctly defined for {specific_autosampler}", exc_info=True)
                raise e

            try:
                unique_sample_loop_volume_unit = dal.get_the_unique_value_in_list_of_dict(info_of_specific_autosampler, 'sample_loop_volume_unit')
            except Exception as e:
                logger.error(f"OM:Measure unit of OntoVapourtec:sampleLoopVolume is not correctly defined for {specific_autosampler}", exc_info=True)
                raise e

            try:
                unique_sample_loop_volume_num_val = dal.get_the_unique_value_in_list_of_dict(info_of_specific_autosampler, 'sample_loop_volume_num_val')
            except Exception as e:
                logger.error(f"OM:Measure num val of OntoVapourtec:sampleLoopVolume is not correctly defined for {specific_autosampler}", exc_info=True)
                raise e

            unique_site_list = dal.get_unique_values_in_list_of_dict(info_of_specific_autosampler, 'site')
            list_autosampler_site = []
            for specific_site in unique_site_list:
                info_of_specific_site = dal.get_sublist_in_list_of_dict_matching_key_value(info_of_specific_autosampler, 'site', specific_site)
                if len(info_of_specific_site) > 1:
                    raise Exception(f"Information for specific site is not correctly defined for {specific_site}, one set of information is expected, obtained: {info_of_specific_site}")
                info_of_specific_site = info_of_specific_site[0]

                if 'chemical_amount' in info_of_specific_site:
                    lst_referred_instance_of_chemical = self.get_chemical(info_of_specific_site['chemical_amount'], ONTOCAPE_REFERSTOMATERIAL)[info_of_specific_site['chemical_amount']]
                    if lst_referred_instance_of_chemical is None:
                        referred_instance_of_chemical = None
                    elif len(lst_referred_instance_of_chemical) > 1:
                        raise Exception(f"There are more than one OntoCAPE:Material referred by OntoVapourtec:AutoSamplerSite <{specific_site}>: {lst_referred_instance_of_chemical}.")
                    else:
                        referred_instance_of_chemical = lst_referred_instance_of_chemical[0]
                else:
                    referred_instance_of_chemical = None
                vial = Vial(
                    instance_iri=info_of_specific_site['vial'],
                    isFilledWith=ChemicalAmount(
                        instance_iri=info_of_specific_site['chemical_amount'],
                        refersToMaterial=referred_instance_of_chemical,
                        fills=info_of_specific_site['vial'],
                        isPreparedBy=None, # TODO [future work] add support for isPreparedBy
                        containsUnidentifiedComponent=info_of_specific_site['contains_unidentified_component'] if 'contains_unidentified_component' in info_of_specific_site else None,
                    ) if 'chemical_amount' in info_of_specific_site else None,
                    hasFillLevel=OM_Volume(
                        instance_iri=info_of_specific_site['fill_level'],
                        hasValue=OM_Measure(
                            instance_iri=info_of_specific_site['fill_level_om_value'],
                            hasUnit=info_of_specific_site['fill_level_unit'],
                            hasNumericalValue=info_of_specific_site['fill_level_num_val']
                        )
                    ),
                    hasWarningLevel=OM_Volume(
                        instance_iri=info_of_specific_site['warn_level'],
                        hasValue=OM_Measure(
                            instance_iri=info_of_specific_site['warn_level_om_value'],
                            hasUnit=info_of_specific_site['warn_level_unit'],
                            hasNumericalValue=info_of_specific_site['warn_level_num_val']
                        )
                    ),
                    hasMaxLevel=OM_Volume(
                        instance_iri=info_of_specific_site['max_level'],
                        hasValue=OM_Measure(
                            instance_iri=info_of_specific_site['max_level_om_value'],
                            hasUnit=info_of_specific_site['max_level_unit'],
                            hasNumericalValue=info_of_specific_site['max_level_num_val']
                        )
                    ),
                )
                autosampler_site = AutoSamplerSite(
                    instance_iri=specific_site,
                    holds=vial,
                    locationID=info_of_specific_site['loc']
                )
                list_autosampler_site.append(autosampler_site)

            autosampler = AutoSampler(
                instance_iri=specific_autosampler,
                manufacturer=unique_specific_autosampler_manufacturer_iri,
                hasPowerSupply=unique_specific_autosampler_power_supply_iri,
                hasSite=list_autosampler_site,
                sampleLoopVolume=OM_Volume(
                    instance_iri=unique_sample_loop_volume_iri,
                    hasValue=OM_Measure(
                        instance_iri=unique_sample_loop_volume_value_iri,
                        hasUnit=unique_sample_loop_volume_unit,
                        hasNumericalValue=unique_sample_loop_volume_num_val
                    )
                )
            )
            list_autosampler.append(autosampler)

        return list_autosampler

    def getExpReactionCondition(self, rxnexp_iris: Union[str, list]) -> Dict[str, List[ReactionCondition]]:
        """
            This method retrieves a dict of ReactionCondition pointed by the given instances of OntoReaction:ReactionExperiment/ReactionVariation.

            Arguments:
                rxnexp_iris - IRIs of OntoReaction:ReactionExperiment/ReactionVariation instances
        """

        if not isinstance(rxnexp_iris, list):
            rxnexp_iris = [rxnexp_iris]

        # Delete "<" and ">" around the IRI
        rxnexp_iris = trimIRI(rxnexp_iris)

        # Construct query string
        query = f"""{PREFIX_RDFS} {PREFIX_RDF} {PREFIX_XSD} {PREFIX_OWL}
                SELECT DISTINCT ?rxn ?condition ?subo ?clz ?measure ?val ?unit ?parameter_setting ?id ?multi ?usage
                WHERE {{
                    VALUES ?rxn {{ <{'> <'.join(rxnexp_iris)}> }}
                    ?subo rdfs:subPropertyOf* <{ONTOREACTION_HASREACTIONCONDITION}> . ?rxn ?subo ?condition .
                    ?condition rdf:type ?clz . FILTER(?clz != owl:Thing && ?clz != owl:NamedIndividual && ?clz != <{ONTOREACTION_REACTIONCONDITION}>) .
                    ?condition <{OM_HASVALUE}> ?measure . ?measure <{OM_HASUNIT}> ?unit; <{OM_HASNUMERICALVALUE}> ?val .
                    OPTIONAL {{ ?parameter_setting <{ONTOLAB_HASQUANTITY}> ?condition . }} .
                    OPTIONAL {{ ?condition <{ONTODOE_POSITIONALID}> ?id . }} .
                    OPTIONAL {{ ?condition <{ONTOREACTION_INDICATESMULTIPLICITYOF}> ?multi . }} .
                    OPTIONAL {{ ?condition <{ONTOREACTION_INDICATESUSAGEOF}> ?usage . }} .
                }}"""

        # Perform SPARQL query
        response_of_all_rxn = self.performQuery(query)

        # Construct a dict of ReactionCondition
        dict_rxn_condition = {}

        # Get response for each reaction
        unique_rxn_iris = dal.get_unique_values_in_list_of_dict(response_of_all_rxn, 'rxn')
        for rxn_iri in unique_rxn_iris:
            response = dal.get_sublist_in_list_of_dict_matching_key_value(response_of_all_rxn, 'rxn', rxn_iri)

            # Populate the list of ReactionCondition based on query results
            list_con = []

            unique_reaction_condition = dal.get_unique_values_in_list_of_dict(response, 'condition')
            dct_unique_rc = {rc:dal.get_sublist_in_list_of_dict_matching_key_value(response, 'condition', rc) for rc in unique_reaction_condition}
            for rc in unique_reaction_condition:
                _info = dct_unique_rc[rc]
                __clz = list(set([d['clz'] for d in _info]))
                if len(__clz) == 1:
                    _clz = __clz[0]
                else:
                    raise Exception("The rdf:type of ReactionCondition <%s> is not uniquely identified: %s" % (rc, __clz))
                _subo = list(set([d['subo'] for d in _info]))
                _id = list(set([d['id'] for d in _info if 'id' in d]))
                _measure = list(set([d['measure'] for d in _info if 'measure' in d]))
                _unit = list(set([d['unit'] for d in _info if 'unit' in d]))
                _val = list(set([d['val'] for d in _info if 'val' in d]))
                _parameter_setting = list(set([d['parameter_setting'] for d in _info if 'parameter_setting' in d]))
                _multi = list(set([d['multi'] for d in _info if 'multi' in d]))
                _usage = list(set([d['usage'] for d in _info if 'usage' in d]))

                rxn_condition = ReactionCondition(
                    instance_iri=rc,
                    clz=_clz,
                    objPropWithExp=_subo,
                    hasValue=OM_Measure(instance_iri=_measure[0],hasUnit=_unit[0],hasNumericalValue=_val[0]) if len(_measure) == 1 else None,
                    positionalID=_id[0] if len(_id) == 1 else None,
                    translateToParameterSetting=_parameter_setting[0] if len(_parameter_setting) == 1 else None,
                    indicatesMultiplicityOf=_multi[0] if len(_multi) == 1 else None,
                    indicatesUsageOf=_usage[0] if len(_usage) == 1 else None
                )
                list_con.append(rxn_condition)

            dict_rxn_condition[rxn_iri] = list_con

        return dict_rxn_condition

    def getExpPerformanceIndicator(self, rxnexp_iris: Union[str, list]) -> Dict[str, List[PerformanceIndicator]]:
        """
            This method retrieves a dict of PerformanceIndicator pointed by the given instances of OntoReaction:ReactionExperiment/ReactionVariation.

            Arguments:
                rxnexp_iris - IRIs of OntoReaction:ReactionExperiment/ReactionVariation instances
        """

        if not isinstance(rxnexp_iris, list):
            rxnexp_iris = [rxnexp_iris]

        # Delete "<" and ">" around the IRI
        rxnexp_iris = trimIRI(rxnexp_iris)

        # Construct query string
        query = f"""{PREFIX_RDFS} {PREFIX_RDF} {PREFIX_XSD} {PREFIX_OWL}
                SELECT DISTINCT ?rxn ?perf ?subo ?clz ?measure ?val ?unit ?id
                WHERE {{
                    VALUES ?rxn {{ <{'> <'.join(rxnexp_iris)}> }}
                    ?subo rdfs:subPropertyOf* <{ONTOREACTION_HASPERFORMANCEINDICATOR}> . ?rxn ?subo ?perf .
                    ?perf rdf:type ?clz . FILTER(?clz != owl:Thing && ?clz != owl:NamedIndividual && ?clz != <{ONTOREACTION_PERFORMANCEINDICATOR}>) .
                    OPTIONAL {{ ?perf <{ONTODOE_POSITIONALID}> ?id . }} .
                    OPTIONAL {{ ?perf <{OM_HASVALUE}> ?measure . ?measure <{OM_HASUNIT}> ?unit; <{OM_HASNUMERICALVALUE}> ?val . }} .
                }}"""
        # Perform SPARQL query
        response_of_all_rxn = self.performQuery(query)

        # Construct a dict of PerformanceIndicator, default to have everything as None
        dict_perf_indicator = {rx:None for rx in rxnexp_iris}

        # Populate the dict of PerformanceIndicator based on query results
        unique_rxn_exp = dal.get_unique_values_in_list_of_dict(response_of_all_rxn, 'rxn')
        if len(unique_rxn_exp) == 0:
            logger.info(f"ReactionExperiment/ReactionVariation {rxnexp_iris} has no PerformanceIndicator computed yet")
            return dict_perf_indicator

        for rxnexp_iri in unique_rxn_exp:
            response = dal.get_sublist_in_list_of_dict_matching_key_value(response_of_all_rxn, 'rxn', rxnexp_iri)

            # Populate the list of PerformanceIndicator based on query results
            list_perf = []
            unique_performance_indicator = dal.get_unique_values_in_list_of_dict(response, 'perf')
            dct_unique_pi = {pi:dal.get_sublist_in_list_of_dict_matching_key_value(response, 'perf', pi) for pi in unique_performance_indicator}
            for pi in unique_performance_indicator:
                _info = dct_unique_pi[pi]
                __clz = list(set([d['clz'] for d in _info]))
                if len(__clz) == 1:
                    _clz = __clz[0]
                else:
                    raise Exception("The rdf:type of PerformanceIndicator <%s> is not uniquely identified: %s" % (pi, __clz))
                _subo = list(set([d['subo'] for d in _info]))
                _id = list(set([d['id'] for d in _info if 'id' in d]))
                _measure = list(set([d['measure'] for d in _info if 'measure' in d]))
                _unit = list(set([d['unit'] for d in _info if 'unit' in d]))
                _val = list(set([d['val'] for d in _info if 'val' in d]))
                perf_indicator = PerformanceIndicator(
                    instance_iri=pi,
                    clz=_clz,
                    rxn_exp_iri=rxnexp_iri,
                    objPropWithExp=_subo,
                    hasValue=OM_Measure(instance_iri=_measure[0],hasUnit=_unit[0],hasNumericalValue=_val[0]) if len(_measure) == 1 else None,
                    positionalID=_id[0] if len(_id) == 1 else None
                )
                list_perf.append(perf_indicator)

            # Add the list of PerformanceIndicator to the dict, this will replace the None value
            dict_perf_indicator[rxnexp_iri] = list_perf

        return dict_perf_indicator

    def getDoEStrategy(self, strategy_iri: str) -> Strategy:
        """
            This method retrieves information given instance of dataclass OntoDoE.Strategy.

            Arguments:
                strategy_iri - iri of OntoDoE:Strategy instance
        """
        # Delete "<" and ">" around the IRI
        strategy_iri = trimIRI(strategy_iri)

        # Construct query string
        query = f"""{PREFIX_RDF} {PREFIX_OWL}
                SELECT ?strategy
                WHERE {{
                    <{strategy_iri}> rdf:type ?strategy .
                    FILTER(?strategy != owl:Thing && ?strategy != owl:NamedIndividual) .
                }}"""

        # Perform SPARQL query
        response = self.performQuery(query)

        # Extract from the response
        if (len(response) > 1):
            raise Exception("Strategy Instance <%s> should only have one rdf:type." % (strategy_iri))
        res = [list(r.values())[0] for r in response]

        if (res[0] == ONTODOE_TSEMO):
            tsemo_instance = self.getTSEMOSettings(strategy_iri)
            return tsemo_instance
        elif (res[0] == ONTODOE_NEWSTBO):
            sobo_instance = self.get_newstbo_settings(strategy_iri)
            return sobo_instance
        elif (res[0] == ONTODOE_LHS):
            # TODO implement handling for LHS
            raise NotImplementedError("LHS as a OntoDoE:Strategy is not yet supported.")
        else:
            # TODO implement handling for other DoE strategy
            raise NotImplementedError("<%s> as a OntoDoE:Strategy is not yet supported." % (res[0]))

    def getTSEMOSettings(self, tsemo_iri: str) -> TSEMO:
        """
            This method retrieves the settings of TSEMO algorithm as part of `Summit` package from the given instance of OntoDoE.TSEMO.
            (For TSEMO algorithm in package `Summit`, please visit: https://gosummit.readthedocs.io/en/latest/strategies.html#tsemo)

            Arguments:
                tsemo_iri - iri of OntoDoE:TSEMO instance
        """

        # Construct query string
        query = """SELECT ?nGenerations ?nRetries ?nSpectralPoints ?populationSize \
                WHERE { \
                OPTIONAL {
                    <%s> <%s> ?nGenerations . \
                    <%s> <%s> ?nRetries . \
                    <%s> <%s> ?nSpectralPoints . \
                    <%s> <%s> ?populationSize . \
                } \
                }""" % (tsemo_iri, ONTODOE_NGENERATIONS, tsemo_iri, ONTODOE_NRETRIES, tsemo_iri, ONTODOE_NSPECTRALPOINTS, tsemo_iri, ONTODOE_POPULATIONSIZE)

        # Perform SPARQL query
        response = self.performQuery(query)
        if (len(response) > 1):
            raise Exception("Instance <%s> should only have one set of settings." % (tsemo_iri))
        tsemo_instance = TSEMO(instance_iri=tsemo_iri,**response[0])
        return tsemo_instance

    def get_newstbo_settings(self, newstbo_iri: str) -> NewSTBO:
        # TODO add support for customising NewSTBO settings
        return NewSTBO(instance_iri=newstbo_iri)

    def getNewExperimentFromDoE(self, doe_iri: str) -> ReactionExperiment:
        doe_iri = trimIRI(doe_iri)

        query = """SELECT ?newexp \
                WHERE { <%s> <%s> ?newexp .}""" % (doe_iri, ONTODOE_PROPOSESNEWEXPERIMENT)

        response = self.performQuery(query)

        if (len(response) > 1):
            raise Exception(
                "DesignOfExperiment instance <%s> should only propose ONE instance of ReactionExperiment, it is currently proposing: <%s>" % (
                    doe_iri, ">, <".join([res['newexp'] for res in response])
                )
            )
        elif (len(response) == 1):
            return self.getReactionExperiment(response[0]['newexp'])[0]
        return None

    def collect_triples_for_equip_settings(self, equip_settings: List[EquipmentSettings], configure_digital_twin: bool):
        g = Graph()
        for es in equip_settings:
            g = es.create_instance_for_kg(g, configure_digital_twin)

        return g

    def get_preferred_vapourtec_rs400(
        self, rxnexp: ReactionExperiment,
        list_of_labs: list=None,
        less_desired_reactors: list=None,
    ) -> Tuple[VapourtecRS400, HPLC]:
        """ This function queries the digital twin of the most suitable VapourtecRS400 for the given reaction experiment."""
        # query the digital twin to get the list of VapourtecRS400 modules (if within the given list of labs)
        list_vapourtec_rs400 = self.get_vapourtec_rs400(list_of_labs_as_constraint=list_of_labs)

        suitable_vapourtec_rs400 = [rs400 for rs400 in list_vapourtec_rs400 if rs400.is_suitable_for_reaction_experiment(rxnexp)]
        desirable_vapourtec_rs400 = [
            rs400 for rs400 in suitable_vapourtec_rs400 if not rs400.consists_of_lab_equipment(less_desired_reactors)
        ] if less_desired_reactors is not None else suitable_vapourtec_rs400
        vapourtec_rs400_final_options = suitable_vapourtec_rs400 if len(desirable_vapourtec_rs400) == 0 else desirable_vapourtec_rs400
        for rs400 in vapourtec_rs400_final_options:
            # for each suitable VapourtecRS400, query the KG to get the HPLC
            # TODO [futre work] add support for other analytical instruments
            associated_hplc = self.get_hplc_given_vapourtec_rs400(rs400.instance_iri)
            if associated_hplc.is_suitable_for_reaction_experiment(rxnexp):
                return rs400, associated_hplc

        return None, None

    def update_vapourtec_rs400_state(self, vapourtec_rs400_iri: str, target_state: str, timestamp: float):
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        target_state = trimIRI(target_state)
        if target_state in LIST_ONTOVAPOURTEC_VALIDSTATE:
            update = PREFIX_RDF + """DELETE {?state rdf:type ?stateType. ?state <%s> ?timestamp.}
                    INSERT {?state rdf:type <%s>. ?state <%s> %f.}
                    WHERE {<%s> <%s> ?state. ?state rdf:type ?stateType; <%s> ?timestamp.}""" % (
                        ONTOLAB_STATELASTUPDATEDAT, target_state,
                        ONTOLAB_STATELASTUPDATEDAT, timestamp,
                        vapourtec_rs400_iri, SAREF_HASSTATE, ONTOLAB_STATELASTUPDATEDAT
                    )
            self.performUpdate(update)
        else:
            raise Exception("Target state <%s> is not recognised as a valid state for VapourtecRS400, the intended instance of VapourtecRS400 is <%s>." % (target_state, vapourtec_rs400_iri))

    def get_autosampler(self, autosampler_iri: str) -> AutoSampler:
        autosampler_iri = trimIRI(autosampler_iri)
        autosampler = self.get_all_autosampler_with_fill(given_autosampler_iri=autosampler_iri)
        if len(autosampler) == 1:
            return autosampler[0]
        else:
            raise Exception(f"AutoSampler <{autosampler_iri}> is not uniquely identified in the knowledge graph, retrieved results: {str(autosampler)} when querying {self.construct_query_for_autosampler(autosampler_iri)}")

    def get_vapourtec_rs400(
        self,
        list_vapourtec_rs400_iri: Union[str, list]=None,
        list_of_labs_as_constraint: list=None
    ) -> List[VapourtecRS400]:
        list_vapourtec_rs400_iri = trimIRI([list_vapourtec_rs400_iri] if not isinstance(list_vapourtec_rs400_iri, list) else list_vapourtec_rs400_iri) if list_vapourtec_rs400_iri is not None else None
        list_of_labs_as_constraint = trimIRI(list_of_labs_as_constraint) if list_of_labs_as_constraint is not None else None
        query = f"""SELECT ?rs400 ?rs400_manufacturer ?rs400_power_supply ?state
                           ?state_type ?last_update ?autosampler ?is_managed_by
                           ?collection_method ?collection_method_type ?waste_receptacle
                           ?recommended_reaction_scale ?recommended_reaction_scale_measure ?recommended_reaction_scale_unit ?recommended_reaction_scale_val
                WHERE {{
                    {"VALUES ?rs400 { <%s> } ." % '> <'.join(list_vapourtec_rs400_iri) if list_vapourtec_rs400_iri is not None else ""}
                    {"VALUES ?laboratory { <%s> } ." % '> <'.join(list_of_labs_as_constraint) if list_of_labs_as_constraint is not None else ""}
                    ?laboratory <{ONTOLAB_CONTAINS}> ?rs400.
                    ?rs400 <{SAREF_CONSISTSOF}> ?autosampler;
                           <{DBPEDIA_MANUFACTURER}> ?rs400_manufacturer;
                           <{ONTOLAB_HASPOWERSUPPLY}> ?rs400_power_supply;
                           <{SAREF_HASSTATE}> ?state.
                    ?state a ?state_type; <{ONTOLAB_STATELASTUPDATEDAT}> ?last_update.
                    ?autosampler a <{ONTOVAPOURTEC_AUTOSAMPLER}>.
                    ?rs400 <{ONTOVAPOURTEC_HASCOLLECTIONMETHOD}> ?collection_method.
                    ?collection_method a ?collection_method_type.
                    OPTIONAL{{?collection_method <{ONTOVAPOURTEC_TORECEPTACLE}> ?waste_receptacle.}}
                    OPTIONAL {{
                        ?rs400 <{ONTOVAPOURTEC_RECOMMENDEDREACTIONSCALE}> ?recommended_reaction_scale.
                        ?recommended_reaction_scale <{OM_HASVALUE}> ?recommended_reaction_scale_measure. ?recommended_reaction_scale_measure <{OM_HASUNIT}> ?recommended_reaction_scale_unit; <{OM_HASNUMERICALVALUE}> ?recommended_reaction_scale_val.
                    }}
                    OPTIONAL{{?rs400 <{ONTOLAB_ISMANAGEDBY}> ?is_managed_by.}}
                }}"""

        response = self.performQuery(query)

        if len(response) == 0: return None # TODO [nice-to-have] add more information and decide whether to throw an exception

        list_obtained_rs400_iri = dal.get_unique_values_in_list_of_dict(response, 'rs400')
        list_vapourtec_rs400_instance = []
        for rs400_iri in list_obtained_rs400_iri:
            _info = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'rs400', rs400_iri)
            # NOTE here we are assuming one VapourtecRS400 module has only ONE manufacturer, locates in only ONE laboratory, also has only ONE type of power supply
            # NOTE this might not hold universally, but we will simplify for the moment
            if len(_info) > 1:
                raise Exception(f"One VapourtecRS400 ({rs400_iri}) should only consist of one AutoSampler module. Identified multiple: {str(_info)}")
            else:
                res = _info[0]

            list_vapourtec_reactor_and_pump = []
            list_vapourtec_reactor_and_pump.append(self.get_autosampler(res['autosampler']))
            list_vapourtec_reactor_and_pump += self.get_r4_reactor_given_vapourtec_rs400(rs400_iri)
            list_vapourtec_reactor_and_pump += self.get_r2_pump_given_vapourtec_rs400(rs400_iri)

            vapourtec_rs400 = VapourtecRS400(
                instance_iri=rs400_iri,
                manufacturer=res['rs400_manufacturer'],
                hasPowerSupply=res['rs400_power_supply'],
                isManagedBy=res['is_managed_by'] if 'is_managed_by' in res else None,
                consistsOf=list_vapourtec_reactor_and_pump,
                hasState=VapourtecState(
                    instance_iri=res['state'],
                    clz=res['state_type'],
                    stateLastUpdatedAt=res['last_update']
                ),
                hasCollectionMethod=CollectionMethod(
                    instance_iri=res['collection_method'],
                    clz=res['collection_method_type'],
                    toReceptacle=res.get('waste_receptacle'),
                ),
                recommendedReactionScale=OM_Volume(
                    instance_iri=res['recommended_reaction_scale'],
                    hasValue=OM_Measure(instance_iri=res['recommended_reaction_scale_measure'],hasUnit=res['recommended_reaction_scale_unit'],hasNumericalValue=res['recommended_reaction_scale_val'])
                ) if 'recommended_reaction_scale' in res else None,
            )
            list_vapourtec_rs400_instance.append(vapourtec_rs400)

        return list_vapourtec_rs400_instance

    def get_vapourtec_rs400_given_autosampler(self, autosampler: AutoSampler) -> VapourtecRS400:
        query = f"""{PREFIX_RDF}
                SELECT ?rs400 ?rs400_manufacturer ?rs400_power_supply ?state ?state_type ?last_update ?is_managed_by
                ?collection_method ?collection_method_type ?waste_receptacle
                ?recommended_reaction_scale ?recommended_reaction_scale_measure ?recommended_reaction_scale_unit ?recommended_reaction_scale_val
                WHERE {{
                    ?rs400 <{SAREF_CONSISTSOF}> <{autosampler.instance_iri}>;
                            rdf:type <{ONTOVAPOURTEC_VAPOURTECRS400}>;
                            <{DBPEDIA_MANUFACTURER}> ?rs400_manufacturer;
                            <{ONTOLAB_HASPOWERSUPPLY}> ?rs400_power_supply;
                            <{SAREF_HASSTATE}> ?state.
                    ?state a ?state_type; <{ONTOLAB_STATELASTUPDATEDAT}> ?last_update.
                    ?rs400 <{ONTOVAPOURTEC_HASCOLLECTIONMETHOD}> ?collection_method.
                    ?collection_method a ?collection_method_type.
                    OPTIONAL{{?collection_method <{ONTOVAPOURTEC_TORECEPTACLE}> ?waste_receptacle.}}
                    OPTIONAL{{?rs400 <{ONTOLAB_ISMANAGEDBY}> ?is_managed_by.}}
                    OPTIONAL {{
                        ?rs400 <{ONTOVAPOURTEC_RECOMMENDEDREACTIONSCALE}> ?recommended_reaction_scale.
                        ?recommended_reaction_scale <{OM_HASVALUE}> ?recommended_reaction_scale_measure. ?recommended_reaction_scale_measure <{OM_HASUNIT}> ?recommended_reaction_scale_unit; <{OM_HASNUMERICALVALUE}> ?recommended_reaction_scale_val.
                    }}
                }}"""

        response = self.performQuery(query)

        # NOTE here we are assuming one VapourtecRS400 module has only ONE manufacturer, locates in only ONE laboratory, also has only ONE type of power supply
        # NOTE this might not hold universally, but we will simplify for the moment
        if len(response) > 1:
            raise Exception("One AutoSampler (%s) should only be attached to one VapourtecRS400 module. Identified multiple: %s" % (autosampler.instance_iri, str(response)))
        elif len(response) < 1:
            raise Exception("The given AutoSampler (%s) is not associated with any instances of VapourtecRS400 module." % (autosampler.instance_iri))
        else:
            res = response[0]

        list_vapourtec_reactor_and_pump = []
        list_vapourtec_reactor_and_pump.append(autosampler)
        list_vapourtec_reactor_and_pump += self.get_r4_reactor_given_vapourtec_rs400(res['rs400'])
        list_vapourtec_reactor_and_pump += self.get_r2_pump_given_vapourtec_rs400(res['rs400'])

        vapourtec_rs400 = VapourtecRS400(
            instance_iri=res['rs400'],
            manufacturer=res['rs400_manufacturer'],
            hasPowerSupply=res['rs400_power_supply'],
            isManagedBy=res['is_managed_by'] if 'is_managed_by' in res else None,
            consistsOf=list_vapourtec_reactor_and_pump,
            hasState=VapourtecState(
                instance_iri=res['state'],
                clz=res['state_type'],
                stateLastUpdatedAt=res['last_update']
            ),
            hasCollectionMethod=CollectionMethod(
                instance_iri=res['collection_method'],
                clz=res['collection_method_type'],
                toReceptacle=res.get('waste_receptacle'),
            ),
            recommendedReactionScale=OM_Volume(
                instance_iri=res['recommended_reaction_scale'],
                hasValue=OM_Measure(instance_iri=res['recommended_reaction_scale_measure'],hasUnit=res['recommended_reaction_scale_unit'],hasNumericalValue=res['recommended_reaction_scale_val'])
            ) if 'recommended_reaction_scale' in res else None,
        )

        return vapourtec_rs400

    # TODO add unit test
    def vapourtec_rs400_is_running_reaction(self, vapourtec_rs400_iri: str) -> bool:
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        query = f"""ASK {{<{vapourtec_rs400_iri}> <{SAREF_HASSTATE}> ?state. ?state a <{ONTOVAPOURTEC_RUNNINGREACTION}>.}}"""
        response = self.performQuery(query)
        return response[0]['ASK']

    def get_rxn_exp_assigned_to_r4_reactor(self, r4_reactor_iri: str) -> List[str]:
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        query = """
                SELECT ?rxnexp WHERE {?rxnexp <%s> <%s>.}
                """ % (ONTOREACTION_ISASSIGNEDTO, r4_reactor_iri)
        response = self.performQuery(query)
        list_rxn = [res['rxnexp'] for res in response]
        return list_rxn

    def get_r4_reactor_rxn_exp_assigned_to(self, rxn_exp_iris: str) -> str:
        if not isinstance(rxn_exp_iris, list):
            rxn_exp_iris = [rxn_exp_iris]
        rxn_exp_iris = trimIRI(rxn_exp_iris)
        query = f"""SELECT DISTINCT ?rxn ?r4_reactor
                    WHERE {{
                        VALUES ?rxn {{ <{'> <'.join(rxn_exp_iris)}> }}
                        ?rxn <{ONTOREACTION_ISASSIGNEDTO}> ?r4_reactor.
                }}"""
        response_of_all_rxn = self.performQuery(query)
        dict_reactor = {}
        for iri in rxn_exp_iris:
            response = dal.get_sublist_in_list_of_dict_matching_key_value(response_of_all_rxn, 'rxn', iri)
            r4_reactor_iri = [res['r4_reactor'] for res in response]
            if len(r4_reactor_iri) > 1:
                raise Exception(f"ReactionExperiment <{iri}> is assigned to multiple VapourtecR4Reactor: {r4_reactor_iri}")
            dict_reactor[iri] = r4_reactor_iri[0] if len(r4_reactor_iri) == 1 else None
        return dict_reactor

    def get_r4_reactor_given_vapourtec_rs400(self, vapourtec_rs400_iri: str) -> List[VapourtecR4Reactor]:
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        query = PREFIX_RDF + \
                """
                SELECT ?r4_reactor ?r4_reactor_manufacturer ?r4_reactor_power_supply ?loc ?r4_reactor_material
                ?r4_reactor_internal_diameter ?r4_reactor_internal_diameter_measure ?r4_reactor_internal_diameter_unit ?r4_reactor_internal_diameter_val
                ?r4_reactor_length ?r4_reactor_length_measure ?r4_reactor_length_unit ?r4_reactor_length_val
                ?r4_reactor_volume ?r4_reactor_volume_measure ?r4_reactor_volume_unit ?r4_reactor_volume_val
                ?r4_reactor_temp_lower ?r4_reactor_temp_lower_measure ?r4_reactor_temp_lower_unit ?r4_reactor_temp_lower_val
                ?r4_reactor_temp_upper ?r4_reactor_temp_upper_measure ?r4_reactor_temp_upper_unit ?r4_reactor_temp_upper_val
                WHERE {
                    <%s> <%s> ?r4_reactor .
                    ?r4_reactor rdf:type <%s>; <%s> ?r4_reactor_manufacturer; <%s> ?r4_reactor_power_supply; <%s> ?loc;
                    <%s> ?r4_reactor_material; <%s> ?r4_reactor_internal_diameter; <%s> ?r4_reactor_length;
                    <%s> ?r4_reactor_volume; <%s> ?r4_reactor_temp_lower; <%s> ?r4_reactor_temp_upper.
                    ?r4_reactor_internal_diameter <%s> ?r4_reactor_internal_diameter_measure. ?r4_reactor_internal_diameter_measure <%s> ?r4_reactor_internal_diameter_unit; <%s> ?r4_reactor_internal_diameter_val.
                    ?r4_reactor_length <%s> ?r4_reactor_length_measure. ?r4_reactor_length_measure <%s> ?r4_reactor_length_unit; <%s> ?r4_reactor_length_val.
                    ?r4_reactor_volume <%s> ?r4_reactor_volume_measure. ?r4_reactor_volume_measure <%s> ?r4_reactor_volume_unit; <%s> ?r4_reactor_volume_val.
                    ?r4_reactor_temp_lower <%s> ?r4_reactor_temp_lower_measure. ?r4_reactor_temp_lower_measure <%s> ?r4_reactor_temp_lower_unit; <%s> ?r4_reactor_temp_lower_val.
                    ?r4_reactor_temp_upper <%s> ?r4_reactor_temp_upper_measure. ?r4_reactor_temp_upper_measure <%s> ?r4_reactor_temp_upper_unit; <%s> ?r4_reactor_temp_upper_val.
                }
                """ % (
                    vapourtec_rs400_iri, SAREF_CONSISTSOF, ONTOVAPOURTEC_VAPOURTECR4REACTOR, DBPEDIA_MANUFACTURER, ONTOLAB_HASPOWERSUPPLY, ONTOVAPOURTEC_LOCATIONID,
                    ONTOVAPOURTEC_HASREACTORMATERIAL, ONTOVAPOURTEC_HASINTERNALDIAMETER, ONTOVAPOURTEC_HASREACTORLENGTH,
                    ONTOVAPOURTEC_HASREACTORVOLUME, ONTOVAPOURTEC_HASREACTORTEMPERATURELOWERLIMIT, ONTOVAPOURTEC_HASREACTORTEMPERATUREUPPERLIMIT,
                    OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                    OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                    OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                    OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE,
                    OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE
                )

        response = self.performQuery(query)
        unique_r4_reactor_list = dal.get_unique_values_in_list_of_dict(response, 'r4_reactor')
        logger.debug(f"The list of all OntoVapourtec:VapourtecR4Reactor associated with the given instance of OntoVapourtec:VapourtecRS400 {vapourtec_rs400_iri}: {str(unique_r4_reactor_list)}")
        list_r4_reactor = []
        for specific_r4_reactor in unique_r4_reactor_list:
            info_of_specific_r4_reactor = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'r4_reactor', specific_r4_reactor)
            if len(info_of_specific_r4_reactor) > 1:
                raise Exception("Multiple records of OntoVapourtec:VapourtecR4Reactor <%s> are identified: %s" % (specific_r4_reactor, str(info_of_specific_r4_reactor)))
            elif len(info_of_specific_r4_reactor) < 1:
                raise Exception("No record of OntoVapourtec:VapourtecR4Reactor <%s> are identified." % (specific_r4_reactor))
            else:
                info_ = info_of_specific_r4_reactor[0]

            r4_reactor = VapourtecR4Reactor(
                instance_iri=info_['r4_reactor'],
                manufacturer=info_['r4_reactor_manufacturer'],
                hasPowerSupply=info_['r4_reactor_power_supply'],
                locationID=info_['loc'],
                hasReactorMaterial=info_['r4_reactor_material'],
                hasInternalDiameter=OM_Diameter(
                    instance_iri=info_['r4_reactor_internal_diameter'],
                    hasValue=OM_Measure(instance_iri=info_['r4_reactor_internal_diameter_measure'],hasUnit=info_['r4_reactor_internal_diameter_unit'],hasNumericalValue=info_['r4_reactor_internal_diameter_val'])
                ),
                hasReactorLength=OM_Length(
                    instance_iri=info_['r4_reactor_length'],
                    hasValue=OM_Measure(instance_iri=info_['r4_reactor_length_measure'],hasUnit=info_['r4_reactor_length_unit'],hasNumericalValue=info_['r4_reactor_length_val'])
                ),
                hasReactorVolume=OM_Volume(
                    instance_iri=info_['r4_reactor_volume'],
                    hasValue=OM_Measure(instance_iri=info_['r4_reactor_volume_measure'],hasUnit=info_['r4_reactor_volume_unit'],hasNumericalValue=info_['r4_reactor_volume_val'])
                ),
                hasReactorTemperatureLowerLimit=OM_CelsiusTemperature(
                    instance_iri=info_['r4_reactor_temp_lower'],
                    hasValue=OM_Measure(instance_iri=info_['r4_reactor_temp_lower_measure'],hasUnit=info_['r4_reactor_temp_lower_unit'],hasNumericalValue=info_['r4_reactor_temp_lower_val'])
                ),
                hasReactorTemperatureUpperLimit=OM_CelsiusTemperature(
                    instance_iri=info_['r4_reactor_temp_upper'],
                    hasValue=OM_Measure(instance_iri=info_['r4_reactor_temp_upper_measure'],hasUnit=info_['r4_reactor_temp_upper_unit'],hasNumericalValue=info_['r4_reactor_temp_upper_val'])
                ),
            )
            list_r4_reactor.append(r4_reactor)

        return list_r4_reactor

    def get_r2_pump_given_vapourtec_rs400(self, vapourtec_rs400_iri: str) -> List[VapourtecR2Pump]:
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        query = f"""SELECT ?r2_pump ?r2_pump_manufacturer ?r2_pump_power_supply ?loc ?reagent_bottle
                WHERE {{
                    <{vapourtec_rs400_iri}> <{SAREF_CONSISTSOF}> ?r2_pump .
                    ?r2_pump a <{ONTOVAPOURTEC_VAPOURTECR2PUMP}>;
                             <{DBPEDIA_MANUFACTURER}> ?r2_pump_manufacturer;
                             <{ONTOLAB_HASPOWERSUPPLY}> ?r2_pump_power_supply;
                             <{ONTOVAPOURTEC_LOCATIONID}> ?loc.
                    OPTIONAL{{?r2_pump <{ONTOVAPOURTEC_HASREAGENTSOURCE}> ?reagent_bottle.}}
                }}"""

        response = self.performQuery(query)
        unique_r2_pump_list = dal.get_unique_values_in_list_of_dict(response, 'r2_pump')
        logger.debug(f"The list of all OntoVapourtec:VapourtecR2Pump associated with the given instance of OntoVapourtec:VapourtecRS400 {vapourtec_rs400_iri}: {str(unique_r2_pump_list)}")
        list_r2_pump = []
        for specific_r2_pump in unique_r2_pump_list:
            info_of_specific_r2_pump = dal.get_sublist_in_list_of_dict_matching_key_value(response, 'r2_pump', specific_r2_pump)

            if len(info_of_specific_r2_pump) > 1:
                raise Exception("Multiple records of OntoVapourtec:VapourtecR4Reactor <%s> are identified: %s" % (specific_r2_pump, str(info_of_specific_r2_pump)))
            elif len(info_of_specific_r2_pump) < 1:
                raise Exception("No record of OntoVapourtec:VapourtecR4Reactor <%s> are identified." % (specific_r2_pump))
            else:
                info_ = info_of_specific_r2_pump[0]

            r2_pump = VapourtecR2Pump(
                instance_iri=info_['r2_pump'],
                manufacturer=info_['r2_pump_manufacturer'],
                hasPowerSupply=info_['r2_pump_power_supply'],
                locationID=info_['loc'],
                hasReagentSource=self.get_reagent_bottle(info_['reagent_bottle']) if 'reagent_bottle' in info_ else None,
            )
            list_r2_pump.append(r2_pump)

        return list_r2_pump

    def get_reagent_bottle(self, reagent_bottle_iri: str) -> ReagentBottle:
        reagent_bottle_iri = trimIRI(reagent_bottle_iri)
        query = f"""SELECT ?reagent_bottle ?chemical_amount
        ?fill_level ?fill_level_om_value ?fill_level_unit ?fill_level_num_val
        ?max_level ?max_level_om_value ?max_level_unit ?max_level_num_val
        ?warn_level ?warn_level_om_value ?warn_level_unit ?warn_level_num_val
        ?contains_unidentified_component
        WHERE {{
            VALUES ?reagent_bottle {{ <{reagent_bottle_iri}> }}
            ?reagent_bottle <{ONTOLAB_HASFILLLEVEL}> ?fill_level.
            ?fill_level <{OM_HASVALUE}> ?fill_level_om_value.
            ?fill_level_om_value <{OM_HASUNIT}> ?fill_level_unit; <{OM_HASNUMERICALVALUE}> ?fill_level_num_val.

            ?reagent_bottle <{ONTOLAB_HASMAXLEVEL}> ?max_level.
            ?max_level <{OM_HASVALUE}> ?max_level_om_value.
            ?max_level_om_value <{OM_HASUNIT}> ?max_level_unit; <{OM_HASNUMERICALVALUE}> ?max_level_num_val.

            ?reagent_bottle <{ONTOLAB_HASWARNINGLEVEL}> ?warn_level.
            ?warn_level <{OM_HASVALUE}> ?warn_level_om_value.
            ?warn_level_om_value <{OM_HASUNIT}> ?warn_level_unit; <{OM_HASNUMERICALVALUE}> ?warn_level_num_val.

            ?reagent_bottle <{ONTOLAB_ISFILLEDWITH}> ?chemical_amount.
            ?chemical_amount <{ONTOLAB_CONTAINSUNIDENTIFIEDCOMPONENT}> ?contains_unidentified_component.
        }}"""

        response = self.performQuery(query)
        if len(response) != 1:
            raise Exception(f"ReagentBottle <{reagent_bottle_iri}> is not uniquely identified, found: {response}")
        res = response[0]

        return ReagentBottle(
            instance_iri=res['reagent_bottle'],
            isFilledWith=ChemicalAmount(
                instance_iri=res['chemical_amount'],
                refersToMaterial=self.get_chemical(res['chemical_amount'], ONTOCAPE_REFERSTOMATERIAL, ONTOREACTION_CHEMICAL)[res['chemical_amount']][0],
                fills=res['reagent_bottle'],
                isPreparedBy=None, # TODO [future work] add support for isPreparedBy
                containsUnidentifiedComponent=res['contains_unidentified_component'],
            ),
            hasFillLevel=OM_Volume(
                instance_iri=res['fill_level'],
                hasValue=OM_Measure(
                    instance_iri=res['fill_level_om_value'],
                    hasUnit=res['fill_level_unit'],
                    hasNumericalValue=res['fill_level_num_val']
                )
            ),
            hasWarningLevel=OM_Volume(
                instance_iri=res['warn_level'],
                hasValue=OM_Measure(
                    instance_iri=res['warn_level_om_value'],
                    hasUnit=res['warn_level_unit'],
                    hasNumericalValue=res['warn_level_num_val']
                )
            ),
            hasMaxLevel=OM_Volume(
                instance_iri=res['max_level'],
                hasValue=OM_Measure(
                    instance_iri=res['max_level_om_value'],
                    hasUnit=res['max_level_unit'],
                    hasNumericalValue=res['max_level_num_val']
                )
            ),
        )

    def assign_rxn_exp_to_r4_reactor(self, rxn_exp_iri: str, r4_reactor_iri: str):
        rxn_exp_iri = trimIRI(rxn_exp_iri)
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        update = """INSERT DATA {<%s> <%s> <%s>}""" % (rxn_exp_iri, ONTOREACTION_ISASSIGNEDTO, r4_reactor_iri)
        self.performUpdate(update)
        logger.info(f"ReactionExperiment {rxn_exp_iri} is now assigned to VapourtecR4Reactor {r4_reactor_iri}.")

    def remove_rxn_exp_from_r4_reactor(self, rxn_exp_iri: str, r4_reactor_iri: str):
        rxn_exp_iri = trimIRI(rxn_exp_iri)
        r4_reactor_iri = trimIRI(r4_reactor_iri)
        update = """DELETE DATA {<%s> <%s> <%s>}""" % (rxn_exp_iri, ONTOREACTION_ISASSIGNEDTO, r4_reactor_iri)
        self.performUpdate(update)
        logger.info(f"ReactionExperiment {rxn_exp_iri} is no longer assigned to VapourtecR4Reactor {r4_reactor_iri}.")

    def get_prior_rxn_exp_in_queue(self, rxn_exp_iri: str, vapourtec_execution_agent_iri: str):
        """This method queries the instances of ReactionExperiment that are prior in the queue for execution.
        NOTE: It is assumed there is only ONE possible OntoAgent:Service of DoE Agent. This can be extended if deemed necessary in the future."""
        # TODO [future work] support query prior experiments in the situation of multiple DoE Agent available
        rxn_exp_iri = trimIRI(rxn_exp_iri)
        vapourtec_execution_agent_iri = trimIRI(vapourtec_execution_agent_iri)
        query = PREFIX_RDF + """
                SELECT ?rxn ?timestamp ?reactor
                WHERE {
                    <%s> <%s> ?dd. ?dd <%s> ?doe_agent; <%s>/<%s>/<%s> ?specific_timestamp.
                    VALUES ?type {<%s> <%s>}.
                    ?rxn rdf:type ?type; <%s> ?doe_derivation.
                    ?doe_derivation <%s> ?doe_agent; <%s>/<%s>/<%s> ?timestamp.
                    ?rxn ^<%s> ?exe_derivation.
                    OPTIONAL {?rxn <%s> ?reactor.}
                    ?exe_derivation <%s> <%s>.
                    ?exe_derivation <%s>/rdf:type ?status_type.
                    filter(?timestamp < ?specific_timestamp)
                }
                """ % (rxn_exp_iri, ONTODERIVATION_BELONGSTO, ONTODERIVATION_ISDERIVEDUSING, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION,
                ONTOREACTION_REACTIONEXPERIMENT, ONTOREACTION_REACTIONVARIATION, ONTODERIVATION_BELONGSTO,
                ONTODERIVATION_ISDERIVEDUSING, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION,
                ONTODERIVATION_ISDERIVEDFROM, ONTOREACTION_ISASSIGNEDTO, ONTODERIVATION_ISDERIVEDUSING, vapourtec_execution_agent_iri, ONTODERIVATION_HASSTATUS)

        response = self.performQuery(query)
        rxn_exp_queue = {
            res['rxn']:{
                TIME_NUMERICPOSITION:res['timestamp'],
                ONTOREACTION_ISASSIGNEDTO:res['reactor'] if 'reactor' in res else None
            } for res in response
        }
        return rxn_exp_queue

    def detect_new_hplc_report(self, hplc_digital_twin, start_timestamp, end_timestamp):
        query = """SELECT ?hplc_report WHERE { <%s> <%s> ?hplc_report. ?hplc_report <%s> ?timestamp.
                FILTER(?timestamp > %f && ?timestamp < %f)}""" % (
            hplc_digital_twin, ONTOHPLC_HASPASTREPORT, ONTOHPLC_LASTUPLOADEDAT, start_timestamp, end_timestamp)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple HPLCReport found between given period of time for HPLC <%s>: %s" % (hplc_digital_twin, str(response)))
        elif len(response) < 1:
            return None
        else:
            return response[0]['hplc_report']

    def collect_triples_for_hplc_job(
        self,
        rxn_exp_iri, chemical_amount_iri,
        hplc_digital_twin, hplc_report_iri, hplc_method_iri,
        g: Graph
    ):
        hplc_job_iri = initialiseInstanceIRI(getNameSpace(hplc_digital_twin), ONTOHPLC_HPLCJOB)
        logger.info(f"The initialised HPLCJob IRI is: {hplc_job_iri}")

        hplc_method_iri = trimIRI(hplc_method_iri)
        logger.info(f"The HPLCReport {hplc_report_iri} was generated using HPLCMethod {hplc_method_iri}")

        g.add((URIRef(hplc_digital_twin), URIRef(ONTOHPLC_HASJOB), URIRef(hplc_job_iri)))
        g.add((URIRef(hplc_job_iri), RDF.type, URIRef(ONTOHPLC_HPLCJOB)))
        g.add((URIRef(hplc_job_iri), URIRef(ONTOHPLC_CHARACTERISES), URIRef(rxn_exp_iri)))
        g.add((URIRef(hplc_job_iri), URIRef(ONTOHPLC_USESMETHOD), URIRef(hplc_method_iri)))
        g.add((URIRef(hplc_job_iri), URIRef(ONTOHPLC_HASREPORT), URIRef(hplc_report_iri)))
        g.add((URIRef(hplc_report_iri), URIRef(ONTOHPLC_GENERATEDFOR), URIRef(chemical_amount_iri)))
        return g

    def upload_raw_hplc_report_to_kg(self, local_file_path, timestamp_last_modified, remote_report_subdir, hplc_digital_twin) -> str:
        _remote_file_subdir = remote_report_subdir.replace(':', '').replace('\\', '/').replace(' ', '_') if remote_report_subdir is not None else None
        # repeat the upload process every 10 secs until the file is uploaded successfully
        file_uploaded = False
        while not file_uploaded:
            try:
                remote_file_path, timestamp_upload = self.uploadFile(local_file_path, _remote_file_subdir)
                logger.info(f"HPLC raw report ({local_file_path}) was uploaded to fileserver {self.fs_url} at {timestamp_upload} with remote file path at: {remote_file_path}")
            except Exception as e:
                logger.error(f"HPLC raw report ({local_file_path}) upload failed. Retrying in 10 seconds...", exc_info=True)
                time.sleep(10)
            else:
                file_uploaded = True

        hplc_report_iri = initialiseInstanceIRI(getNameSpace(hplc_digital_twin), ONTOHPLC_HPLCREPORT)
        logger.info(f"The initialised HPLCReport IRI is: {hplc_report_iri}")

        update = f"""{PREFIX_XSD} INSERT DATA {{
            <{hplc_report_iri}> a <{ONTOHPLC_HPLCREPORT}>; <{ONTOHPLC_REMOTEFILEPATH}> "{remote_file_path}"^^xsd:anyURI;
            <{ONTOHPLC_LOCALFILEPATH}> "{local_file_path}"^^xsd:string; <{ONTOHPLC_LASTLOCALMODIFIEDAT}> {timestamp_last_modified};
            <{ONTOHPLC_LASTUPLOADEDAT}> {timestamp_upload}. <{hplc_digital_twin}> <{ONTOHPLC_HASPASTREPORT}> <{hplc_report_iri}>.
        }}""".replace("\\", "\\\\")
        # repeat the update process in KG every 10 secs until the update is successful
        kg_updated = False
        while not kg_updated:
            try:
                self.performUpdate(update)
            except Exception as e:
                logger.error(f"SPARQL update to write information about the uploaded HPLCReport (remote file path: {remote_file_path}) failed: {update}. Retrying in 10 seconds...", exc_info=True)
                time.sleep(10)
            else:
                kg_updated = True

        return hplc_report_iri

    def register_agent_with_hardware(self, agent_iri, hardware_digital_twin):
        g = Graph()
        g.add((URIRef(hardware_digital_twin), URIRef(ONTOLAB_ISMANAGEDBY), URIRef(agent_iri)))
        # NOTE SPARQL update with sub-query to ensure same triple don't get written to KG (although this should not cause any issues)
        update = """INSERT { %s } WHERE { FILTER NOT EXISTS { %s } }""" % (
            g.serialize(format='nt'), g.serialize(format='nt')
        )
        self.performUpdate(update)

    def identify_rxn_exp_when_uploading_hplc_report(self, hplc_digital_twin: str, hplc_remote_file_path: str) -> str:
        hplc_digital_twin = trimIRI(hplc_digital_twin)
        query = """SELECT DISTINCT ?rxn_exp WHERE {<%s> ^<%s>/<%s>+/^<%s>/<%s> ?rxn_exp.}""" % (
            hplc_digital_twin, SAREF_CONSISTSOF, SAREF_CONSISTSOF, ONTOLAB_SPECIFIES, ONTOLAB_WASGENERATEDFOR)
        response = self.performQuery(query)
        rxn_exp = [list(res.values())[0] for res in response]
        if len(rxn_exp) > 1:
            raise Exception("Multiple instances of ReactionExperiment is identified to be associated with HPLC <%s> when uploading HPLCReport <%s>: %s" % (
                hplc_digital_twin, hplc_remote_file_path, str(response)))
        elif len(rxn_exp) < 1:
            raise Exception("No instance of ReactionExperiment is identified to be associated with HPLC <%s> when uploading HPLCReport <%s>" % (
                hplc_digital_twin, hplc_remote_file_path))
        else:
            return rxn_exp[0]

    def get_raw_hplc_report_remote_path_and_extension(self, hplc_report_iri: str) -> Tuple[str, str]:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = """SELECT ?remote_path ?extension WHERE {<%s> <%s> ?remote_path . ?hplc <%s>/<%s> <%s> . ?hplc <%s> ?extension .}""" % (
            hplc_report_iri, ONTOHPLC_REMOTEFILEPATH, ONTOHPLC_HASJOB, ONTOHPLC_HASREPORT, hplc_report_iri, ONTOHPLC_REPORTEXTENSION)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple matches for filepath and extension for raw HPLC report <%s> was found: %s" % (hplc_report_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No raw HPLC report found for <%s> when querying: %s" % (hplc_report_iri, query))
        else:
            remote_file_path = response[0]['remote_path']
            try:
                file_extension = MAPPING_FILENAMEEXTENSION.get(response[0]['extension'])
            except:
                raise NotImplementedError("Retrieving raw HPLC report with a file extension <%s> is NOT yet supported, associated HPLCReport iri <%s>, remote file path <%s>" % (
                    response[0]['extension'], hplc_report_iri, remote_file_path))
        return remote_file_path, file_extension

    def get_matching_species_from_hplc_results(self, retention_time: RetentionTime, hplc_method: HPLCMethod) -> str:
        # TODO [future work] here we took a shortcut, but in theory unit should also be checked - unit conversion is a generic problem that needs to be solved...
        hplc_rt = {rt.refersToSpecies:rt.hasValue.hasNumericalValue for rt in hplc_method.hasRetentionTime}
        rt_diff = {key: abs(hplc_rt[key] - retention_time.hasValue.hasNumericalValue) for key in hplc_rt}
        key_min_rt_diff = min(rt_diff, key=rt_diff.get)
        if rt_diff.get(key_min_rt_diff) < hplc_method.retentionTimeMatchThreshold:
            return key_min_rt_diff
        else:
            logger.warning(f"No OntoSpecies:Species identified for OntoHPLC:RetentionTime instance ({retention_time.instance_iri}) given RETENTION_TIME_MATCH_THRESHOLD of {hplc_method.retentionTimeMatchThreshold} and OntoHPLC:HPLCMethod ({hplc_method.instance_iri})")
            return None

    def get_internal_standard(self, hplc_method_iri: str) -> InternalStandard:
        hplc_method_iri = trimIRI(hplc_method_iri)
        query = PREFIX_RDF+"""SELECT ?is ?species ?property ?property_type ?value ?unit ?num_val
                   WHERE {
                       VALUES ?type {<%s>}.
                       <%s> <%s> ?is.
                       ?is rdf:type ?type; <%s> ?species; <%s> ?property.
                       ?property rdf:type ?property_type; <%s> ?value.
                       ?value <%s> ?unit; <%s> ?num_val.
                   }""" % (ONTOHPLC_INTERNALSTANDARD, hplc_method_iri, ONTOHPLC_USESINTERNALSTANDARD,
                ONTOCAPE_REPRESENTSOCCURENCEOF, ONTOCAPE_HASPROPERTY, ONTOCAPE_HASVALUE, ONTOCAPE_HASUNITOFMEASURE, ONTOCAPE_NUMERICALVALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise NotImplementedError("Multiple instances of InternalStandard (%s) are identified for HPLCMethod <%s>, this is NOT support yet." % (
                str(response), hplc_method_iri))
        elif len(response) < 1:
            raise Exception("InternalStandard NOT found for HPLCMethod <%s> when querying: %s" % (hplc_method_iri, query))
        else:
            r = response[0]

        if r['property_type'] == OntoCAPE_Molarity.__fields__['clz'].default:
            concentration = OntoCAPE_Molarity(instance_iri=r['property'],
                hasValue=OntoCAPE_ScalarValue(instance_iri=r['value'],numericalValue=r['num_val'],hasUnitOfMeasure=r['unit']))
        else:
            # TODO [future work] add support for other type of OntoCAPE_PhaseComponentConcentration
            raise NotImplementedError("Support for <%s> as OntoCAPE_PhaseComponentConcentration is NOT implemented yet." % r['property_type'])
        internal_standard = InternalStandard(
            instance_iri=r['is'],
            hasProperty=concentration,
            representsOccurenceOf=r['species']
        )
        return internal_standard

    def get_hplc_method_given_hplc_report(self, hplc_report_iri: str) -> HPLCMethod:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = PREFIX_RDF+"""SELECT ?hplc_method WHERE {?hplc_job rdf:type <%s>; <%s> <%s>; <%s> ?hplc_method}""" % (
            ONTOHPLC_HPLCJOB, ONTOHPLC_HASREPORT, hplc_report_iri, ONTOHPLC_USESMETHOD)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple instances identified for HPLCMethod given HPLCReport <%s>: %s" % (hplc_report_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of HPLCMethod found given the HPLCReport <%s>" % hplc_report_iri)
        else:
            return self.get_hplc_method(response[0]['hplc_method'])

    def get_hplc_method(self, hplc_method_iri: str) -> HPLCMethod:
        hplc_method_iri = trimIRI(hplc_method_iri)
        query = f"""
            SELECT ?type ?r ?species ?measure ?unit ?value ?rt_match_threshold
            WHERE {{
                VALUES ?type {{ <{ONTOHPLC_RESPONSEFACTOR}> <{ONTOHPLC_RETENTIONTIME}> }}.
                <{hplc_method_iri}> <{ONTOHPLC_HASRESPONSEFACTOR}>|<{ONTOHPLC_HASRETENTIONTIME}> ?r.
                ?r a ?type; <{ONTOHPLC_REFERSTOSPECIES}> ?species; <{OM_HASVALUE}> ?measure.
                ?measure <{OM_HASUNIT}> ?unit; <{OM_HASNUMERICALVALUE}> ?value.
                <{hplc_method_iri}> <{ONTOHPLC_RETENTIONTIMEMATCHTHRESHOLD}> ?rt_match_threshold.
            }}"""

        response = self.performQuery(query)
        try:
            rt_match_threshold = dal.get_the_unique_value_in_list_of_dict(response, 'rt_match_threshold')
        except Exception as e:
            raise Exception(f"HPLCMethod {hplc_method_iri} has more than one retentionTimeMatchThreshold: {dal.get_unique_values_in_list_of_dict(response, 'rt_match_threshold')}") from e

        list_rf = [] # list of ResponseFactor
        list_rt = [] # list of RetentionTime
        for res in response:
            if res['type'] == ONTOHPLC_RESPONSEFACTOR:
                rf = ResponseFactor(
                    instance_iri=res['r'],
                    hasValue=OM_Measure(
                        instance_iri=res['measure'],
                        hasUnit=res['unit'],
                        hasNumericalValue=res['value']
                    ),
                    refersToSpecies=res['species']
                )
                list_rf.append(rf)
            elif res['type'] == ONTOHPLC_RETENTIONTIME:
                rt = RetentionTime(
                    instance_iri=res['r'],
                    hasValue=OM_Measure(
                        instance_iri=res['measure'],
                        hasUnit=res['unit'],
                        hasNumericalValue=res['value']
                    ),
                    refersToSpecies=res['species']
                )
                list_rt.append(rt)

        comment = self.performQuery(PREFIX_RDFS+"SELECT ?comment WHERE {<%s> rdfs:comment ?comment}" % hplc_method_iri)[0]['comment']
        hplc_method = HPLCMethod(
            instance_iri=hplc_method_iri,
            hasResponseFactor=list_rf,
            hasRetentionTime=list_rt,
            usesInternalStandard=self.get_internal_standard(hplc_method_iri),
            retentionTimeMatchThreshold=rt_match_threshold,
            rdfs_comment=comment
        )
        return hplc_method

    def get_species_molar_mass_kilogrampermole(self, species_iri: str) -> float:
        """This method returns the molecular weight of the given OntoSpecies:Species iri in the unit of kg/mol."""
        species_iri = trimIRI(species_iri)
        query = """SELECT ?unit ?value WHERE { <%s> <%s> ?mw. ?mw <%s> ?unit; <%s> ?value. }""" % (
            species_iri, ONTOSPECIES_HASMOLECULARWEIGHT, ONTOSPECIES_UNITS, ONTOSPECIES_VALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of MolecularWeight were identified for OntoSpecies:Species <%s>: %s" % (species_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of MolecularWeight was identified for OntoSpecies:Species <%s>" % (species_iri))
        else:
            if response[0]['unit'] == ONTOSPECIES_GRAMPERMOLUNIT:
                return float(response[0]['value']) / 1000
            elif response[0]['unit'] == ONTOSPECIES_KILOGRAMPERMOLUNIT:
                return float(response[0]['value'])
            else:
                raise NotImplementedError("Record of MolecularWeight in unit of <%s> is NOT yet supported for OntoSpecies:Species <%s>" % (
                    response[0]['unit'], species_iri))

    # TODO replace this with the proper representation of the species density - at given temperature, what's the density of the given species
    def get_species_density(self, species_iri: str) -> Tuple[float, str]:
        PLACEHOLDER_HASDENSITY = 'https://www.theworldavatar.com/kg/_for_species/hasDensity'
        species_iri = trimIRI(species_iri)
        query = """SELECT ?value ?unit WHERE { <%s> <%s>/<%s> ?density. ?density <%s> ?unit; <%s> ?value. }""" % (
            species_iri, PLACEHOLDER_HASDENSITY, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of Density were identified for OntoSpecies:Species <%s>: %s" % (species_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of Density was identified for OntoSpecies:Species <%s>" % (species_iri))
        else:
            return float(response[0]['value']), response[0]['unit']

    # TODO replace this with the proper representation of the material cost - with the given vendor, what's the cost of the given chemical
    def get_species_material_cost(self, species_iri: str) -> Tuple[float, str]:
        PLACEHOLDER_HASMATERIALCOST = 'https://www.theworldavatar.com/kg/_for_species/hasMaterialCost'
        species_iri = trimIRI(species_iri)
        query = """SELECT ?value ?unit WHERE { <%s> <%s>/<%s> ?cost. ?cost <%s> ?unit; <%s> ?value. }""" % (
            species_iri, PLACEHOLDER_HASMATERIALCOST, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of MaterialCost were identified for OntoSpecies:Species <%s>: %s" % (species_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of MaterialCost was identified for OntoSpecies:Species <%s>" % (species_iri))
        else:
            return float(response[0]['value']), response[0]['unit']

    # TODO replace this with the proper representation of the eco score - not sure if this is an "intrinsic" property of a given chemical
    def get_species_eco_score(self, species_iri: str) -> Tuple[float, str]:
        PLACEHOLDER_HASECOSCORE = 'https://www.theworldavatar.com/kg/_for_species/hasEcoScore'
        species_iri = trimIRI(species_iri)
        query = """SELECT ?value ?unit WHERE { <%s> <%s>/<%s> ?ecoscore. ?ecoscore <%s> ?unit; <%s> ?value. }""" % (
            species_iri, PLACEHOLDER_HASECOSCORE, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of EcoScore were identified for OntoSpecies:Species <%s>: %s" % (species_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of EcoScore was identified for OntoSpecies:Species <%s>" % (species_iri))
        else:
            return float(response[0]['value']), response[0]['unit']

    def get_reactor_volume_given_reactor(self, reactor_iri: str) -> Tuple[float, str]:
        reactor_iri = trimIRI(reactor_iri)
        query = """SELECT ?unit ?value WHERE { <%s> <%s>/<%s> ?measure. ?measure <%s> ?unit; <%s> ?value. }""" % (
            reactor_iri, ONTOVAPOURTEC_HASREACTORVOLUME, OM_HASVALUE, OM_HASUNIT, OM_HASNUMERICALVALUE)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of ReactorVolume were identified for VapourtecR4Reactor <%s>: %s" % (reactor_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of ReactorVolume was identified for VapourtecR4Reactor <%s>" % (reactor_iri))
        else:
            return float(response[0]['value']), response[0]['unit']

    def get_rxn_exp_associated_with_hplc_report(self, hplc_report_iri: str) -> ReactionExperiment:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = """SELECT ?rxn_exp WHERE { ?hplc_job <%s> <%s>; <%s> ?rxn_exp. }""" % (ONTOHPLC_HASREPORT, hplc_report_iri, ONTOHPLC_CHARACTERISES)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of ReactionExperiment were identified for HPLCReport <%s>: %s" % (hplc_report_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of ReactionExperiment was identified for HPLCReport <%s>" % (hplc_report_iri))
        else:
            rxn_exp_iri = response[0]['rxn_exp']

        return self.getReactionExperiment(rxn_exp_iri)[0]

    def get_internal_standard_associated_with_hplc_report(self, hplc_report_iri: str) -> InternalStandard:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = """SELECT ?hplc_method WHERE { ?hplc_job <%s> <%s>; <%s> ?hplc_method. }""" % (ONTOHPLC_HASREPORT, hplc_report_iri, ONTOHPLC_USESMETHOD)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of HPLEMethod were identified for HPLCReport <%s>: %s" % (hplc_report_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No record of HPLCMethod was identified for HPLCReport <%s>" % (hplc_report_iri))
        else:
            hplc_method_iri = response[0]['hplc_method']

        return self.get_internal_standard(hplc_method_iri)

    def process_raw_hplc_report(self, hplc_report_iri: str, internal_standard_species: str, internal_standard_run_conc_moleperlitre: float, temp_local_folder: str=None) -> HPLCReport:
        """Here we can assume that the required information are already provided by the previous agents."""
        remote_hplc_report_path, hplc_report_extension = self.get_raw_hplc_report_remote_path_and_extension(hplc_report_iri)

        if temp_local_folder is None: temp_local_folder = str(Path(__file__).absolute().parent)
        temp_local_file_path = os.path.join(temp_local_folder,f'{str(uuid.uuid4())}.'+hplc_report_extension)
        self.download_remote_raw_hplc_report(remote_hplc_report_path, temp_local_file_path)

        # retrieve a list of points
        list_points = hplc.read_raw_hplc_report_file(hplc_report_iri=hplc_report_iri, file_path=temp_local_file_path, filename_extension=hplc_report_extension)

        # get the instance of HPLCMethod
        hplc_method = self.get_hplc_method_given_hplc_report(hplc_report_iri)
        # create a dict for response factor
        dct_response_factor = {rf.refersToSpecies:rf.hasValue.hasNumericalValue for rf in hplc_method.hasResponseFactor}

        # map them to chromatogram point (qury phase component based on hplc method, and hplc)
        list_concentration = []
        list_phase_component = []
        list_chrom_pts= []
        dct_points = {}
        _flag_unidentified_species = False
        for pt in list_points:
            _identified_species = self.get_matching_species_from_hplc_results(pt.get(ONTOHPLC_RETENTIONTIME), hplc_method)
            # first handle the unidentified species
            if _identified_species is None:
                _flag_unidentified_species = True
                _unidentified_chrom_pt = ChromatogramPoint(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(hplc_report_iri),
                    indicatesComponent=None,
                    hasPeakArea=pt.get(ONTOHPLC_PEAKAREA),
                    atRetentionTime=pt.get(ONTOHPLC_RETENTIONTIME),
                    unidentified=True,
                    rdfs_comment = f"Species unidentified due to peak information not provided in HPLCMethod {hplc_method.instance_iri}.",
                )
                list_chrom_pts.append(_unidentified_chrom_pt)
            else:
                # then add the identified species to dict
                if _identified_species not in dct_points:
                    dct_points[_identified_species] = pt
                else:
                    # if multiple records identified for the same species, pick the one that has larger peak area
                    # all the rest will be marked as unidentified
                    # TODO [next iteration] also consider the height of the peak
                    logger.warning(f"Multiple records of ChromatogramPoint were identified for the same Species <{_identified_species}>: {str([dct_points[_identified_species], pt])}")
                    _rt_ref = hplc_method.get_retention_time_for_species(_identified_species)
                    _previous_pt = dct_points[_identified_species]
                    _pt_peak_area_unit_converted = unit_conv.unit_conversion_return_value(
                       pt[ONTOHPLC_PEAKAREA].hasValue.hasNumericalValue,
                       pt[ONTOHPLC_PEAKAREA].hasValue.hasUnit,
                       _previous_pt[ONTOHPLC_PEAKAREA].hasValue.hasUnit,
                    )
                    if _pt_peak_area_unit_converted >= _previous_pt[ONTOHPLC_PEAKAREA].hasValue.hasNumericalValue:
                        # if the current point has a larger peak area, then the previous one is to be marked as unidentified
                        _mark_as_unidentified = _previous_pt
                        # then repalce the previous one in dct_points with the new one for potentially later concentration calculation
                        dct_points[_identified_species] = pt
                    else:
                        # if the previous point has a larger peak area, then the current one is to be marked as unidentified
                        _mark_as_unidentified = pt
                        # and we don't need to replace it in dct_points

                    # mark the peak that is determined as unidentified
                    _unidentified_chrom_pt = ChromatogramPoint(
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(hplc_report_iri),
                        indicatesComponent=None,
                        hasPeakArea=_mark_as_unidentified.get(ONTOHPLC_PEAKAREA),
                        atRetentionTime=_mark_as_unidentified.get(ONTOHPLC_RETENTIONTIME),
                        unidentified=True,
                        rdfs_comment = f"""Species unidentified due to multiple peaks present within range of {_rt_ref.hasValue.hasNumericalValue} +/- {hplc_method.retentionTimeMatchThreshold} {_rt_ref.hasValue.hasUnit} for species {_rt_ref.refersToSpecies}, and there exist other peaks that have a larger peak area compared to this one. The HPLCMethod used was {hplc_method.instance_iri}.""",
                    )
                    list_chrom_pts.append(_unidentified_chrom_pt)

        try:
            internal_standard_peak_area = dct_points[internal_standard_species][ONTOHPLC_PEAKAREA].hasValue.hasNumericalValue
        except KeyError:
            # TODO maybe instead of throwing an exception, we make a flag to set all performance indicator to be NaN
            # TODO check how to represent NaN in python, if that works for all the rest of the codes
            raise Exception(f"InternalStandard {internal_standard_species} is NOT identified in the end stream associated with HPLCReport {hplc_report_iri}, all ChromatogramPoint: {list_chrom_pts}")

        # iterate through the dct_points which now should only contain the list of peaks that are identified as the species
        for pt in dct_points:
            # calculate concentration based on the peak area and response factor
            if pt not in dct_response_factor:
                # if the species is identified, but the response factor is unknown, then we consider this as unidentified species
                # TODO [next iteration] in theory, a good practice would be making the species as identified, but unquantified, but simplified for now
                _flag_unidentified_species = True
                _unidentified_chrom_pt = ChromatogramPoint(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(hplc_report_iri),
                    indicatesComponent=None,
                    hasPeakArea=dct_points[pt][ONTOHPLC_PEAKAREA],
                    atRetentionTime=dct_points[pt][ONTOHPLC_RETENTIONTIME],
                    unidentified=True,
                    rdfs_comment = f"Species identified as {pt}, but the response factor is not provided in HPLCMethod {hplc_method.instance_iri}. Therefore, annotated as unidentified for simplicity.",
                )
                list_chrom_pts.append(_unidentified_chrom_pt)
                logger.warning(f"ResponseFactor of Species {pt} is not presented in the HPLCMethod {hplc_method.instance_iri}")

            else:
                # means the response factor information is available for this species
                conc_num_val = dct_points[pt][ONTOHPLC_PEAKAREA].hasValue.hasNumericalValue / internal_standard_peak_area * internal_standard_run_conc_moleperlitre / dct_response_factor[pt]
                concentration = OntoCAPE_Molarity(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(hplc_report_iri),
                    hasValue=OntoCAPE_ScalarValue(
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(hplc_report_iri),
                        hasUnitOfMeasure=OM_MOLEPERLITRE,
                        numericalValue=conc_num_val
                    )
                )
                # generate phase component
                phase_component = OntoCAPE_PhaseComponent(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(hplc_report_iri),
                    hasProperty=concentration,
                    representsOccurenceOf=pt
                )

                chrom_pt = ChromatogramPoint(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(hplc_report_iri),
                    indicatesComponent=phase_component,
                    hasPeakArea=dct_points[pt][ONTOHPLC_PEAKAREA],
                    atRetentionTime=dct_points[pt][ONTOHPLC_RETENTIONTIME],
                    rdfs_comment=f"Species identified as {pt}, and the response factor is provided in HPLCMethod {hplc_method.instance_iri}.",
                )
                list_concentration.append(concentration)
                list_phase_component.append(phase_component)
                list_chrom_pts.append(chrom_pt)

        composition = OntoCAPE_Composition(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(hplc_report_iri),
            comprisesDirectly=list_concentration
        )

        # create output chemical instance
        # generate instance_iri for output_chemical (so that it can be used in OntoCAPE_SinglePhase)
        output_chemical_iri = initialiseInstanceIRI(getNameSpace(hplc_report_iri), ONTOREACTION_OUTPUTCHEMICAL)
        output_chemical = OutputChemical(
            instance_iri=output_chemical_iri,
            thermodynamicBehaviour=OntoCAPE_SinglePhase(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=getNameSpace(hplc_report_iri),
                hasStateOfAggregation=OntoCAPE_liquid,
                isComposedOfSubsystem=list_phase_component,
                has_composition=composition,
                representsThermodynamicBehaviorOf=output_chemical_iri
            )
        )

        # create chemical amount instance
        _response = self.performQuery(
            f"""SELECT ?amount ?local_report_file ?last_local_modification ?last_upload ?vial
            WHERE{{
                <{hplc_report_iri}> <{ONTOHPLC_GENERATEDFOR}> ?amount;
                                    <{ONTOHPLC_LOCALFILEPATH}> ?local_report_file;
                                    <{ONTOHPLC_LASTLOCALMODIFIEDAT}> ?last_local_modification;
                                    <{ONTOHPLC_LASTUPLOADEDAT}> ?last_upload.
                ?amount ^<{ONTOLAB_ISFILLEDWITH}> ?vial.
            }}""")
        if len(_response) > 1:
            raise Exception("Multiple instances of ChemicalAmount identified for HPLCReport <%s>: %s" % (hplc_report_iri, str(_response)))
        elif len(_response) < 1:
            raise Exception("No instance of ChemicalAmount identified for HPLCReport <%s>" % hplc_report_iri)
        else:
            chem_amount_iri = _response[0]['amount']
            chem_amount_vial = _response[0]['vial']
            local_report_file = _response[0]['local_report_file']
            last_local_modified = _response[0]['last_local_modification']
            last_upload = _response[0]['last_upload']

        chemical_amount = ChemicalAmount(
            instance_iri=chem_amount_iri,
            refersToMaterial=output_chemical,
            fills=chem_amount_vial,
            isPreparedBy=None, # TODO [future work] add support for isPreparedBy
            # TODO [next iteration, shortcut for now] note that HPLC will not be able to identify all the species
            # therefore, we need to set this flag to True when unidentifiable species is represented in the reaction, e.g. NaOH
            containsUnidentifiedComponent=_flag_unidentified_species,
        )

        # generate hplc report instance
        hplc_report_instance = HPLCReport(
            instance_iri=hplc_report_iri,
            remoteFilePath=remote_hplc_report_path,
            records=list_chrom_pts,
            generatedFor=chemical_amount,
            localFilePath=local_report_file,
            lastLocalModifiedAt=last_local_modified,
            lastUploadedAt=last_upload
        )

        # Remove downloaded temporary file
        os.remove(temp_local_file_path)
        return hplc_report_instance

    def get_chromatogram_point_of_hplc_report(self, hplc_report_iri: str) -> List[ChromatogramPoint]:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = f"""{PREFIX_RDF}
                    SELECT ?pt ?phase_component ?chemical_species ?conc ?conc_type
                    ?conc_value ?conc_unit ?conc_num_val ?area ?area_value ?area_unit ?area_num_val
                    ?rt ?rt_value ?rt_unit ?rt_num_val ?unidentified ?rdfs_comment
                    WHERE {{
                        <{hplc_report_iri}> <{ONTOHPLC_RECORDS}> ?pt.
                        ?pt <{ONTOHPLC_HASPEAKAREA}> ?area; <{ONTOHPLC_ATRETENTIONTIME}> ?rt.
                        ?area <{OM_HASVALUE}> ?area_value. ?area_value <{OM_HASUNIT}> ?area_unit; <{OM_HASNUMERICALVALUE}> ?area_num_val.
                        ?rt <{OM_HASVALUE}> ?rt_value. ?rt_value <{OM_HASUNIT}> ?rt_unit; <{OM_HASNUMERICALVALUE}> ?rt_num_val.
                        OPTIONAL {{
                            ?pt <{ONTOHPLC_INDICATESCOMPONENT}> ?phase_component.
                            ?phase_component <{ONTOCAPE_REPRESENTSOCCURENCEOF}> ?chemical_species.
                            ?phase_component <{ONTOCAPE_HASPROPERTY}> ?conc. ?conc rdf:type ?conc_type; <{ONTOCAPE_HASVALUE}> ?conc_value.
                            ?conc_value <{ONTOCAPE_HASUNITOFMEASURE}> ?conc_unit; <{ONTOCAPE_NUMERICALVALUE}> ?conc_num_val.
                        }}
                        ?pt <{ONTOHPLC_UNIDENTIFIED}> ?unidentified.
                        OPTIONAL {{ ?pt <{RDFS_COMMENT}> ?rdfs_comment. }}
                    }}"""
        response = self.performQuery(query)

        list_chrom_pts = []
        if len(response) != len(dal.get_unique_values_in_list_of_dict(response, 'pt')):
            raise Exception("The ChromatogramPoint of the given HPLCReport <%s> is not uniquely stored: %s" % (
                hplc_report_iri, str(response)))
        for r in response:
            if 'phase_component' in r:
                if r['conc_type'] == OntoCAPE_Molarity.__fields__['clz'].default:
                    concentration = OntoCAPE_Molarity(instance_iri=r['conc'],hasValue=OntoCAPE_ScalarValue(instance_iri=r['conc_value'],numericalValue=r['conc_num_val'],hasUnitOfMeasure=r['conc_unit']))
                else:
                    # TODO [future work] add support for other type of OntoCAPE_PhaseComponentConcentration
                    raise NotImplementedError("Support for <%s> as OntoCAPE_PhaseComponentConcentration is NOT implemented yet." % r['conc_type'])
                _phase_component = OntoCAPE_PhaseComponent(
                    instance_iri=r['phase_component'],
                    hasProperty=concentration,
                    representsOccurenceOf=r['chemical_species']
                )
            else:
                # phase component can be None is the peak is unidentified
                _phase_component = None

            pt = ChromatogramPoint(
                instance_iri=r['pt'],
                indicatesComponent=_phase_component,
                hasPeakArea=PeakArea(
                    instance_iri=r['area'],
                    hasValue=OM_Measure(instance_iri=r['area_value'],hasUnit=r['area_unit'],hasNumericalValue=r['area_num_val'])
                ),
                atRetentionTime=RetentionTime(
                    instance_iri=r['rt'],
                    hasValue=OM_Measure(instance_iri=r['rt_value'],hasUnit=r['rt_unit'],hasNumericalValue=r['rt_num_val'])
                ),
                unidentified=r['unidentified'],
                rdfs_comment=r['rdfs_comment'] if 'rdfs_comment' in r else None,
            )
            list_chrom_pts.append(pt)

        return list_chrom_pts

    def get_existing_hplc_report(self, hplc_report_iri: str) -> HPLCReport:
        hplc_report_iri = trimIRI(hplc_report_iri)
        query = f"""SELECT ?chemical_amount ?report_path ?local_report_file ?lastLocalModifiedAt ?lastUploadedAt ?vial ?contains_unidentified_component
                   WHERE {{
                       <{hplc_report_iri}> <{ONTOHPLC_GENERATEDFOR}> ?chemical_amount; <{ONTOHPLC_REMOTEFILEPATH}> ?report_path;
                       <{ONTOHPLC_LOCALFILEPATH}> ?local_report_file; <{ONTOHPLC_LASTLOCALMODIFIEDAT}> ?lastLocalModifiedAt;
                       <{ONTOHPLC_LASTUPLOADEDAT}> ?lastUploadedAt. ?chemical_amount ^<{ONTOLAB_ISFILLEDWITH}> ?vial.
                       ?chemical_amount <{ONTOLAB_CONTAINSUNIDENTIFIEDCOMPONENT}> ?contains_unidentified_component.
                    }}"""
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple instances of ChemicalAmount or HPLCReport path identified for HPLCReport <%s>: %s" % (
                hplc_report_iri, str(response)
            ))
        elif len(response) < 1:
            raise Exception("No instances of ChemicalAmount or HPLCReport path identified for HPLCReport <%s>" % (hplc_report_iri))
        else:
            chem_amount_iri = response[0]['chemical_amount']
            hplc_report_path = response[0]['report_path']
            local_report_file = response[0]['local_report_file']
            lastLocalModifiedAt =  response[0]['lastLocalModifiedAt']
            lastUploadedAt = response[0]['lastUploadedAt']
            chem_amount_vial = response[0]['vial']
            contains_unidentified_component = response[0]['contains_unidentified_component']

        hplc_report_instance = HPLCReport(
            instance_iri=hplc_report_iri,
            remoteFilePath=hplc_report_path,
            localFilePath=local_report_file,
            lastLocalModifiedAt=lastLocalModifiedAt,
            lastUploadedAt=lastUploadedAt,
            records=self.get_chromatogram_point_of_hplc_report(hplc_report_iri),
            generatedFor=ChemicalAmount(
                instance_iri=chem_amount_iri,
                refersToMaterial=self.get_chemical(chem_amount_iri, ONTOCAPE_REFERSTOMATERIAL, ONTOREACTION_OUTPUTCHEMICAL)[chem_amount_iri][0],
                fills=chem_amount_vial,
                isPreparedBy=None, # TODO [future work] add support for isPreparedBy
                containsUnidentifiedComponent=contains_unidentified_component,
            )
        )

        return hplc_report_instance

    def get_hplc_job_given_hplc_report_iri(self, hplc_report_iri: str) -> HPLCJob:
        hplc_report_instance = self.get_existing_hplc_report(trimIRI(hplc_report_iri))
        return self.get_hplc_job_given_hplc_report_instance(hplc_report_instance)

    def get_hplc_job_given_hplc_report_instance(self, hplc_report_instance: HPLCReport) -> HPLCJob:
        query = """SELECT ?hplc_job ?rxn_exp WHERE {?hplc_job <%s> <%s>; <%s> ?rxn_exp.}""" % (
            ONTOHPLC_HASREPORT, hplc_report_instance.instance_iri, ONTOHPLC_CHARACTERISES)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple instances of HPLCJob/ReactionExperiment is associated with the given HPLCReport <%s>: %s" % (
                hplc_report_instance.instance_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No records of HPLCJob/ReactionExperiment were found with the given HPLCReport instance <%s>" % hplc_report_instance.instance_iri)
        else:
            hplc_job_iri = response[0]['hplc_job']
            rxn_exp_iri = response[0]['rxn_exp']

        hplc_method_instance = self.get_hplc_method_given_hplc_report(hplc_report_instance.instance_iri)
        hplc_job_instance = HPLCJob(
            instance_iri=hplc_job_iri,
            hasReport=hplc_report_instance,
            characterises=self.getReactionExperiment(rxn_exp_iri)[0],
            usesMethod=hplc_method_instance
        )
        return hplc_job_instance

    def get_hplc_job(self, hplc_job_iri: str) -> HPLCJob:
        hplc_job_iri = trimIRI(hplc_job_iri)
        query = """SELECT ?hplc_report ?rxn_exp ?hplc_method WHERE {<%s> <%s> ?hplc_report; <%s> ?rxn_exp; <%s> ?hplc_method.}""" % (
            hplc_job_iri, ONTOHPLC_HASREPORT, ONTOHPLC_CHARACTERISES, ONTOHPLC_USESMETHOD)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple instances of HPLCReport/ReactionExperiment/HPLCMethod is associated with the given HPLCJob <%s>: %s" % (
                hplc_job_iri, str(response)))
        elif len(response) < 1:
            raise Exception("No records of HPLCJob/ReactionExperiment/HPLCMethod were found with the given HPLCJob instance <%s>" % hplc_job_iri)
        else:
            hplc_report_iri = response[0]['hplc_report']
            rxn_exp_iri = response[0]['rxn_exp']
            hplc_method_iri = response[0]['hplc_method']

        hplc_job_instance = HPLCJob(
            instance_iri=hplc_job_iri,
            hasReport=self.get_existing_hplc_report(hplc_report_iri),
            characterises=self.getReactionExperiment(rxn_exp_iri)[0],
            usesMethod=self.get_hplc_method(hplc_method_iri)
        )
        return hplc_job_instance

    def download_remote_raw_hplc_report(self, remote_file_path, downloaded_file_path):
        self.downloadFile(remote_file_path, downloaded_file_path)
        logger.info(f"Remote raw HPLC report {remote_file_path} was successfully downloaded to: {downloaded_file_path}")

    def connect_hplc_report_with_chemical_amount(self, hplc_report_iri: str, chemical_amount_iri: str):
        hplc_report_iri = trimIRI(hplc_report_iri)
        chemical_amount_iri = trimIRI(chemical_amount_iri)
        update = """INSERT DATA {<%s> <%s> <%s>.}""" % (hplc_report_iri, ONTOHPLC_GENERATEDFOR, chemical_amount_iri)
        self.performUpdate(update)
        logger.info(f"HPLCReport {hplc_report_iri} is connected to ChemicalAmount {chemical_amount_iri}")

    def get_remote_hplc_report_path_given_local_file(self, hplc_digital_twin: str, hplc_local_file: str) -> str:
        hplc_digital_twin = trimIRI(hplc_digital_twin)
        query = PREFIX_XSD+"""SELECT ?remote_path WHERE {<%s> <%s> ?hplc_report. ?hplc_report <%s> "%s"^^xsd:string; <%s> ?remote_path.}""" % (
            hplc_digital_twin, ONTOHPLC_HASPASTREPORT, ONTOHPLC_LOCALFILEPATH, hplc_local_file, ONTOHPLC_REMOTEFILEPATH)
        response = self.performQuery(query)
        if len(response) > 1:
            raise Exception("Multiple records of HPLCReport remote path identified for local file '%s' of HPLC <%s>: %s" % (
                hplc_local_file, hplc_digital_twin, str(response)))
        elif len(response) < 1:
            raise Exception("No record of HPLCReport remote path identified for local file '%s' of HPLC <%s>" % (hplc_local_file, hplc_digital_twin))
        else:
            return response[0]['remote_path']

    def collect_triples_for_performance_indicators(self, lst_performance_indicator: List[PerformanceIndicator], g: Graph) -> Graph:
        for pi in lst_performance_indicator:
            g = pi.create_instance_for_kg(g)
        return g

    def collect_triples_for_chromatogram_point(self, chrom_pts: List[ChromatogramPoint], hplc_report_iri: str, g: Graph) -> Graph:
        for pt in chrom_pts:
            g = pt.create_instance_for_kg(g)
            g.add((URIRef(hplc_report_iri), URIRef(ONTOHPLC_RECORDS), URIRef(pt.instance_iri)))
        return g

    def collect_triples_for_output_chemical_of_chem_amount(self, chemical_amount: ChemicalAmount, rxn_exp_iri: str, g: Graph):
        # NOTE we do NOT call create_instance_for_kg for chemical_amount here
        # NOTE as the triples about the chemical_amount itself (and vial) should already be in the KG
        # <chemical_amount> <refersToMaterial> <output_chemical>
        g.add((URIRef(chemical_amount.instance_iri), URIRef(ONTOCAPE_REFERSTOMATERIAL), URIRef(chemical_amount.refersToMaterial.instance_iri)))
        # NOTE it is important to finally add if this chemical_amount contains fully identified species or not
        # <chemical_amount> <containsUnidentifiedComponent> boolean
        g.add((URIRef(chemical_amount.instance_iri), URIRef(ONTOLAB_CONTAINSUNIDENTIFIEDCOMPONENT), Literal(chemical_amount.containsUnidentifiedComponent, datatype=XSD.boolean)))
        # Also add triples related to the OutputChemical
        g = chemical_amount.refersToMaterial.create_instance_for_kg(g)
        # <rxn_exp_iri> <hasOutputChemical> <output_chemical>
        g.add((URIRef(rxn_exp_iri), URIRef(ONTOREACTION_HASOUTPUTCHEMICAL), URIRef(chemical_amount.refersToMaterial.instance_iri)))
        return g

    def update_vapourtec_autosampler_liquid_level_millilitre(self, level_change_of_site: Dict[str, float], for_consumption: bool):
        if len(level_change_of_site) > 0:
            delete_clause = []
            insert_clause = []
            where_clause = []
            i = 0
            for site in level_change_of_site:
                i += 1
                site = trimIRI(site)
                delete_clause.append(f"""?measure_{str(i)} <{OM_HASNUMERICALVALUE}> ?liquid_level_{str(i)} .""")
                insert_clause.append(f"""?measure_{str(i)} <{OM_HASNUMERICALVALUE}> ?liquid_level_{str(i)}_updated .""")
                where_clause.append(f"""<{site}> <{ONTOVAPOURTEC_HOLDS}>/<{ONTOLAB_HASFILLLEVEL}>/<{OM_HASVALUE}> ?measure_{str(i)} .
                        ?measure_{str(i)} <{OM_HASNUMERICALVALUE}> ?liquid_level_{str(i)} .
                        BIND (?liquid_level_{str(i)} {'-' if for_consumption else '+'} {level_change_of_site[site]} AS ?liquid_level_{str(i)}_updated).""")

            update = """DELETE {""" + ' '.join(delete_clause) + """} INSERT {""" + ' '.join(insert_clause) + """} WHERE {""" + ' '.join(where_clause) + """}"""
            self.performUpdate(update)

    # TODO add unit test
    def update_waste_bottle_liquid_level_millilitre(self, level_change_of_bottle: Dict[str, float], for_consumption: bool):
        self.update_reagent_bottle_liquid_level_millilitre(level_change_of_bottle, for_consumption)

    # TODO add unit test
    def update_reagent_bottle_liquid_level_millilitre(self, level_change_of_bottle: Dict[str, float], for_consumption: bool):
        if len(level_change_of_bottle) > 0:
            delete_clause = []
            insert_clause = []
            where_clause = []
            i = 0
            for bottle in level_change_of_bottle:
                i += 1
                bottle = trimIRI(bottle)
                delete_clause.append(f"""?measure_{str(i)} <{OM_HASNUMERICALVALUE}> ?liquid_level_{str(i)} .""")
                insert_clause.append(f"""?measure_{str(i)} <{OM_HASNUMERICALVALUE}> ?liquid_level_{str(i)}_updated .""")
                where_clause.append(f"""<{bottle}> <{ONTOLAB_HASFILLLEVEL}>/<{OM_HASVALUE}> ?measure_{str(i)} . ?measure_{str(i)} <{OM_HASNUMERICALVALUE}> ?liquid_level_{str(i)} . BIND (?liquid_level_{str(i)} {'-' if for_consumption else '+'} {level_change_of_bottle[bottle]} AS ?liquid_level_{str(i)}_updated).""")

            update = """DELETE {""" + ' '.join(delete_clause) + """} INSERT {""" + ' '.join(insert_clause) + """} WHERE {""" + ' '.join(where_clause) + """}"""
            self.performUpdate(update)

    def create_chemical_amount_for_reaction_outlet(self, autosampler_site_iri: str, amount_of_chemical_amount: float):
        g = Graph()
        autosampler_site_iri = trimIRI(autosampler_site_iri)
        chemical_amount_iri = initialiseInstanceIRI(getNameSpace(autosampler_site_iri), ONTOLAB_CHEMICALAMOUNT)
        g.add((URIRef(chemical_amount_iri), RDF.type, URIRef(ONTOLAB_CHEMICALAMOUNT)))
        update = f"""{PREFIX_RDF} INSERT {{
            <{chemical_amount_iri}> rdf:type <{ONTOLAB_CHEMICALAMOUNT}>.
            ?vial <{ONTOLAB_ISFILLEDWITH}> <{chemical_amount_iri}>.
            }} WHERE {{<{autosampler_site_iri}> <{ONTOVAPOURTEC_HOLDS}> ?vial.}}"""
        # NOTE hewe we don't add triple <chemical_amount> <containsUnidentifiedComponent> boolean
        # NOTE this will be added by method collect_triples_for_output_chemical_of_chem_amount called by HPLCPostProAgent
        # NOTE as now we don't know if the chemical_amount contains fully identified species or not
        # NOTE which will only be known after the post processing has finished
        # TODO [future work] of course a better design can be made to avoid split things into different places
        self.performUpdate(update)
        self.update_vapourtec_autosampler_liquid_level_millilitre(
            {autosampler_site_iri:amount_of_chemical_amount}, for_consumption=False
        )
        return g

    # TODO add unit test
    # TODO [future work] this method adds the triple <waste_bottle> <ontolab:isFilledWith> <chemical_amount>.
    # TODO [future work] it needs a proper way to handle the waste bottle liquid level and its content
    def create_chemical_amount_for_outlet_to_waste(self, waste_bottle_iri: str, amount_of_chemical_amount: float):
        g = Graph()
        waste_bottle_iri = trimIRI(waste_bottle_iri)
        chemical_amount_iri = initialiseInstanceIRI(getNameSpace(waste_bottle_iri), ONTOLAB_CHEMICALAMOUNT)
        g.add((URIRef(chemical_amount_iri), RDF.type, URIRef(ONTOLAB_CHEMICALAMOUNT)))
        update = f"""{PREFIX_RDF} INSERT DATA {{
            <{chemical_amount_iri}> rdf:type <{ONTOLAB_CHEMICALAMOUNT}>.
            <{waste_bottle_iri}> <{ONTOLAB_ISFILLEDWITH}> <{chemical_amount_iri}>.}}"""
        self.performUpdate(update)
        self.update_waste_bottle_liquid_level_millilitre(
            {waste_bottle_iri:amount_of_chemical_amount}, for_consumption=False
        )
        return g

    def release_vapourtec_rs400_settings(self, vapourtec_rs400_iri: str):
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        update = """DELETE {?settings <%s> ?hardware.} WHERE {<%s> <%s>* ?hardware. ?settings <%s> ?hardware.}""" % (
                ONTOLAB_SPECIFIES, vapourtec_rs400_iri, SAREF_CONSISTSOF, ONTOLAB_SPECIFIES
            )
        self.performUpdate(update)

    def upload_vapourtec_input_file_to_kg(self, vapourtec_digital_twin, local_file_path: str, remote_file_subdir: str):
        # replace windows path separators with unix path separators
        _remote_file_subdir = remote_file_subdir.replace(':', '').replace('\\', '/') if remote_file_subdir is not None else None
        try:
            remote_file_path, timestamp_upload = self.uploadFile(local_file_path, _remote_file_subdir)
            logger.info(f"Vapourtec input file ({local_file_path}) was uploaded to fileserver {self.fs_url} at {timestamp_upload} with remote file path at: {remote_file_path}")
        except Exception as e:
            logger.error(e)
            raise Exception("Vapourtec input file (%s) upload failed with exception %s" % (local_file_path, str(e)))

        vapourtec_input_file_iri = initialiseInstanceIRI(getNameSpace(vapourtec_digital_twin), ONTOVAPOURTEC_VAPOURTECINPUTFILE)
        logger.info(f"The initialised VapourtecInputFile IRI is: {vapourtec_input_file_iri}")

        update = f"""{PREFIX_XSD} INSERT DATA {{
            <{vapourtec_input_file_iri}> <{ONTOVAPOURTEC_REMOTEFILEPATH}> "{remote_file_path}"^^xsd:anyURI;
            <{ONTOVAPOURTEC_LOCALFILEPATH}> "{local_file_path}"^^xsd:string; <{ONTOVAPOURTEC_LASTLOCALMODIFIEDAT}> {os.path.getmtime(local_file_path)};
            <{ONTOVAPOURTEC_LASTUPLOADEDAT}> {timestamp_upload}.}}""".replace("\\", "\\\\")
        try:
            self.performUpdate(update)
        except Exception as e:
            logger.error(f"SPARQL update to write information about the uploaded VapourtecInputFile (remote file path: {remote_file_path}) failed: {update}", exc_info=True)
            raise e

        return vapourtec_input_file_iri

    def get_vapourtec_input_file(self, vapourtec_input_file_iri) -> VapourtecInputFile:
        vapourtec_input_file_iri = trimIRI(vapourtec_input_file_iri)
        query = """SELECT ?lastLocalModifiedAt ?lastUploadedAt ?localFilePath ?remoteFilePath 
                WHERE {<%s> <%s> ?lastLocalModifiedAt; <%s> ?lastUploadedAt; <%s> ?localFilePath; <%s> ?remoteFilePath.}""" % (
                    vapourtec_input_file_iri, ONTOVAPOURTEC_LASTLOCALMODIFIEDAT, ONTOVAPOURTEC_LASTUPLOADEDAT, ONTOVAPOURTEC_LOCALFILEPATH, ONTOVAPOURTEC_REMOTEFILEPATH
                )

        response = self.performQuery(query)
        if (len(response) > 1):
            raise Exception("VapourtecInputFile <%s> should only have one set of information, retrieved: " % (vapourtec_input_file_iri, str(response)))

        vapourtec_input_file = VapourtecInputFile(instance_iri=vapourtec_input_file_iri, **response[0])
        return vapourtec_input_file

    def get_hplc_given_vapourtec_rs400(self, vapourtec_rs400_iri: str) -> HPLC:
        vapourtec_rs400_iri = trimIRI(vapourtec_rs400_iri)
        query = """SELECT ?hplc ?hplc_manufacturer ?lab ?hplc_power_supply ?is_managed_by ?report_extension
                WHERE {<%s> ^<%s>/<%s> ?hplc. ?hplc a <%s>; <%s> ?hplc_manufacturer; <%s> ?hplc_power_supply; <%s> ?report_extension.
                OPTIONAL{?hplc <%s> ?is_managed_by}}""" % (
            vapourtec_rs400_iri, SAREF_CONSISTSOF, SAREF_CONSISTSOF, ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY, DBPEDIA_MANUFACTURER, ONTOLAB_HASPOWERSUPPLY,
            ONTOHPLC_REPORTEXTENSION, ONTOLAB_ISMANAGEDBY
        )
        response = self.performQuery(query)
        if (len(response) > 1):
            # NOTE here we assume one HPLC module has only ONE manufacturer, locates in only ONE laboratory, also has only ONE type of power supply
            # NOTE this might not hold universally, but we will simplify for the moment
            raise NotImplementedError("Not yet supported - VapourtecRS400 <%s> is associated with multiple HPLC: %s" % (vapourtec_rs400_iri, str(response)))
        elif (len(response) < 1):
            raise Exception("No HPLC is identified connected with VapourtecRS400 <%s> when querying: %s" % (vapourtec_rs400_iri, query))

        res = response[0]
        hplc = HPLC(
            instance_iri=res['hplc'],
            manufacturer=res['hplc_manufacturer'],
            hasPowerSupply=res['hplc_power_supply'],
            isManagedBy=res['is_managed_by'] if 'is_managed_by' in res else None,
            reportExtension=res['report_extension'],
            # hasJob=, # TODO [future work] add support
            # hasPastReport=, # TODO [future work] add support
        )
        return hplc

    def detect_new_hplc_report_from_hplc_derivation(self, hplc_derivation_iri: str):
        hplc_derivation_iri = trimIRI(hplc_derivation_iri)
        query = """SELECT ?hplc_report WHERE {?hplc_job <%s> <%s>. ?hplc_job <%s> ?hplc_report.}""" % (
            ONTODERIVATION_BELONGSTO, hplc_derivation_iri, ONTOHPLC_HASREPORT
        )
        response = self.performQuery(query)
        if (len(response) > 1):
            # NOTE here we assume one HPLC Derivation has ONLY ONE HPLCJob and thus ONLY ONE HPLCReport
            # NOTE this might not hold universally, but we will simplify for the moment
            raise NotImplementedError("Not yet supported - HPLC Derivation <%s> is associated with multiple HPLCJob/HPLCReport: %s" % (hplc_derivation_iri, str(response)))
        elif (len(response) < 1):
            logger.info(f"No HPLCJob/HPLCReport identified yet for HPLC Derivation {hplc_derivation_iri}.")
            return None
        else:
            return response[0]['hplc_report']

    def get_all_laboratories(self) -> list:
        query = f"""SELECT DISTINCT ?lab WHERE {{?lab a <{ONTOLAB_LABORATORY}>.}}"""
        response = self.performQuery(query)
        return [list(res.values())[0] for res in response]

    def check_if_triple_exist(self, s, p, o) -> bool:
        lst = [s, p, o]
        for i in range(len(lst)):
            lst[i] = f"?_{str(i)}" if lst[i] is None else f"<{trimIRI(lst[i])}>"
        query = f"""ASK {{{lst[0]} {lst[1]} {lst[2]}.}}"""
        response = self.performQuery(query)
        return response[0]['ASK']
