from postprocagent.data_model import *
import postprocagent.utils as utils
import pydantic

class HypoStreamSpecies(pydantic.BaseModel):
    species_iri: str
    def_role: str
    def_molar_mass: utils.DimensionalQuantity
    def_density: utils.DimensionalQuantity
    def_cost: utils.DimensionalQuantity
    def_eco_score: utils.DimensionalQuantity
    run_conc: utils.DimensionalQuantity
    run_mol: utils.DimensionalQuantity
    _run_volume: utils.DimensionalQuantity
    _run_mass: utils.DimensionalQuantity
    _is_reactant: bool = False
    _is_catalyst: bool = False
    _is_internal_standard: bool = False
    _is_solvent: bool = False

    class Config:
        underscore_attrs_are_private = True

    def __init__(__pydantic_self__, **data: Any) -> None:
        super().__init__(**data)

        __pydantic_self__.def_molar_mass = utils.unit_conversion_dq(__pydantic_self__.def_molar_mass, utils.UNIFIED_MOLAR_MASS_UNIT)
        __pydantic_self__.def_density = utils.unit_conversion_dq(__pydantic_self__.def_density, utils.UNIFIED_DENSITY_UNIT)
        __pydantic_self__.def_cost = utils.unit_conversion_dq(__pydantic_self__.def_cost, utils.UNIFIED_COST_UNIT)
        __pydantic_self__.def_eco_score = utils.unit_conversion_dq(__pydantic_self__.def_eco_score, utils.UNIFIED_ECOSCORE_UNIT)
        __pydantic_self__.run_conc = utils.unit_conversion_dq(__pydantic_self__.run_conc, utils.UNIFIED_CONCENTRATION_UNIT)
        __pydantic_self__.run_mol = utils.unit_conversion_dq(__pydantic_self__.run_mol, utils.UNIFIED_MOLE_UNIT)

        _mass_num_val = __pydantic_self__.def_molar_mass.hasNumericalValue * __pydantic_self__.run_mol.hasNumericalValue
        __pydantic_self__._run_mass = utils.DimensionalQuantity(hasUnit=utils.UNIFIED_MASS_UNIT,hasNumericalValue=_mass_num_val)

        _vol_num_val = __pydantic_self__._run_mass.hasNumericalValue / __pydantic_self__.def_density.hasNumericalValue
        __pydantic_self__._run_volume = utils.DimensionalQuantity(hasUnit=utils.UNIFIED_VOLUME_UNIT, hasNumericalValue=_vol_num_val)

        if __pydantic_self__.def_role == ONTOKIN_REACTANT:
            __pydantic_self__._is_reactant = True
        elif __pydantic_self__.def_role == ONTORXN_CATALYST:
            __pydantic_self__._is_catalyst = True
        elif __pydantic_self__.def_role == ONTOHPLC_INTERNALSTANDARD:
            __pydantic_self__._is_internal_standard = True
        elif __pydantic_self__.def_role == ONTORXN_SOLVENT:
            __pydantic_self__._is_solvent = True
        else:
            raise Exception("Role type <%s> NOT supported for Species <%s>." % (__pydantic_self__.def_role, __pydantic_self__.species_iri))

# class HypoSolute(HypoStreamSpecies):
#     is_reactant: bool
#     is_catalyst: bool
#     is_internal_standard: bool

#     def __init__(__pydantic_self__, **data: Any) -> None:
#         super().__init__(**data)
#         lst = [__pydantic_self__.is_reactant, __pydantic_self__.is_catalyst, __pydantic_self__.is_internal_standard]
#         if lst.count(True) > 1:
#             raise Exception("The HypoSolute is not initialised correctly: %s" % str(__pydantic_self__))
#         elif lst.count(True) < 1:
#             raise Exception("The HypoSolute is not initialised correctly: %s" % str(__pydantic_self__))

# class HypoSolvent(HypoStreamSpecies):
#     is_solvent: bool = True

class HypoPumpRunStream(pydantic.BaseModel):
    is_ref_pump: bool
    inlet_iri: str
    run_volume: utils.DimensionalQuantity
    eq_ratio: utils.DimensionalQuantity
    solute: List[HypoStreamSpecies]
    solvent: HypoStreamSpecies

    def __init__(__pydantic_self__, **data: Any) -> None:
        super().__init__(**data)

        __pydantic_self__.run_volume = utils.unit_conversion_dq(__pydantic_self__.run_volume, utils.UNIFIED_VOLUME_UNIT)
        __pydantic_self__.eq_ratio = utils.unit_conversion_dq(__pydantic_self__.eq_ratio, utils.UNIFIED_EQ_RATIO_UNIT)

class HypoReactor(pydantic.BaseModel):
    rxn_exp_iri: str
    residence_time: utils.DimensionalQuantity
    reactor_temperature: utils.DimensionalQuantity
    reactor_volume: utils.DimensionalQuantity
    inlet_run_stream: List[HypoPumpRunStream]
    total_run_volume: utils.DimensionalQuantity

    def __init__(__pydantic_self__, **data: Any) -> None:
        super().__init__(**data)

        __pydantic_self__.residence_time = utils.unit_conversion_dq(__pydantic_self__.residence_time, utils.UNIFIED_TIME_UNIT)
        __pydantic_self__.reactor_temperature = utils.unit_conversion_dq(__pydantic_self__.reactor_temperature, utils.UNIFIED_TEMPERATURE_UNIT)
        __pydantic_self__.reactor_volume = utils.unit_conversion_dq(__pydantic_self__.reactor_volume, utils.UNIFIED_VOLUME_UNIT)

        if abs(__pydantic_self__.total_run_volume.hasNumericalValue - sum([s.run_volume.hasNumericalValue for s in __pydantic_self__.inlet_run_stream])) > 0.01:
            raise Exception("HypoReactor is NOT initialised correctly for ReactionExperiment <%s>, the total_run_volume mismatches the sum of all its inlet_run_stream: %s" % (
                __pydantic_self__.rxn_exp_iri, __pydantic_self__))

class HypoEndStream(pydantic.BaseModel):
    # total_run_volume: 
    # internal_standard: HypoInternalStandard
    # product: HypoSolute
    # impurity: Optional[List[HypoSolute]]
    # un_reacted: Optional[List[HypoSolute]]
    pass
