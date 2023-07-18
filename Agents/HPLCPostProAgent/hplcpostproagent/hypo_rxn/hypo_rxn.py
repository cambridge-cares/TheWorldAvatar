from hplcpostproagent.data_model import *
import chemistry_and_robots.data_model.unit_conversion as unit_conv
import pydantic

class HypoStreamSpecies(pydantic.BaseModel):
    species_iri: str
    def_role: str
    def_molar_mass: unit_conv.DimensionalQuantity
    def_density: unit_conv.DimensionalQuantity
    def_cost: unit_conv.DimensionalQuantity
    # def_eco_score: unit_conv.DimensionalQuantity # commented out as it's not used in the current version
    run_conc: unit_conv.DimensionalQuantity
    run_mol: unit_conv.DimensionalQuantity
    _run_volume: unit_conv.DimensionalQuantity
    _run_mass: unit_conv.DimensionalQuantity
    _is_reactant: bool = False
    _is_catalyst: bool = False
    _is_internal_standard: bool = False
    _is_solvent: bool = False
    _is_target_product: bool = False
    _is_impurity: bool = False
    _is_base: bool = False

    class Config:
        underscore_attrs_are_private = True

    def __init__(__pydantic_self__, **data: Any) -> None:
        super().__init__(**data)

        __pydantic_self__.def_molar_mass = unit_conv.unit_conversion_dq(__pydantic_self__.def_molar_mass, unit_conv.UNIFIED_MOLAR_MASS_UNIT)
        __pydantic_self__.def_density = unit_conv.unit_conversion_dq(__pydantic_self__.def_density, unit_conv.UNIFIED_DENSITY_UNIT)
        __pydantic_self__.def_cost = unit_conv.unit_conversion_dq(__pydantic_self__.def_cost, unit_conv.UNIFIED_COST_UNIT)
        # __pydantic_self__.def_eco_score = unit_conv.unit_conversion_dq(__pydantic_self__.def_eco_score, unit_conv.UNIFIED_ECOSCORE_UNIT) # commented out as it's not used in the current version
        __pydantic_self__.run_conc = unit_conv.unit_conversion_dq(__pydantic_self__.run_conc, unit_conv.UNIFIED_CONCENTRATION_UNIT)
        __pydantic_self__.run_mol = unit_conv.unit_conversion_dq(__pydantic_self__.run_mol, unit_conv.UNIFIED_MOLE_UNIT)

        _mass_num_val = __pydantic_self__.def_molar_mass.hasNumericalValue * __pydantic_self__.run_mol.hasNumericalValue
        __pydantic_self__._run_mass = unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_MASS_UNIT,hasNumericalValue=_mass_num_val)

        _vol_num_val = __pydantic_self__._run_mass.hasNumericalValue / __pydantic_self__.def_density.hasNumericalValue
        __pydantic_self__._run_volume = unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_VOLUME_UNIT, hasNumericalValue=_vol_num_val)

        if __pydantic_self__.def_role == ONTOKIN_REACTANT:
            __pydantic_self__._is_reactant = True
        elif __pydantic_self__.def_role == ONTOREACTION_CATALYST:
            __pydantic_self__._is_catalyst = True
        elif __pydantic_self__.def_role == ONTOHPLC_INTERNALSTANDARD:
            __pydantic_self__._is_internal_standard = True
        elif __pydantic_self__.def_role == ONTOREACTION_SOLVENT:
            __pydantic_self__._is_solvent = True
        elif __pydantic_self__.def_role == ONTOREACTION_TARGETPRODUCT:
            __pydantic_self__._is_target_product = True
        elif __pydantic_self__.def_role == ONTOREACTION_IMPURITY:
            __pydantic_self__._is_impurity = True
        elif __pydantic_self__.def_role == ONTOREACTION_BASE:
            __pydantic_self__._is_base = True
        else:
            raise Exception("Role type <%s> NOT supported for Species <%s>." % (__pydantic_self__.def_role, __pydantic_self__.species_iri))

class HypoPumpRunStream(pydantic.BaseModel):
    is_ref_pump: bool
    inlet_iri: str
    run_volume: unit_conv.DimensionalQuantity
    eq_ratio: unit_conv.DimensionalQuantity
    solute: List[HypoStreamSpecies]
    solvent: HypoStreamSpecies

    def __init__(__pydantic_self__, **data: Any) -> None:
        super().__init__(**data)

        __pydantic_self__.run_volume = unit_conv.unit_conversion_dq(__pydantic_self__.run_volume, unit_conv.UNIFIED_VOLUME_UNIT)
        __pydantic_self__.eq_ratio = unit_conv.unit_conversion_dq(__pydantic_self__.eq_ratio, unit_conv.UNIFIED_EQ_RATIO_UNIT)

class HypoReactor(pydantic.BaseModel):
    rxn_exp_iri: str
    residence_time: unit_conv.DimensionalQuantity
    reactor_temperature: unit_conv.DimensionalQuantity
    reactor_volume: unit_conv.DimensionalQuantity
    inlet_run_stream: List[HypoPumpRunStream]
    total_run_volume: unit_conv.DimensionalQuantity

    def __init__(__pydantic_self__, **data: Any) -> None:
        super().__init__(**data)

        __pydantic_self__.residence_time = unit_conv.unit_conversion_dq(__pydantic_self__.residence_time, unit_conv.UNIFIED_TIME_UNIT)
        __pydantic_self__.reactor_temperature = unit_conv.unit_conversion_dq(__pydantic_self__.reactor_temperature, unit_conv.UNIFIED_TEMPERATURE_UNIT)
        __pydantic_self__.reactor_volume = unit_conv.unit_conversion_dq(__pydantic_self__.reactor_volume, unit_conv.UNIFIED_VOLUME_UNIT)

        if abs(__pydantic_self__.total_run_volume.hasNumericalValue - sum([s.run_volume.hasNumericalValue for s in __pydantic_self__.inlet_run_stream])) > 0.01:
            raise Exception("HypoReactor is NOT initialised correctly for ReactionExperiment <%s>, the total_run_volume mismatches the sum of all its inlet_run_stream: %s" % (
                __pydantic_self__.rxn_exp_iri, __pydantic_self__))

class HypoEndStream(pydantic.BaseModel):
    total_run_volume: unit_conv.DimensionalQuantity
    component: List[HypoStreamSpecies]
    containsUnidentifiedComponent: bool
