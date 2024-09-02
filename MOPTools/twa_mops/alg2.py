# NOTE if we would like to allow CBUs functioning as 3-planar to function as 2-bent in new MOPs
# we can change "=" in `filter (?gbu_modularity >= ?metal_gbu_modularity)` and `filter (?_gbu_modularity >= ?organic_gbu_modularity)`
# to ">="
alg2 = """
prefix os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
prefix om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
prefix mops: <https://www.theworldavatar.com/kg/ontomops/>
prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
select distinct ?metal_formula ?organic_formula ?am_label ?am_symmetry ?metal_mw ?organic_mw ?metal_charge ?organic_charge ?metal_gbu_label ?organic_gbu_label ?metal_gbu_number ?organic_gbu_number ?am ?metal ?organic ?metal_gbu ?organic_gbu
where {
  {
    select distinct ?metal_formula ?metal_gbu_label ?am_label ?am_symmetry ?metal_gbu_number ?am ?metal_gbu ?metal ?metal_binding ?metal_ocn
    where {
      ?metal a mops:ChemicalBuildingUnit; mops:hasBindingSite ?metal_site; mops:hasBindingDirection/rdf:type ?metal_binding.
      ?metal_site rdf:type mops:MetalSite; mops:hasOuterCoordinationNumber ?metal_ocn.
      ?metal mops:isFunctioningAs ?gbu; mops:hasCBUFormula ?metal_formula.
      ?gbu ^mops:isFunctioningAs ?cbu_acting_as_the_same_gbu; mops:hasModularity ?gbu_modularity.
      ?cbu_acting_as_the_same_gbu mops:hasCBUFormula ?formula_of_cbu_acting_as_the_same_gbu.
      filter exists {
        ?metal ^mops:hasChemicalBuildingUnit ?_evi_m1; mops:hasCBUFormula ?temp_formula_metal.
        ?_evi_m1 mops:hasAssemblyModel ?shared_am; mops:hasMOPFormula ?_evi_m1_formula.
        ?cbu_acting_as_the_same_gbu ^mops:hasChemicalBuildingUnit ?_evi_m2; mops:hasCBUFormula ?temp_formula_of_cbu_acting_as_the_same_gbu.
        ?_evi_m2 mops:hasAssemblyModel ?shared_am; mops:hasMOPFormula ?_evi_m2_formula.
        ?shared_am mops:hasGenericBuildingUnit ?gbu; mops:hasGenericBuildingUnitNumber ?gbu_n.
        ?gbu_n mops:isNumberOf ?gbu; mops:hasUnitNumberValue ?gbu_number.
        filter (contains(str(?_evi_m1_formula), concat(str(?temp_formula_metal), str(?gbu_number))))
        filter (contains(str(?_evi_m2_formula), concat(str(?temp_formula_of_cbu_acting_as_the_same_gbu), str(?gbu_number))))
      }
      ?cbu_acting_as_the_same_gbu mops:isFunctioningAs ?metal_gbu.
      ?metal_gbu mops:hasModularity ?metal_gbu_modularity.
      ?cbu_acting_as_the_same_gbu ^mops:hasChemicalBuildingUnit ?existing_mop; mops:hasCBUFormula ?cbu_acting_as_the_same_gbu_formula.
      ?existing_mop mops:hasAssemblyModel ?am; mops:hasMOPFormula ?existing_mop_formula.
      ?am rdfs:label ?am_label; mops:hasSymmetryPointGroup ?am_symmetry; mops:hasGenericBuildingUnitNumber ?metal_gbu_n.
      ?metal_gbu ^mops:hasGenericBuildingUnit ?am; rdfs:label ?metal_gbu_label.
      ?metal_gbu_n mops:isNumberOf ?metal_gbu; mops:hasUnitNumberValue ?metal_gbu_number.
      filter (contains(str(?existing_mop_formula), concat(str(?cbu_acting_as_the_same_gbu_formula), str(?metal_gbu_number))))
      filter (?gbu_modularity = ?metal_gbu_modularity)
    } order by ?metal_formula
  }
  {
    select distinct ?organic_formula ?organic_gbu_label ?am_label ?organic_gbu_number ?am ?organic_gbu ?organic ?organic_binding ?organic_ocn
    where {
      ?organic a mops:ChemicalBuildingUnit; mops:hasBindingSite ?organic_site; mops:hasBindingDirection/rdf:type ?organic_binding.
      ?organic_site rdf:type mops:OrganicSite; mops:hasOuterCoordinationNumber ?organic_ocn.
      ?organic mops:isFunctioningAs ?_gbu; mops:hasCBUFormula ?organic_formula.
      ?_gbu ^mops:isFunctioningAs ?_cbu_acting_as_the_same_gbu; mops:hasModularity ?_gbu_modularity.
      ?_cbu_acting_as_the_same_gbu mops:hasCBUFormula ?_formula_of_cbu_acting_as_the_same_gbu.
      filter exists {
        ?organic ^mops:hasChemicalBuildingUnit ?_evi_m1; mops:hasCBUFormula ?_temp_formula_organic.
        ?_evi_m1 mops:hasAssemblyModel ?_shared_am; mops:hasMOPFormula ?_evi_m1_formula.
        ?_cbu_acting_as_the_same_gbu ^mops:hasChemicalBuildingUnit ?_evi_m2; mops:hasCBUFormula ?_temp_formula_of_cbu_acting_as_the_same_gbu.
        ?_evi_m2 mops:hasAssemblyModel ?_shared_am; mops:hasMOPFormula ?_evi_m2_formula.
        ?_shared_am mops:hasGenericBuildingUnit ?_gbu; mops:hasGenericBuildingUnitNumber ?_gbu_n.
        ?_gbu_n mops:isNumberOf ?_gbu; mops:hasUnitNumberValue ?_gbu_number.
        filter (contains(str(?_evi_m1_formula), concat(str(?_temp_formula_organic), str(?_gbu_number))))
        filter (contains(str(?_evi_m2_formula), concat(str(?_temp_formula_of_cbu_acting_as_the_same_gbu), str(?_gbu_number))))
      }
      ?_cbu_acting_as_the_same_gbu mops:isFunctioningAs ?organic_gbu.
      ?organic_gbu mops:hasModularity ?organic_gbu_modularity.
      ?_cbu_acting_as_the_same_gbu ^mops:hasChemicalBuildingUnit ?_existing_mop; mops:hasCBUFormula ?_cbu_acting_as_the_same_gbu_formula.
      ?_existing_mop mops:hasAssemblyModel ?am; mops:hasMOPFormula ?_existing_mop_formula.
      ?am mops:hasGenericBuildingUnitNumber ?organic_gbu_n.
      ?organic_gbu ^mops:hasGenericBuildingUnit ?am; rdfs:label ?organic_gbu_label.
      ?organic_gbu_n mops:isNumberOf ?organic_gbu; mops:hasUnitNumberValue ?organic_gbu_number.
      filter (contains(str(?_existing_mop_formula), concat(str(?_cbu_acting_as_the_same_gbu_formula), str(?organic_gbu_number))))
      filter (?_gbu_modularity = ?organic_gbu_modularity)
    }
  }
  ?metal os:hasMolecularWeight/om:hasValue/om:hasNumericalValue ?metal_mw; os:hasCharge/om:hasValue/om:hasNumericalValue ?metal_charge.
  ?organic os:hasMolecularWeight/om:hasValue/om:hasNumericalValue ?organic_mw; os:hasCharge/om:hasValue/om:hasNumericalValue ?organic_charge.
  filter (?organic_gbu != ?metal_gbu)
  filter (?organic_ocn = ?metal_ocn)
  filter (?organic_binding = ?metal_binding)
  filter not exists {
    ?_mop mops:hasAssemblyModel ?am; mops:hasChemicalBuildingUnit ?metal, ?organic.
  }
}
"""
