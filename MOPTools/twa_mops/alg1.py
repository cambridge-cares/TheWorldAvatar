alg1 = """
prefix mops: <https://www.theworldavatar.com/kg/ontomops/>
prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
select distinct ?metal_formula ?organic_formula ?am_label ?metal_gbu_label ?organic_gbu_label ?metal_gbu_number ?organic_gbu_number ?am ?metal ?organic
where {
  {
    select distinct ?metal_formula ?metal_gbu_label ?am_label ?metal_gbu_number ?am ?metal_gbu ?metal ?metal_binding ?metal_ocn
    where {
      ?metal a mops:ChemicalBuildingUnit; mops:hasBindingSite ?metal_site; mops:hasBindingDirection/rdf:type ?metal_binding.
      ?metal_site rdf:type mops:MetalSite; mops:hasOuterCoordinationNumber ?metal_ocn.
      ?metal mops:isFunctioningAs ?metal_gbu; mops:hasCBUFormula ?metal_formula.
      ?metal ^mops:hasChemicalBuildingUnit ?existing_mop.
      ?existing_mop mops:hasAssemblyModel ?am; mops:hasMOPFormula ?existing_mop_formula.
      ?am a mops:AssemblyModel; rdfs:label ?am_label; mops:hasSymmetryPointGroup ?am_symmetry; mops:hasGenericBuildingUnitNumber ?metal_gbu_n.
      ?metal_gbu ^mops:hasGenericBuildingUnit ?am; rdfs:label ?metal_gbu_label.
      ?metal_gbu_n mops:isNumberOf ?metal_gbu; mops:hasUnitNumberValue ?metal_gbu_number.
      filter (contains(str(?existing_mop_formula), concat(str(?metal_formula), str(?metal_gbu_number))))
    }
  }
  {
    select distinct ?organic_formula ?organic_gbu_label ?organic_gbu_number ?am ?organic_gbu ?organic ?organic_binding ?organic_ocn
    where {
      ?organic a mops:ChemicalBuildingUnit; mops:hasBindingSite ?organic_site; mops:hasBindingDirection/rdf:type ?organic_binding.
      ?organic_site rdf:type mops:OrganicSite; mops:hasOuterCoordinationNumber ?organic_ocn.
      ?organic mops:isFunctioningAs ?organic_gbu; mops:hasCBUFormula ?organic_formula.
      ?organic ^mops:hasChemicalBuildingUnit ?_existing_mop.
      ?_existing_mop mops:hasAssemblyModel ?am; mops:hasMOPFormula ?_existing_mop_formula.
      ?am a mops:AssemblyModel; rdfs:label ?am_label; mops:hasSymmetryPointGroup ?am_symmetry; mops:hasGenericBuildingUnitNumber ?organic_gbu_n.
      ?organic_gbu ^mops:hasGenericBuildingUnit ?am; rdfs:label ?organic_gbu_label.
      ?organic_gbu_n mops:isNumberOf ?organic_gbu; mops:hasUnitNumberValue ?organic_gbu_number.
      filter (contains(str(?_existing_mop_formula), concat(str(?organic_formula), str(?organic_gbu_number))))
    }
  }
  filter (?metal_gbu != ?organic_gbu)
  filter (?metal_ocn = ?organic_ocn)
  filter (?metal_binding = ?organic_binding)
  filter not exists {
    ?_mop mops:hasAssemblyModel ?am; mops:hasChemicalBuildingUnit ?metal, ?organic.
  }
}
order by ?organic_formula
"""
