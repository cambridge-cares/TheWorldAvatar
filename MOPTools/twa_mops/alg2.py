# NOTE if we would like to allow CBUs functioning as 3-planar to function as 2-bent in new MOPs
# we can change "=" in `filter (?gbu_modularity >= ?metal_gbu_modularity)` and `filter (?_gbu_modularity >= ?organic_gbu_modularity)`
# to ">="
alg2 = """
prefix os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
prefix om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
prefix mops: <https://www.theworldavatar.com/kg/ontomops/>
prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
select distinct ?mop_formula ?mop_mw ?mop_charge ?am ?metal ?organic ?metal_gbu ?organic_gbu ?metal_formula ?organic_formula ?am_label ?am_symmetry ?metal_gbu_label ?organic_gbu_label ?metal_gbu_number ?organic_gbu_number
where {
  {
    select distinct ?metal_formula ?metal_mw ?metal_charge ?metal_gbu_label ?am_label ?am_symmetry ?metal_gbu_number ?am ?metal_gbu ?metal ?metal_binding ?metal_ocn
    where {
      ?metal a mops:ChemicalBuildingUnit; mops:hasBindingSite ?metal_site; mops:hasBindingDirection/rdf:type ?metal_binding.
      ?metal_site rdf:type mops:MetalSite; mops:hasOuterCoordinationNumber ?metal_ocn.
      ?metal mops:isFunctioningAs ?_metal_gbu; mops:hasCBUFormula ?metal_formula.
      filter exists {?metal ^mops:hasChemicalBuildingUnit/mops:hasAssemblyModel/mops:hasGenericBuildingUnit ?_metal_gbu.}
      ?_metal_gbu mops:hasGBUType/mops:hasModularity ?_metal_gbu_modularity.
      ?_metal_gbu ^mops:isFunctioningAs/mops:isFunctioningAs ?metal_gbu.
      ?metal_gbu mops:hasGBUType/mops:hasModularity ?metal_gbu_modularity.
      ?metal_gbu ^mops:hasGenericBuildingUnit ?am; mops:hasGBUType/rdfs:label ?metal_gbu_label.
      ?am a mops:AssemblyModel; rdfs:label ?am_label; mops:hasSymmetryPointGroup ?am_symmetry.
      ?metal_gbu_n mops:isNumberOf ?metal_gbu; mops:hasUnitNumberValue ?metal_gbu_number.
      ?metal os:hasMolecularWeight/om:hasValue/om:hasNumericalValue ?metal_mw; os:hasCharge/om:hasValue/om:hasNumericalValue ?metal_charge.
      filter (?_metal_gbu_modularity = ?metal_gbu_modularity)
    }
  }
  {
    select distinct ?organic_formula ?organic_mw ?organic_charge ?organic_gbu_label ?organic_gbu_number ?am ?organic_gbu ?organic ?organic_binding ?organic_ocn #?equiv_cbu
    where {
      ?organic a mops:ChemicalBuildingUnit; mops:hasBindingSite ?organic_site; mops:hasBindingDirection/rdf:type ?organic_binding.
      ?organic_site rdf:type mops:OrganicSite; mops:hasOuterCoordinationNumber ?organic_ocn.
      ?organic mops:isFunctioningAs ?_organic_gbu; mops:hasCBUFormula ?organic_formula.
      filter exists {?organic ^mops:hasChemicalBuildingUnit/mops:hasAssemblyModel/mops:hasGenericBuildingUnit ?_organic_gbu.}
      ?_organic_gbu mops:hasGBUType/mops:hasModularity ?_organic_gbu_modularity.
      ?_organic_gbu ^mops:isFunctioningAs ?equiv_cbu. ?equiv_cbu mops:isFunctioningAs ?organic_gbu.
      ?organic_gbu mops:hasGBUType/mops:hasModularity ?organic_gbu_modularity.
      ?organic_gbu ^mops:hasGenericBuildingUnit ?am; mops:hasGBUType/rdfs:label ?organic_gbu_label.
      ?am a mops:AssemblyModel; rdfs:label ?am_label; mops:hasSymmetryPointGroup ?am_symmetry.
      ?organic_gbu_n mops:isNumberOf ?organic_gbu; mops:hasUnitNumberValue ?organic_gbu_number.
      ?organic os:hasMolecularWeight/om:hasValue/om:hasNumericalValue ?organic_mw; os:hasCharge/om:hasValue/om:hasNumericalValue ?organic_charge.
      filter (?_organic_gbu_modularity = ?organic_gbu_modularity)
    }
  }
  filter (?metal_gbu != ?organic_gbu)
  filter (?metal_ocn = ?organic_ocn)
  filter (?metal_binding = ?organic_binding)
  bind (?organic_mw*?organic_gbu_number+?metal_mw*?metal_gbu_number as ?mop_mw)
  bind (?organic_charge*?organic_gbu_number+?metal_charge*?metal_gbu_number as ?mop_charge)
  bind (concat(?metal_formula, str(?metal_gbu_number), ?organic_formula, str(?organic_gbu_number)) as ?mop_formula)
  filter not exists {
    ?_mop mops:hasAssemblyModel ?am; mops:hasChemicalBuildingUnit ?metal, ?organic.
  }
}
order by ?am_label
"""
