from typing import List, Optional

from services.connectors.sg_buildings.model import BuildingAttrKey, PropertyUsage


class SGBuildingsSPARQLMaker:
    def count_buildings(self, usage: Optional[PropertyUsage], groupby_usage: bool):
        select_vars = ["(COUNT(?Building) AS ?BuildingCount)"]
        where_clauses = ["?Building rdf:type bldg:Building ."]

        if usage or groupby_usage:
            select_vars.append("?PropertyUsage")
            if usage:
                where_clauses.append(
                    "VALUES ?PropertyUsage {{ {values} }}".format(
                        values="ontobuiltenv:" + usage.value
                    )
                )
            where_clauses.append(
                "?Building ontobuiltenv:hasPropertyUsage/rdf:type ?PropertyUsage ."
            )

        return """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX bldg: <http://www.opengis.net/citygml/building/2.0/>
PREFIX ontobuiltenv: <https://www.theworldavatar.com/kg/ontobuiltenv/>
        
SELECT {select_vars} WHERE {{
    {where_clauses}
}}{groupby}""".format(
            select_vars=" ".join(select_vars),
            where_clauses="\n  ".join(where_clauses),
            groupby="\nGROUP BY ?PropertyUsage" if groupby_usage or usage else "",
        )

    def lookup_building_attribute(
        self, facility_iris: List[str], attr_key: BuildingAttrKey
    ):
        if attr_key is BuildingAttrKey.HEIGHT:
            attr_var = "?Height"
            attr_triple = "?Building bldg:measuredHeight ?Height ."
        else:
            attr_var = "?FootPrintWKT"
            attr_triple = (
                """?Building bldg:lod0FootPrint/^grp:parent/geo:asWKT ?FootPrintWKT ."""
            )

        return """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX bldg: <http://www.opengis.net/citygml/building/2.0/>
PREFIX grp: <http://www.opengis.net/citygml/cityobjectgroup/2.0/>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX ontobim: <https://www.theworldavatar.com/kg/ontobim/>

SELECT DISTINCT ?Building ?FacilityName {attr_var} WHERE {{
    ?Building rdf:type bldg:Building .
    FILTER ( ?Facility IN ({facility_iris}) )
    ?Facility rdfs:label ?FacilityName .
    ?Building ontobim:hasFacility ?Facility .
    {attr_triple}
}}""".format(
            attr_var=attr_var,
            facility_iris=", ".join("<{iri}>".format(iri=iri) for iri in facility_iris),
            attr_triple=attr_triple,
        )
