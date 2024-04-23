from typing import List, Optional

from services.connectors.sg_buildings.model import PropertyUsage


class SGBuildingsSPARQLMaker:
    def count_buildings(self, usage: Optional[PropertyUsage], groupby_usage: bool):
        select_vars = ["(COUNT(?Building) AS ?BuildingCount)"]
        where_clauses = ["?Building rdf:type gml:Building ."]

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
PREFIX gml: <http://www.opengis.net/citygml/building/2.0/>
PREFIX ontobuiltenv: <https://www.theworldavatar.com/kg/ontobuiltenv/>
        
SELECT {select_vars} WHERE {{
    {where_clauses}
}}{groupby}""".format(
            select_vars=" ".join(select_vars),
            where_clauses="\n  ".join(where_clauses),
            groupby="\nGROUP BY ?PropertyUsage" if groupby_usage or usage else "",
        )
