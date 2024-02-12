import copy
import random
from typing import Dict, Iterable, List, Optional

from SPARQLWrapper import JSON, POST, SPARQLWrapper
import networkx as nx
from constants.functions import NumOp
from constants.namespaces import QUERY_PREFIXES
from constants.predicates import RDF_TYPE, RDFS_LITERAL

from utils import Utils
from utils.ontology import UtilsOntology


class QueryGraphGenerator:
    def __init__(
        self,
        ontology: nx.DiGraph,
        kg_endpoint: str,
        numcls2prop: Dict[str, str] = dict(),
        prop_blacklist: List[str] = [],
        questionnode_blacklist: List[str] = [],
    ):
        G = UtilsOntology.flatten_subclassof(ontology)
        G = UtilsOntology.remove_egdes_by_label(G, labels=prop_blacklist)
        self.ontology = G
        self.numcls2prop = numcls2prop
        self.questionnode_blacklist = questionnode_blacklist

        sparql_client = SPARQLWrapper(kg_endpoint)
        sparql_client.setReturnFormat(JSON)
        sparql_client.setMethod(POST)
        self.sparql_client = sparql_client

    def generate_query_template(
        self,
        edge_num: int,
        question_node: Optional[str] = None,
    ):
        if question_node is None:
            sampling_frame = [
                n for n in self.ontology.nodes() if n not in self.questionnode_blacklist
            ]
            question_node = random.choices(
                sampling_frame,
                weights=[self.ontology.degree[n] for n in sampling_frame],
            )[0]

        G = nx.MultiDiGraph()
        G.add_node(question_node, question_node=True)
        for _ in range(edge_num):
            n = random.choices(
                list(G.nodes()),
                weights=[int(self.ontology.degree[n] > 0) for n in G.nodes()],
            )[0]

            out_edges = list(self.ontology.out_edges(n, data="label"))
            in_edges = list(self.ontology.in_edges(n, data="label"))

            edge = random.choice(out_edges + in_edges)
            G.add_edge(edge[0], edge[1], label=edge[2])

        for n, question_node in list(G.nodes(data="question_node")):
            if question_node:
                continue
            if random.uniform(0, 1) < 0.8:
                if n not in self.numcls2prop:
                    G.nodes[n]["template_node"] = True
                elif RDFS_LITERAL not in G.neighbors(n):
                    comparative = random.choice([x.value for x in NumOp])

                    var = self.cls2var(n)
                    val_node = var + "Val"
                    func_node = var + comparative.value

                    G.add_nodes_from(
                        [
                            (
                                val_node,
                                dict(
                                    template_node=True,
                                    literal=True,
                                    label="rdfs:Literal",
                                ),
                            ),  # value node
                            (
                                func_node,
                                dict(func=comparative, label=comparative.value),
                            ),  # function node
                        ]
                    )
                    G.add_edges_from(
                        [
                            (n, val_node, dict(label=self.numcls2prop[n])),
                            (val_node, func_node, dict(label="func")),
                        ]
                    )

        return G

    def query_kg(self, query: str):
        self.sparql_client.setQuery(query)
        return self.sparql_client.queryAndConvert()

    def cls2var(self, cls: str):
        cls = Utils.shorten_iri(cls)
        return f"{cls.split(':', maxsplit=1)[-1]}"

    def make_sparql(
        self,
        triples: Iterable[str],
        result_vars: Optional[Iterable[str]] = None,
        limit: Optional[int] = None,
    ):
        triples = "\n  " + "\n  ".join(triples)
        select_vars = " ".join(result_vars) if result_vars is not None else "*"
        limit_clause = f"\nLIMIT {limit}" if limit is not None else ""
        return f"""{QUERY_PREFIXES}

SELECT DISTINCT {select_vars} WHERE {{{triples}
}}{limit_clause}"""

    def template2sparql(self, query_template: nx.MultiDiGraph):
        """Makes the SPARQL query that grounds all nodes in query_template"""
        triples = list()
        for h, t, prop in query_template.edges(data="label"):
            h, t, prop = (Utils.shorten_iri(x) for x in (h, t, prop))
            h_var, t_var = (self.cls2var(x) for x in (h, t))
            triples.append(f"?{h_var} {prop} ?{t_var} .")
            if h != "rdfs:Literal":
                triples.append(f"?{h_var} a {h} .")
            if t != "rdfs:Literal":
                triples.append(f"?{t_var} a {t} .")
        return self.make_sparql(triples, limit=1)

    def template2query(self, query_template: nx.MultiDiGraph):
        """Grounds query_template to obtain a query graph"""
        query = self.template2sparql(query_template)
        bindings = self.query_kg(query)["results"]["bindings"]

        if len(bindings) == 0:
            return None
        binding = bindings[0]

        query_graph = nx.DiGraph()
        for node, attr in query_template.nodes(data=True):
            var = self.cls2var(node)
            if attr.get("template_node"):
                query_graph.add_node(binding[var]["value"], **{**attr, **binding[var]})
            else:
                query_graph.add_node(var, **attr)
                query_graph.add_edge(var, node, label=RDF_TYPE)

        def get_graph_entity(cls: str):
            if query_template.nodes[cls].get("template_node"):
                return binding[self.cls2var(cls)]["value"]
            return self.cls2var(cls)

        for h, t, attr in query_template.edges(data=True):
            query_graph.add_edge(get_graph_entity(h), get_graph_entity(t), **attr)

        return query_graph

    def graph2sparql(self, query_graph: nx.DiGraph):
        def get_sparql_entity(n: str):
            if query_graph.nodes[n].get("template_node"):
                if n.startswith("http"):
                    return f"<{n}>"
                return (
                    f'"{n}"^^{Utils.shorten_iri(query_graph.nodes[n].get("datatype"))}'
                )
            if n.startswith("http"):  # is a class
                return Utils.shorten_iri(n)
            return "?" + n

        triples = list()
        for h, t, prop in query_graph.edges(data="label"):
            h, t = (get_sparql_entity(x) for x in (h, t))
            prop = Utils.shorten_iri(prop)
            if t != "rdfs:Literal":
                triples.append(f"{h} {prop} {t} .")

        result_var = "?" + next(
            n
            for n, question_node in query_graph.nodes(data="question_node")
            if question_node
        )

        return self.make_sparql(
            triples=triples,
            result_vars=[result_var],
        )

    def get_hashable_kg_response(self, graph: nx.DiGraph):
        return UtilsOntology.hash_kg_response(self.query_kg(self.graph2sparql(graph)))

    def minimize_query_graph(self, query_graph: Optional[nx.DiGraph]):
        if query_graph is None:
            return None

        response_init = self.get_hashable_kg_response(query_graph)
        for u, v in query_graph.edges():
            _query_graph = copy.deepcopy(query_graph)
            _query_graph.remove_edge(u, v)
            response = self.get_hashable_kg_response(_query_graph)
            if response == response_init:
                query_graph = _query_graph

        question_node = next(
            n
            for n, question_node in query_graph.nodes(data="question_node")
            if question_node
        )
        connected_nodes = list(
            nx.shortest_path(query_graph.to_undirected(), source=question_node).keys()
        )
        for n in list(query_graph.nodes()):
            if n not in connected_nodes:
                query_graph.remove_node(n)

        return query_graph

    def generate_query_graph(
        self, edge_num: int = 1, question_node: Optional[str] = None
    ):
        query_template = self.generate_query_template(
            edge_num=edge_num, question_node=question_node
        )
        query_graph = self.template2query(query_template)
        query_graph_min = self.minimize_query_graph(query_graph)
        return query_graph_min, query_graph, query_template
