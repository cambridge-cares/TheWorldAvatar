from decimal import Decimal
import random

from constants.functions import OBE_NUM_OPS, NumOp
from constants.namespaces import OBE
from constants.ontobuiltenv import OBE_PROPERTYUSAGE_LABELS, OBEAttrKey
from locate_then_ask.ontobuiltenv.locate.attr import OBEAttrLocator
from locate_then_ask.ontobuiltenv.model import OBEProperty
from locate_then_ask.query_graph import QueryGraph
from utils.numerical import make_operand_and_verbn


class OBEPropertyUsageLocator(OBEAttrLocator):
    def locate(self, query_graph: QueryGraph, entity: OBEProperty):
        assert any(entity.property_usage)

        verbns = []
        samples = random.sample(
            entity.property_usage,
            k=random.randint(1, min(len(entity.property_usage), 2)),
        )
        assert len(samples) == len(set(samples))
        samples.sort(
            key=lambda use: use.usage_share is None
        )  # those with usage_share comes first
        use_share_num = sum(use.usage_share is not None for use in samples)
        do_locate_share_num = random.randint(0, use_share_num)
        do_locate_shares = (
            i < do_locate_share_num for i in range(len(samples))
        )
        for use, do_locate_share in zip(samples, do_locate_shares):
            usage_node = query_graph.make_blank_node(key=OBEAttrKey.PROPERTY_USAGE)
            assert use.concept.startswith(OBE), use.concept
            clsname = use.concept[len(OBE) :]
            clsname_node = "obe:" + clsname

            query_graph.add_iri_node(clsname_node, prefixed=True)
            query_graph.add_triples(
                [
                    ("Property", "obe:hasPropertyUsage", usage_node),
                    (usage_node, "a/rdfs:subClassOf*", clsname_node),
                ]
            )

            verbn = random.choice(OBE_PROPERTYUSAGE_LABELS[clsname])

            if do_locate_share:
                if use.usage_share == Decimal("1."):
                    sampling_frame = [
                        NumOp.GREATER_THAN,
                        NumOp.GREATER_THAN_EQUAL,
                        NumOp.EQUAL,
                    ]
                else:
                    sampling_frame = OBE_NUM_OPS
                operator = random.choice(sampling_frame)
                share_pctg = use.usage_share * 100

                operand, verbn_numop = make_operand_and_verbn(
                    operator,
                    value=share_pctg,
                    min_val=share_pctg / 2,
                    max_val=(100 + share_pctg) / 2,
                )

                if operator == NumOp.EQUAL:
                    verbn_numop = str(operand[0])

                for num in operand:
                    share_pctg_str = str(num) + "%"
                    verbn_numop = verbn_numop.replace(str(num), share_pctg_str)
                operand = tuple(x / 100 for x in operand)

                usageshare_node = clsname + "UsageShare"
                query_graph.add_literal_node(usageshare_node)
                query_graph.add_triple(usage_node, "obe:hasUsageShare", usageshare_node)
                query_graph.add_func(
                    target_node=usageshare_node, operator=operator, operand=operand
                )

                verbn = verbn + " for " + verbn_numop

            verbns.append(verbn)

        verbalization = "usage is " + " and ".join(verbns)

        return verbalization
