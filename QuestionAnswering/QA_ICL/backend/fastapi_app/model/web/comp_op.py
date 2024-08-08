from enum import Enum


class ComparisonOperator(str, Enum):
    EQ = "eq"
    LT = "lt"
    GT = "gt"
    LTE = "lte"
    GTE = "gte"


COMP_OP_2_SPARQL_SYMBOL = {
    ComparisonOperator.EQ: "=",
    ComparisonOperator.LT: "<",
    ComparisonOperator.GT: ">",
    ComparisonOperator.LTE: "<=",
    ComparisonOperator.GTE: ">=",
}
