from model.web.comp_op import ComparisonOperator


def parse_rhs_colon(val: str):
    operator, operand = val.split(":", maxsplit=1)
    return ComparisonOperator(operator), float(operand)
