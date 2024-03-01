import json

from model.constraint import (
    AtomicNumericalConstraint,
    CompoundNumericalConstraint,
    LogicalOperator,
)
from services.openai_client import get_openai_client


def parse_unstructured(text: str, schema: dict):
    response = get_openai_client().chat.completions.create(
        model="gpt-3.5-turbo-0125",
        messages=[{"role": "user", "content": "Find entities where " + text}],
        tools=[
            {
                "type": "function",
                "function": {
                    "name": "find",
                    "description": "Find entities given some constraints",
                    "parameters": schema,
                },
            }
        ],
        tool_choice="auto",
    )
    # TODO: handle potential parsing error by OpenAI
    return json.loads(response.choices[0].message.tool_calls[0].function.arguments)


def parse_constraint(text: str):
    args = parse_unstructured(
        text=text,
        schema={
            "type": "object",
            "properties": {
                "key": {
                    "type": "string",
                    "description": "Property name e.g. boiling point, heat of vaporization",
                },
                "logical_operator": {
                    "type": "string",
                    "enum": ["AND", "OR"],
                },
                "constraints": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "properties": {
                            "operator": {
                                "type": "string",
                                "enum": ["=", "<", "<=", ">", ">="],
                                "description": "The numerical operator of the constraint",
                            },
                            "operand": {
                                "type": "number",
                                "description": "The reference value of the constraint operator",
                            },
                        },
                    },
                },
            },
        },
    )
    key = args["key"]

    if "logical_operator" not in args:
        logical_operator = None
    if args.get("logical_operator") == "AND":
        logical_operator = LogicalOperator.AND
    elif args.get("logical_operator") == "OR":
        logical_operator = LogicalOperator.OR
    else:
        logical_operator = None

    constraints = [
        AtomicNumericalConstraint(
            operator=constraint["operator"], operand=constraint["operand"]
        )
        for constraint in args["constraints"]
    ]

    constraint = CompoundNumericalConstraint(
        logical_operator=logical_operator,
        constraints=constraints,
    )
    return key, constraint
