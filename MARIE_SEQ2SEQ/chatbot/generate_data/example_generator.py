import random
from typing import Any, Dict, List

from sklearn.model_selection import train_test_split

from generate_data.template_utils import add_space_and_lower


def ExampleGenerator(
    template_name: str,
    query_template: str,
    query_compact_template: str,
    qn_templates: List[str],
    arg_samplers: Dict[str, Any],
):
    argnames = _get_argnames(query_template)

    assert argnames == _get_argnames(query_compact_template), (
        "Templates for query and compact query don't contain the same set of arguments.\n"
        f"query_template: {query_template}\n"
        f"query_compact_template: {query_compact_template}"
    )

    for qn_template in qn_templates:
        assert argnames == _get_argnames(qn_template), (
            "Templates for query and question don't contain the same set of arguments.\n"
            f"query_template: {query_template}\n"
            f"qn_template: {qn_template}"
        )

    while True:
        kwargs = dict()
        for argname in argnames:
            if argname.startswith("maxval"):
                continue
            if argname.startswith("minval"):
                minval, maxval = arg_samplers["minval_maxval"]()
                kwargs[argname] = minval
                kwargs["maxval" + argname[len("minval") :]] = maxval
            else:
                kwargs[argname] = arg_samplers[_remove_numerical_suffix(argname)]()

        yield dict(
            template_name=template_name,
            question=random.choice(qn_templates).format(
                **{
                    k: add_space_and_lower(v)
                    if any(
                        k.startswith(prefix) for prefix in ["Property", "Identifier"]
                    )
                    else v
                    for k, v in kwargs.items()
                }
            ),
            sparql_query=query_template.format(**kwargs),
            sparql_query_compact=query_compact_template.format(**kwargs),
        )


def make_arg_samplers(
    properties: List[str],
    identifiers: List[str],
    species: List[str],
    chemicalclasses: List[str],
    uses: List[str],
):
    train_species, test_species = train_test_split(species, test_size=0.1)
    train_chemicalclasses, test_chemicalclasses = train_test_split(
        chemicalclasses, test_size=0.1
    )
    train_uses, test_uses = train_test_split(uses, test_size=0.1)

    def make_sampler(values: List[str]):
        def sampler():
            return random.choice(values)
        return sampler

    def val_sampler():
        return random.randint(20, 500)

    def minval_maxval_sampler():
        minval = random.randint(20, 500)
        maxval = minval + random.randint(20, 500)
        return minval, maxval

    property_sampler = make_sampler(properties)
    identifier_sampler = make_sampler(identifiers)
    train_arg_samplers = dict(
        PropertyName=property_sampler,
        IdentifierName=identifier_sampler,
        species=make_sampler(train_species),
        ChemClass=make_sampler(train_chemicalclasses),
        Use=make_sampler(train_uses),
        value=val_sampler,
        minval_maxval=minval_maxval_sampler,
    )
    test_arg_samplers = dict(
        PropertyName=property_sampler,
        IdentifierName=identifier_sampler,
        species=make_sampler(test_species),
        ChemClass=make_sampler(test_chemicalclasses),
        Use=make_sampler(test_uses),
        value=val_sampler,
        minval_maxval=minval_maxval_sampler,
    )

    return train_arg_samplers, test_arg_samplers


def _get_argnames(text: str):
    """Returns a set of argument names used in an f-string."""
    idx = 0
    arg_names: List[str] = []

    while idx < len(text):
        if text[idx] != "{":
            idx += 1
            continue

        if idx + 1 < len(text) and text[idx + 1] == "{":
            idx += 2
            continue
        else:
            open_bracket_idx = idx
            while idx < len(text) and text[idx] != "}":
                idx += 1
            if idx == len(text):
                raise ValueError("Text contains unclosed curly braces: " + text)
            arg_names.append(text[open_bracket_idx + 1 : idx])
    
    arg_names = list(set(arg_names))
    arg_names.sort()
    
    return arg_names


def _remove_numerical_suffix(text: str):
    idx = len(text)
    while idx >= 0 and text[idx - 1].isdigit():
        idx -= 1
    return text[:idx]
