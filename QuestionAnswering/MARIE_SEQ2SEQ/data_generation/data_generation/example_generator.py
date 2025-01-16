from collections import defaultdict
import random
from typing import Callable, Dict, List, Tuple

import numpy as np
from sklearn.model_selection import train_test_split

from data_generation.utils import add_space_and_lower


def ExampleGenerator(
    template_name: str,
    query_template: str,
    query_compact_template: str,
    qn_templates: List[str],
    arg_samplers: Dict[str, Callable[[int], List[str]]],
    val_sampler: Callable[[int], List[int]],
    minvalue_maxvalue_sampler: Callable[[int], List[Tuple[int, int]]],
):
    argnames = get_argnames(query_template)

    assert argnames == get_argnames(query_compact_template), (
        "Templates for query and compact query don't contain the same set of arguments.\n"
        f"query_template: {query_template}\n"
        f"query_compact_template: {query_compact_template}"
    )

    for qn_template in qn_templates:
        assert argnames == get_argnames(qn_template), (
            "Templates for query and question don't contain the same set of arguments.\n"
            f"query_template: {query_template}\n"
            f"qn_template: {qn_template}"
        )

    while True:
        argnames_by_type = defaultdict(list)
        for argname in argnames:
            argnames_by_type[_remove_numerical_suffix(argname)].append(argname)

        qn_kwargs = dict()
        sparql_kwargs = dict()
        for argtype, _argnames in argnames_by_type.items():
            if argtype == "maxvalue":
                continue
            if argtype == "minvalue":
                value_pairs = minvalue_maxvalue_sampler(len(_argnames))
                for argname, (minval, maxval) in zip(_argnames, value_pairs):
                    maxvalue_argname = "maxvalue" + argname[len("minvalue") :]
                    for kwargs in [qn_kwargs, sparql_kwargs]:
                        kwargs[argname] = minval
                        kwargs[maxvalue_argname] = maxval
            elif argtype == "value":
                values = val_sampler(len(_argnames))
                for argname, value in zip(_argnames, values):
                    for kwargs in [qn_kwargs, sparql_kwargs]:
                        kwargs[argname] = value
            else:
                values = arg_samplers[argtype](len(_argnames))
                for argname, value in zip(_argnames, values):
                    qn_kwargs[argname] = add_space_and_lower(value)
                    sparql_kwargs[argname] = value

        yield dict(
            template_name=template_name,
            question=random.choice(qn_templates).format(**qn_kwargs),
            sparql_query=query_template.format(**sparql_kwargs),
            sparql_query_compact=query_compact_template.format(**sparql_kwargs),
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

    def make_sampler(sampling_data: List[str]):
        def sampler(k: int = 1):
            return random.sample(sampling_data, k)

        return sampler

    def val_sampler(k: int = 1):
        return np.random.randint(20, 500, size=k)

    def minvalue_maxvalue_sampler(k: int = 1):
        minvals = np.random.randint(20, 500, size=k)
        maxvals = minvals + np.random.randint(20, 500, size=k)
        return list(zip(minvals, maxvals))

    property_sampler = make_sampler(properties)
    identifier_sampler = make_sampler(identifiers)
    train_arg_samplers = dict(
        PropertyName=property_sampler,
        IdentifierName=identifier_sampler,
        species=make_sampler(train_species),
        ChemClass=make_sampler(train_chemicalclasses),
        Use=make_sampler(train_uses),
    )
    test_arg_samplers = dict(
        PropertyName=property_sampler,
        IdentifierName=identifier_sampler,
        species=make_sampler(test_species),
        ChemClass=make_sampler(test_chemicalclasses),
        Use=make_sampler(test_uses),
    )

    return dict(
        train_arg_samplers=train_arg_samplers,
        test_arg_samplers=test_arg_samplers,
        val_sampler=val_sampler,
        minvalue_maxvalue_sampler=minvalue_maxvalue_sampler,
    )


def get_argnames(text: str):
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
