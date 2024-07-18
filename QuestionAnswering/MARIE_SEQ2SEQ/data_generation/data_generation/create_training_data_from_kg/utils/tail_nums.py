import random
from typing import Optional


def get_tail_nums_total2(
    identifier_num: Optional[int] = None,
    chemicalclass_num: Optional[int] = None,
):
    if chemicalclass_num == 1:
        identifier_num = 0
        property_num, use_num = random.choice([(0, 1), (1, 0)])
    elif chemicalclass_num == None:
        property_num = random.randint(0, 2)
        if identifier_num is None:
            identifier_num = random.randint(property_num, 2) - property_num

        p_i_num = property_num + identifier_num
        if p_i_num == 0:
            use_num, chemicalclass_num = 1, 1
        elif p_i_num == 1:
            use_num, chemicalclass_num = random.choice([(0, 1), (1, 0)])
        else:
            use_num, chemicalclass_num = 0, 0
    else:
        raise ValueError(
            f"Unexpected value for argument `chemicalclass_num`: {chemicalclass_num}."
        )

    assert sum([property_num, identifier_num, use_num, chemicalclass_num]) == 2

    return dict(
        property_num=property_num,
        identifier_num=identifier_num,
        use_num=use_num,
        chemicalclass_num=chemicalclass_num,
    )


def get_tail_nums_total3(
    identifier_num: Optional[int] = None,
    chemicalclass_num: Optional[int] = None,
):
    if chemicalclass_num == 1:
        identifier_num = 0
        property_num, use_num = random.choice([(1, 1), (2, 0)])
    elif chemicalclass_num == 2:
        identifier_num = 0
        property_num, use_num = random.choice([(0, 1), (1, 0)])
    elif chemicalclass_num == None:
        sampling_space_p_i = [
            (0, 1),
            (0, 2),
            (0, 3),
            (1, 0),
            (1, 1),
            (1, 2),
            (2, 0),
            (2, 1),
            (3, 0),
        ]
        if identifier_num is not None:
            sampling_space_p_i = [
                (p, i) for p, i in sampling_space_p_i if i == identifier_num
            ]

        property_num, identifier_num = random.choice(sampling_space_p_i)
        p_i_num = property_num + identifier_num
        if p_i_num == 1:
            use_num, chemicalclass_num = 1, 1
        elif p_i_num == 2:
            use_num, chemicalclass_num = random.choice([(0, 1), (1, 0)])
        elif p_i_num == 3:
            use_num, chemicalclass_num = 0, 0
        else:
            raise Exception(
                f"Unexpected `property_num` value of {property_num} and `identifier_num` value of {identifier_num}."
            )
    else:
        raise ValueError(
            f"Unexpected value for argument `chemicalclass_num`: {chemicalclass_num}."
        )

    assert sum([property_num, identifier_num, use_num, chemicalclass_num]) == 3

    return dict(
        property_num=property_num,
        identifier_num=identifier_num,
        use_num=use_num,
        chemicalclass_num=chemicalclass_num,
    )
