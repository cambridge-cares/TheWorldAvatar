from decimal import Decimal
import random
import math
from typing import Optional, Tuple

import numpy as np
from numpy.typing import ArrayLike

from constants.functions import NumOp


class NumGetter:
    @classmethod
    def lt(
        cls,
        value: Decimal,
        to_int: bool = False,
        min_val: Optional[Decimal] = None,
    ):
        """Returns a number less than input and of the same sign."""
        if min_val is not None and value < min_val:
            raise ValueError(
                "Unable to generate a number less than {val} but no less than {min}".format(
                    val=value, min=min_val
                )
            )

        if value == 0:
            lt = Decimal("-1.")
        elif value > 0:
            lt = value * Decimal("0.9")
        else:
            lt = value * Decimal("1.1")

        if to_int or random.getrandbits(1):
            lt = Decimal(str(math.floor(lt)))
            if lt == value:
                lt = value - 1

        if min_val is not None:
            lt = max(lt, min_val)

        assert lt < value, value
        assert lt * value >= 0, value
        return lt

    @classmethod
    def gt(
        cls,
        value: Decimal,
        to_int: bool = False,
        max_val: Optional[Decimal] = None,
    ):
        """Returns a number greater than input of the same sign."""
        if max_val is not None and value > max_val:
            raise ValueError(
                "Unable to generate a number greater than {value} but no greater than {max}".format(
                    value=value, max=max_val
                )
            )

        if value == 0:
            gt = Decimal("1.")
        elif value > 0:
            gt = value * Decimal("1.1")
        else:
            gt = value * Decimal("0.9")

        if to_int or random.getrandbits(1):
            gt = Decimal(str(math.ceil(gt)))
            if gt == value:
                gt = value + 1

        if max_val is not None:
            gt = min(gt, max_val)

        assert gt > value, "gt: {gt}; value: {value}".format(gt=gt, value=value)
        assert gt * value >= 0, value
        return gt


def make_operand_and_verbn(
    operator: NumOp,
    value: Decimal,
    to_int: bool = False,
    max_val: Optional[Decimal] = None,
    min_val: Optional[Decimal] = None,
) -> Tuple[Tuple[Decimal, ...], str]:
    if operator is NumOp.LESS_THAN:
        operand = (NumGetter.gt(value, to_int=to_int, max_val=max_val),)
        verbn = random.choice(["<", "less than", "lower than", "smaller than"])
    elif operator is NumOp.LESS_THAN_EQUAL:
        if random.getrandbits(1):
            operand = (NumGetter.gt(value, to_int=to_int, max_val=max_val),)
        else:
            operand = (value,)
        verbn = random.choice(["<=", "less than or equal to", "not greater than"])
    elif operator is NumOp.GREATER_THAN:
        operand = (NumGetter.lt(value, to_int=to_int, min_val=min_val),)
        verbn = random.choice([">", "greater than", "higher than", "bigger than"])
    elif operator is NumOp.GREATER_THAN_EQUAL:
        if random.getrandbits(1):
            operand = (NumGetter.lt(value, to_int=to_int, min_val=min_val),)
        else:
            operand = (value,)
        verbn = random.choice([">=", "greater than or equal to", "not less than"])
    elif operator is NumOp.EQUAL:
        operand = (value,)
        verbn = random.choice(["=", "equal to"])
    elif operator is NumOp.INSIDE_RANGE:
        low = NumGetter.lt(value, to_int=to_int, min_val=min_val)
        high = NumGetter.gt(value, to_int=to_int, max_val=max_val)
        operand = (low, high)
        verbn = random.choice(
            ["in the range between", "between", "inside the interval"]
        )
    elif operator is NumOp.OUTSIDE_RANGE:
        low = NumGetter.lt(value, to_int=to_int, min_val=min_val)
        high = NumGetter.gt(value, to_int=to_int, max_val=max_val)
        operand = (low, high)
        verbn = random.choice(["outside the interval", "not between"])
    elif operator is NumOp.AROUND:
        operand = (value,)
        verbn = random.choice(["around", "approximately"])
    else:
        raise ValueError("Unexpected operator: " + str(operator))

    if len(operand) > 1:
        operand_str = "({values})".format(values=", ".join([str(x) for x in operand]))
    else:
        operand_str = str(operand[0])
    verbn += " " + operand_str

    return operand, verbn


def normalize_1d(arr: ArrayLike):
    return np.array(arr) / sum(arr)
