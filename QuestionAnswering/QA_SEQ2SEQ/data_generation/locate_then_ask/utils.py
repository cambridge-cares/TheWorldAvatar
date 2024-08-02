import random
import math


def get_lt(value: float):
    if value == 0:
        lt = -1.0
    elif value > 0:
        lt = value * 0.9
    else:
        lt = value * 1.1

    if random.getrandbits(1):
        lt_int = math.floor(lt)
        if lt_int < value:
            lt = lt_int

    return lt


def get_gt(value: float):
    if value == 0:
        gt = 1.0
    elif value > 0:
        gt = value * 1.1
    else:
        gt = value * 0.9

    if random.getrandbits(1):
        gt_int = math.ceil(gt)
        if gt_int > value:
            gt = gt_int

    return gt
