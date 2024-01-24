from collections import defaultdict
from typing import List


def tails_to_tail_nums(tails:  List[dict]):
    tail_nums = defaultdict(lambda: 0)
    for tail in tails:
        tail_nums[tail["type"] + "_num"] += 1
    return tail_nums