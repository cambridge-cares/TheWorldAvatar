from typing import Union

from pubchemagent.unit_parse.config import Quantity, config
from pubchemagent.unit_parse.pre_processing_substitution import remove_strings, substitution
from pubchemagent.unit_parse.pre_processing_multiple import multiple_quantities_main
from pubchemagent.unit_parse.core import text_list_to_quantity
from pubchemagent.unit_parse.reduce_quantities import reduce_quantities
from pubchemagent.unit_parse.utils import remove_empty_cells
from pubchemagent.unit_parse.logger import logger


def parser(text_in: str) -> Union[Quantity, list[Quantity], list[list[Quantity]]]:
    """ parser

    Main function to call to do parsing.

    Parameters
    ----------
    text_in: str
        text you want to be parsed

    Returns
    -------
    output: Quantity, list[Quantity], list[list[Quantity]]

    """
    logger.info(f"INPUT: {text_in}")
    # type check
    if not isinstance(text_in, str):
        raise TypeError(f"'text_in' must be a string. Given {text_in} (type: {type(text_in)}")

    # pre-processing
    text_in = remove_strings(text_in, config.remove_text)
    text_in = substitution(text_in)
    text_list = multiple_quantities_main(text_in)

    # text to unit
    out = text_list_to_quantity(text_list)

    # post-processing
    out = remove_empty_cells(out)
    out = reduce_quantities(out)

    # return unit instead of list if just one
    # if isinstance(out, list) and len(out) == 1:
    #     if not isinstance(out[0], list):
    #         out = out[0]
    #     elif isinstance(out[0], list) and len(out[0]) == 1:
    #         out = out[0][0]

    logger.info(f"OUTPUT: {out}'")
    return out
