import inspect
import os
import sys

from pubchemagent.unit_parse.logger import logger


def get_unit_registry():
    """
    Gets object from Python stack/globals
    Stops at first object it finds

    """
    stack = inspect.stack()
    for frame in stack:
        attrs: dict = frame.frame.f_locals
        for attr in attrs.values():
            if hasattr(attr, "_REGISTRY"):
                return attr._REGISTRY
    else:
        mes = "Pint UnitRegistry not found."
        raise Exception(mes)


def check_for_pint():
    """ Check for Pint

    Pint's requires a Unit Registry to be defined. However, Unit Registries are not interoperable and will throw
    errors if a unit from one registry is used in another. So we go looking to see if one has been created,
    and if it hasn't we will make one!

    Returns
    -------
    UnitRegistry

    """
    modules = sys.modules
    if "pint" in modules:
        logger.warning("'Pint' module found in stack. (you have 'import pint' somewhere in your code).")
        # get unit registry
        try:
            u_ = get_unit_registry()
            logger.warning("\033[32m Unit registry found. :) \033[0m")
            return u_
        except Exception:
            logger.warning("Pint unit registry not found in stack. Loading 'unit_parser' registry. (Note: "
                           "Pint unit registries are not interoperable. ")

    # if no pint found, load local
    import pint
    current_path = os.path.dirname(os.path.realpath(__file__))
    u_ = pint.UnitRegistry(autoconvert_offset_to_baseunit=True,
                           filename=os.path.join(current_path, "support_files", "default_en.txt"))
    u_.default_format = "~"
    return u_


# set pint units
u = check_for_pint()
U = Unit = u.Unit
Q = Quantity = u.Quantity

# load english dictionary
file_path = os.path.dirname(os.path.realpath(__file__))
path_to_dict = os.path.join(file_path, "support_files", "dictionary.txt")
with open(path_to_dict, 'r') as file:
    english_dict = set(file.read().split("\n"))


class Config:
    """


    Attributes
    ----------
    pre_proc_sub : list[str]
        Pre-processing patterns
    pre_proc_split

    last_minute_sub

    english_dict: set
        A reduced set of english words that are removed from the parsing text.

    """

    def __init__(self):
        self.remove_text = []

        self.pre_proc_sub = [
            # [pattern, substitution value]
            ["^[a-zA-Z;,.: /]*", ""],  # remove text at front of strings
            ["(?<=[^a-zA-Z])at([^a-zA-Z])", " @ "],  # replace at with @
            ["−", "-"],  # unify dash (long, short) symbols
            ["·", "*"],  # unify multiplication symbols
            ["° F", " °F"],  # pint gets confused (degree farad)
            ["° C", " °C"],  # pint gets confused
            ["°F", "degF"],  # eliminates issue with capitalization step
            ["°C", "degC"],  # eliminates issue with capitalization step
            ["(?<=[0-9]{1})[ ]{0,1}X[ ]{0,1}(?=[0-9]{1})", "*"],  # unify multiplication symbols
            ["(?<=[0-9]{1})[ ]{0,1}x[ ]{0,1}(?=[0-9]{1})", "*"],  # unify multiplication symbols
            ["\[", "("],  # noqa: W605 # make all brackets parenthesis
            ["\]", ")"],  # noqa: W605 # make all brackets parenthesis
            ["^.*={1}", ""],  # delete everything in front of equal
            ["^.*:{1}", ""],  # delete everything in front of collen
            ["( to )", "-"],  # unify how range are represented
            ["(?<=[a-zA-Z])-(?=[a-zA-Z])", " "],  # turn dashes between text into spaces so dictionary can remove
            ["mm Hg", "mmHg"],  # pint gets confused
            ["KG", "kg"],  # pint gets confused
            ["LB", "lb"],  # pint gets confused
            ["kpa", "kPa"],  # pint gets confused
            ["cu ft", "ft**3"],  # pint gets confused
            ["cu in", "in**3"],  # pint gets confused
            ["cu m", "m**3"],  # pint gets confused
            ["cu cm", "cm**3"],  # pint gets confused
            ["cubic centimeter", "cm**3"],
            ["cu mm", "mm**3"],  # pint gets confused
            ["[0-9]{1,5} ?%", ""],
            ["X10-", "e-"]
        ]

        self.pre_proc_split = [";", ","]

        self.last_minute_sub = [
            # [pattern, substitution value]
            ["-{1}[^0-9]*$", ""],  # remove trailing dash
            ["(?<=[a-zA-Z0-9]) {1,2}[0-9()]{2,5}", ""]  # remove trailing number  ex. 90 g/mol 1999 ->  90 g/mol
        ]

        self.english_dict = english_dict


config = Config()
