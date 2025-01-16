import json
import os
import re
from pprint import pprint

from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR


class OntoKinReactionCleaner:
    """
    This class remove any reactions that are basically identical to each other
    """

    def __init__(self):
        self.dataset_dir = "CrossGraph/ontokin_reactions"
        self.ontology = "ontokin_reactions"
        self.full_dataset_dir = os.path.join(DATA_DIR, self.dataset_dir)
        self.reaction_list_path = os.path.join(self.full_dataset_dir, "unique_reactions.json")
        self.reaction_list = json.loads(open(self.reaction_list_path).read())
        self.reaction_dict_path = os.path.join(self.full_dataset_dir, "ontokin_reactions_value_dict.json")
        self.reaction_dict = json.loads(open(self.reaction_dict_path).read())
        self.unique_species = []

    def rearrange_reactions(self, reaction_string):
        # split into two parts
        reactants, products = reaction_string.split("[=]")
        reactants = sorted([r.strip() for r in reactants.split("+")])
        products = sorted([p.strip() for p in products.split("+")])

        for r in reactants:
            if r not in self.unique_species:
                self.unique_species.append(r)

        for p in products:
            if p not in self.unique_species:
                self.unique_species.append(p)

        if reactants == products:
            return None, None, None
        else:
            equation = " + ".join(reactants) + " [=] " + " + ".join(products) + " "
            return equation, reactants, products

    def create_clean_reaction_set_and_dictionary(self):
        reaction_dict_filtered = {}
        unique_reaction_list_filtered = []
        print(f" Original reaction number: {len(self.reaction_list)} ")
        counter = 0
        for reaction in self.reaction_list:
            counter += 1
            # print(f"{counter} out of {len(self.reaction_list)}")
            reaction_string = self.reaction_dict[reaction] + " "
            ignore_species = re.findall(r"([A-Z]+[0-9]+)+-[A-Z0-9]+ ", reaction_string)
            if len(ignore_species) > 0:
                ignore_part_list = re.findall(r"(-[A-Z]+[0-9]+ )|(-[0-9][A-Z]+ )", reaction_string)
                for ignore_part in ignore_part_list:
                    ignore_part = "".join([x for x in ignore_part])
                    reaction_string = reaction_string.replace(ignore_part, " ")

            rearranged_reaction, reactants, products = self.rearrange_reactions(reaction_string)
            if rearranged_reaction is not None:
                unique_reaction_list_filtered.append(rearranged_reaction)
                row = {"reactants": reactants, "products": products, "reaction_iri": reaction}
                reaction_dict_filtered[rearranged_reaction] = row

        unique_reaction_list_filtered = list(set(unique_reaction_list_filtered))
        self.unique_species = list(set(self.unique_species))
        return reaction_dict_filtered, unique_reaction_list_filtered

    def run(self):
        pass


if __name__ == "__main__":
    my_cleaner = OntoKinReactionCleaner()
    my_cleaner.create_clean_reaction_set_and_dictionary()

