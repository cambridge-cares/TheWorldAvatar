import re

from KGToolbox.Tools.NLPWarehouse import COMPARISON_OPERATOR_LABEL_LIST
# from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
# from Marie.EntityLinking.IRILookup import IRILookup
from Marie.Util.CommonTools import NumericalTools

def join_tuple_string(strings_tuple) -> str:
   return ' '.join(strings_tuple)

# joining all the tuples

def remove_formula(question):
    # print(question)
    # formula_regex = r"([a-zA-Z]*[0-9]+[a-zA-Z]*)"
    # formula_regex = r"[A-Z][a-z]?\d*|\((?:[^()]*(?:\(.*\))?[^()]*)+\)\d+"
    formula_regex = r"([A-Z][a-z]?\d*|\((?:[^()]*(?:\(.*\))?[^()]*)+\)\d+)|([a-z]+\d+)"

    # matches = re.findall(r'(?:\b%s\b\s?)+' % search_str, polymer_str)
    # longest_match = max(matches)
    # return longest_match.count(search_str)

    # formula = re.search(formula_regex, question).group()
    matches = re.findall(formula_regex, question)
    result = "".join([t.strip() for t in list(map(join_tuple_string, matches)) if t != ""])
    print("formula", result)
    question = question.replace(result, "")
    # print(question)
    # print("=========================")
    return question


def remove_mention(q_with_mention, mention):
    stop_words = ["what", "is", "are", "the", "more", "less",
                  "than", "species", "find", "all", "over",
                  "under", "of", "show", "me",
                  "chemical species", "having", "that", "can", "be"
                  ]
    flag_words = ["mops", "cbu", "assembly model"]
    if "mops" not in q_with_mention.lower():
        stop_words += ["with"]

    tokens = [t for t in q_with_mention.split(" ") if t.lower() not in stop_words]
    q_with_mention = " ".join(tokens).strip()

    for flag_word in flag_words:
        if flag_word in q_with_mention.lower():
            q_with_mention = q_with_mention.lower().replace(mention.lower(), "")
            return q_with_mention

    if "reaction" not in q_with_mention.lower():
        q_with_mention = q_with_mention.lower().replace(mention.lower(), "")
        tokens = [t for t in q_with_mention.split(" ") if t.lower() not in stop_words]
        q_with_mention = " ".join(tokens).strip()
        return q_with_mention
    else:
        return "reaction"



class CrossGraphFilter:

    def __init__(self):
        self.stop_words = ["whats", "what", "is", "are", "the", "more", "less",
                           "than", "species", "find", "all", "over",
                           "under", "of", "show", "me",
                           "chemical species", "having", "that", "can", "be", "?"

                           ]
        self.global_stop_words = ["g/mol", "dalton", "celsius", "show", "give", "find", "all", "the", "species"]
        self.global_stop_words += COMPARISON_OPERATOR_LABEL_LIST
        self.global_stop_words += self.stop_words
        self.global_stop_words = list(set(self.global_stop_words))

        # self.nel = ChemicalNEL()
        # self.ner_lite = IRILookup(nel=self.nel, enable_class_ner=False)

    def filter_before_cross_graph(self, question):
        question = question.replace("?", "")
        if "mop" in question.lower() or "assembly model" in question.lower():
            return "MOPs shape"
        elif "reaction" in question.lower():
            return "reaction"
        original_question = question.replace("'s", " of ")
        tokens = [t for t in original_question.strip().split(" ") if t.lower() not in self.global_stop_words]
        original_question = " ".join(tokens)
        original_question = remove_formula(original_question)
            # return ["ontomops"], torch.tensor([0, 0, 0, 0, 0, 0, 0, 1, 0])
        # mention = self.ner_lite.get_mention(question)
        # question = remove_mention(question, mention=mention)
        value, value_str = NumericalTools.numerical_value_extractor(question=original_question)
        if value is not None:
            original_question = original_question.replace(value_str, "")

        return original_question


if __name__ == "__main__":
    # my_filter = CrossGraphFilter()
    # filtered_question = my_filter.filter_before_cross_graph("find all species with molecular weight over 100")
    # print("filtered question:", filtered_question)
    question = "waht is the molecular weight of CH4"
    filtered_question = remove_formula(question=question)

    question = "waht is the molecular weight of C3H4O5"
    filtered_question = remove_formula(question=question)

    question = "waht is the molecular weight of H2O"
    filtered_question = remove_formula(question=question)

    question = "waht is the molecular weight of O2"
    filtered_question = remove_formula(question=question)

    question = "waht is the molecular weight of H2"
    filtered_question = remove_formula(question=question)

    question = "waht is the molecular weight of co2"
    filtered_question = remove_formula(question=question)

    question = "waht is the molecular weight of CO2"
    filtered_question = remove_formula(question=question)

    question = "waht is the molecular weight of h2"
    filtered_question = remove_formula(question=question)

    question = "find species with molecular weight more than 100 g/mol"
    filtered_question = remove_formula(question=question)

