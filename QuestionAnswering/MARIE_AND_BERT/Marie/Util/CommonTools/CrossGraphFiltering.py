import re

from Marie.Util.NLP.NLPWarehouse import COMPARISON_OPERATOR_LABEL_LIST
# from Marie.EntityLinking.ChemicalNEL import ChemicalNEL
# from Marie.EntityLinking.IRILookup import IRILookup
from Marie.Util.CommonTools import NumericalTools

def join_tuple_string(strings_tuple) -> str:
   return ' '.join(strings_tuple)

# joining all the tuples

def remove_formula(question):
    question = question.replace("?", "").strip()
    question = f" {question} "
    # formula_regex = r"([a-zA-Z]*[0-9]+[a-zA-Z]*)"
    # formula_regex = r"[A-Z][a-z]?\d*|\((?:[^()]*(?:\(.*\))?[^()]*)+\)\d+"
    # formula_regex = r"([A-Z][a-z]?\d*|\((?:[^()]*(?:\(.*\))?[^()]*)+\)\d+)|([a-z]+\d+)"
    formula_regex = r"[A-Za-z]+[0-9]+[A-Za-z0-9]*|[ ][COHcoh]+[ ]+"
    matches = re.findall(formula_regex, question)
    result = "".join([t.strip() for t in list(map(join_tuple_string, matches)) if t != ""]).replace(" ", "")
    question_without_formula = question.replace(result, "")
    return question_without_formula.lower()


def remove_mention(q_with_mention, mention):
    stop_words = ["what", "is", "are", "the", "more", "less",
                  "than", "species", "find", "all", "over",
                  "under", "show", "me", "with",
                  "chemical species", "having", "that", "can", "be"
                  ]
    flag_words = ["mops", "cbu", "assembly model"]
    if "mops" not in q_with_mention.lower():
        stop_words += ["with"]

    tokens = [t for t in q_with_mention.split(" ") if t.lower() not in stop_words]
    q_with_mention = " ".join(tokens).strip()

    for flag_word in flag_words:
        if flag_word in q_with_mention.lower():
            print('-----------')
            print("DEBUG for mention:", mention)
            print("DEBUG for question:", q_with_mention)
            print('-----------')
            q_with_mention = q_with_mention.lower().replace(mention.lower(), "")
            return q_with_mention

    if "reaction" not in q_with_mention.lower():
        q_with_mention = q_with_mention.lower().replace(mention.lower(), "")
        tokens = [t for t in q_with_mention.split(" ") if t.lower() not in stop_words]
        q_with_mention = " ".join(tokens).strip()
        return q_with_mention
    else:
        return "reaction"


def remove_non_formula_species(question_with_non_formula):
    regex = r"[ \w]+  | of [\w]+|[ \w]+'s"
    result = re.sub(regex, "", question_with_non_formula, 0)
    if result:
        return result
    else:
        return question_with_non_formula

    # non_formula_regex = r"[ \w]+  | of [\w]+|[ \w]+'s"
    # matches = re.search(non_formula_regex, question_with_non_formula)
    # if matches:
    #     print("match:", matches.group())

    # result = "".join([t for t in list(map(join_tuple_string, matches)) if t != ""])# .replace("  ", "##").replace(" ", "").replace("##", " ")
    # # print("non formula", result)
    # question_without_non_formula = question_with_non_formula.replace(result, "").replace("'s", "").replace(" of ", " ").strip()
    question_without_non_formula = question_with_non_formula
    return question_without_non_formula.lower()


class CrossGraphFilter:

    def __init__(self):
        self.stop_words = ["whats", "what", "is", "are", "the", "more", "less",
                           "than", "species", "find", "all", "over",
                           "under", "show", "me", "with",
                           "chemical species", "having", "that", "can", "be", "?", "give", "created", "waht", "chemical"
                           ]
        self.global_stop_words = ["g/mol", "mn/m", "dalton", "celsius", "show", "give", "find", "all", "the", "species"]
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
        # original_question = question.replace("'s", " of ")
        tokens = [t for t in question.strip().split(" ") if t.lower() not in self.global_stop_words]
        original_question = " ".join(tokens)
        original_question = remove_formula(original_question)
        # print("question after removing formula:", original_question)

            # return ["ontomops"], torch.tensor([0, 0, 0, 0, 0, 0, 0, 1, 0])
        # mention = self.ner_lite.get_mention(question)
        # question = remove_mention(question, mention=mention)

        _, original_question = NumericalTools.qualifier_value_extractor(original_question, for_filter=True)


        value, value_str = NumericalTools.numerical_value_extractor(question=original_question)
        if value is not None:
            original_question = original_question.replace(value_str, "")

        # TODO: remove the qualifiers from the question

        # return original_question
        original_question = remove_non_formula_species(original_question).replace(" of", "")
        return original_question.lower().strip()


if __name__ == "__main__":
    my_filter = CrossGraphFilter()
    # filtered_question = my_filter.filter_before_cross_graph("find all species with molecular weight over 100")
    # print("filtered question:", filtered_question)
    question = "Thermal Properties of C2H2 at 472.9 K and 123000 Pa"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")


    question = "Thermal Properties of C2H2 at 472.9K and 123000 Pa"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "waht is the molecular weight of CH4"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "What is the power conversion efficiency of c6h6?"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "What is H2's Enthalpy"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "What is the Gibbs energy of C3H4?"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "What is ethylene glycol's power conversion efficiency?"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "waht is the molecular weight of CH4"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "waht is the molecular weight of C3H4O5"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")


    question = "waht is the molecular weight of H2O"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "waht is the molecular weight of O2"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "waht is the molecular weight of H2"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "waht is the molecular weight of co2"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "waht is the molecular weight of CO2"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "waht is the molecular weight of h2"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "find species with molecular weight more than 100 g/mol"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")


    question = "What is benzene's power conversion efficiency?"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "What is power conversion efficiency of benzene?"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "Find all species with boiling point above 0 celsius"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")


    question = "what is the heat capacity of C3H4O"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")

    question = "What are the chemical species having molecular weight around 150 g/mol"
    rst = my_filter.filter_before_cross_graph(question=question)
    print("===========================")
    print(rst)
    print("===========================")