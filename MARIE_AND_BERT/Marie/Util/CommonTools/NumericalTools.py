import re


def numerical_value_extractor(question):
    """

    :param question: question in text
    :return: numerical value (float) extracted from the question
    """
    numerical_values = re.findall(r"[ ][-]*\d+[\.]*\d+", question)
    if len(numerical_values) > 0:
        return float(numerical_values[0]), numerical_values[0]
    else:
        return None, ""
