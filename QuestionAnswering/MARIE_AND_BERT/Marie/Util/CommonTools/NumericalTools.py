import re


def numerical_value_extractor(question):
    """

    :param question: question in text
    :return: numerical value (float) extracted from the question
    """
    if "-" in question:
        return None, ""

   #  numerical_values = re.findall(r"[ ][-]*\d+[\.]*\d+", question)
    # numerical_values = re.findall(r"[ ][-]*\d+[\.]*\d*", question)
    numerical_values = re.findall(r"[ ]\d+[\.]*\d*", question)
    if len(numerical_values) > 0:
        return float(numerical_values[0]), numerical_values[0].strip()
    else:
        return None, ""


def numerical_value_extractor_with_no_space(text):
    # print("text", text)
    numerical_values = re.findall(r"[-]*\d*[\.]*\d+", text)
    # print("numerical_values", numerical_values)

    if len(numerical_values) > 0:
        return float(numerical_values[0]), numerical_values[0]
    else:
        return None, ""


def qualifier_value_extractor(question_before_qualifer, for_filter=False):
    """
    The function separates the numerical value and unit for the qualifiers
    :param question: question in string
    :return: a dictionary with two keys: temperature and pressure, if no value is under the category, it returns
    None
    """
    lower_case_question = question_before_qualifer.lower()
    temperature_regex = r"(((\d+[\.]*\d+)|[0-9])+[ ]*(kelvin|k|celsius|farenheit|degree[s]*)+[ ]*(celsius)*)|(room temperature)"
    # pressure_regex = r"([ ][-]*\d+[\.]*\d+[ ]*(atm|bar|pascal|p|pa|bar)) | (room temperature)"
    pressure_regex = r"((\d+[\.]*\d+)|[0-9])[ ]*(atm|bar|pascal|pa|bar)"
    temperature = re.search(temperature_regex, lower_case_question)
    pressure = re.search(pressure_regex, lower_case_question)
    lower_case_question = lower_case_question.replace(" at ", " ")

    # print("question:", lower_case_question)
    if temperature:
        temperature = temperature.group()
        # print("temperature", temperature)
        lower_case_question = lower_case_question.replace(temperature, "")
        temperature, _ = numerical_value_extractor_with_no_space(temperature)
        # print(temperature)

    if pressure:
        pressure = pressure.group()
        # print("pressure", pressure)
        lower_case_question = lower_case_question.replace(pressure, "").replace(" and", " ").strip()
        pressure, _ = numerical_value_extractor_with_no_space(pressure)
        # print(pressure)
    if for_filter:
        question_to_return = lower_case_question
    else:
        question_to_return = question_before_qualifer

    if (temperature is None) and (pressure is None):
        return {}, question_to_return
    else:
        return {"temperature": temperature,
                "pressure": pressure}, question_to_return


if __name__ == "__main__":

    question = "Find all species with boiling point above 0 celsius"
    value, value_str = numerical_value_extractor(question)
    if value is not None:
        question = question.replace(value_str, "")
    print(question)


    # rst = qualifier_value_extractor("what is the heat capacity of benzene at room temperature and 100 pa")
    # print(rst)
    # rst = qualifier_value_extractor("what is the heat capacity of benzene at 123 degrees and 100 bar")
    # print(rst)
    # rst = qualifier_value_extractor("what is the heat capacity of benzene and 1 atm")
    # print(rst)
