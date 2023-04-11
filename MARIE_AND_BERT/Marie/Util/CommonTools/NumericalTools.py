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


def qualifier_value_extractor(question):
    question = question.lower()
    print("question:", question)
    temperature_regex = r"((\d+[\.]*\d+)|[0-9])+[ ]*(kelvin|k|celsius|farenheit|degree[s]*)+[ ]*(celsius)*"
    pressure_regex = r"([ ][-]*\d+[\.]*\d+[ ]*(atm|bar|pascal|p|pa|bar)) | (room temperature)"
    xpressure_regex = r"((\d+[\.]*\d+)|[0-9])[ ]*(atm|bar|pascal|p|pa|bar)|(room temperature)"

    temperature = re.search(temperature_regex, question)
    pressure = re.search(xpressure_regex, question)


    if temperature:
        temperature = temperature.group()

    if pressure:
        pressure = pressure.group()
        # pressure = [p for p in pressure[0] if p != ""]


    return {"temperature": temperature,
            "pressure": pressure}


if __name__ == "__main__":
    rst = qualifier_value_extractor("what is the heat capacity of benzene at room temperature and 100 pa")
    print(rst)
    rst = qualifier_value_extractor("what is the heat capacity of benzene at 123 degrees and 100 bar")
    print(rst)
    rst = qualifier_value_extractor("what is the heat capacity of benzene and 1 atm")
    print(rst)