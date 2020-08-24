from Chatbot.CoordinateAgent import CoordinateAgent


# ca = CoordinateAgent()
# ca.run('what is the weight of all the alkines with a boiling point less than -100')

questions = []
def load_and_process_questions():
    with open('../wiki_corpus/type_4_questions') as f:
        contents = f.readlines()[1:]
    for line in contents:
        line = line.replace('(comparison)', '')
        line = line.replace('(attribute)', '')
        line = line.replace('(numerical_value)', '')
        line = line.replace('(class)', '')
        line = line.replace('[', '').replace(']', '')
        questions.append(line)

load_and_process_questions()