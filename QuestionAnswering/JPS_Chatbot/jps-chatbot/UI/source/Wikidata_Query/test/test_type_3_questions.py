from source.CoordinateAgent import CoordinateAgent

ca = CoordinateAgent()

questions = []


def load_and_process_questions():
    with open('../../wiki_corpus/type_3_questions') as f:
        contents = f.readlines()[1:]
    for line in contents:
        line = line.replace('(entity)', '')
        line = line.replace('(comparison)', '')
        line = line.replace('(attribute)', '')
        line = line.replace('(numerical_value)', '')
        line = line.replace('(class)', '')
        line = line.replace('[', '').replace(']', '')
        questions.append(line)

    return questions


def append_log(content):
    with open('test_log_type_3', 'a') as f:
        f.write(str(content) + '\n -------------- \n')


questions = load_and_process_questions()
for q in questions:
    r = ca.run(q)
    if r is not None and r[1] == 1:
        print('this is a big big achievement')
        append_log(q)

        append_log('----------')
        append_log(r[2])
