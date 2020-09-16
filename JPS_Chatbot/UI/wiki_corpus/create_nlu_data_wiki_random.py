import json, re, time, random
import nltk
from nltk.stem import PorterStemmer
from datetime import date

ps = PorterStemmer()

# size_of_trainning = 500000

labels = json.loads(open('corpus_for_trainning').read())
c_labels = labels[0]
i_labels = labels[1]
p_labels = labels[2]

print('number of c_labels', len(c_labels))
print('number of p_labels', len(p_labels))

with open('wiki_corpus_lda', 'w') as f:
    labels = c_labels + p_labels  #
    f.write(json.dumps(labels))

# generate different types of questions ... 

# batch_restriction_query e.g. give me the [flash point](attribute) of all the [fossil fuels](class)
# item_attribute_query e.g. what is the freezing point of benzene
question_pool = ['what is', 'show me the', 'show the', 'list the', 'whats the', 'what\'s the', '', 'what are the',
                 'find the', 'the']


# generate different types of questions ... 

# batch_restriction_query e.g. give me the [flash point](attribute) of all the [fossil fuels](class)
# item_attribute_query e.g. what is the freezing point of benzene

# to identify the grammatic structure of the attribute and generate the questions in the proper form 

# e.g. What is xxx(instance) 


def parse_attribute(p_label, c_or_i_label, type):
    structure_dictionary = {}

    # remove DT ... 
    # ['formed', 'from'] the pos tree [('formed', 'VBN'), ('from', 'IN')]
    print('------------- parsing the attribute ---------')
    stop_tags = ['DT']
    text = nltk.word_tokenize(p_label)
    # text = [ ps.stem(w.lower()) for w in text]
    word_tag_pairs = nltk.pos_tag(text)
    tags = [pair[1] for pair in word_tag_pairs if pair[1] not in stop_tags]
    # seperate ['VBN', 'IN']
    tag_chain = ' '.join(tags)

    pool_1 = ['', 'what', 'how', 'when', 'where', 'who']
    pool_2 = ['what is', 'what are', 'show me', 'show', 'give', 'give me', 'list', '']
    WH_word = random.choices(pool_1, weights=(5, 70, 10, 5, 5, 5), k=1)[0] + ' '

    ###################################################################################################
    print(text)
    print(tag_chain)

    # if 'VBN' in tags or 'V' in tags: # e.g. what is xxx used for
    if tag_chain == 'VBN IN' or tag_chain == 'VBN IN IN' or tag_chain == 'RB VBN IN' or tag_chain == 'RB VBZ IN':
        temp = WH_word + random.choices(
            ['can [%s](%s) be [%s](attribute)', 'is [%s](%s) [%s](attribute)', 'are [%s](%s) [%s](attribute)'],
            weights=(10, 45, 45), k=1)[0]
        question_template = temp % (c_or_i_label, type, p_label.strip())
    elif tag_chain == 'VBN' or tag_chain == 'VBN IN NN IN':  # e.g. coined, invented, awarded , VBN IN NN IN named in honor of
        temp = WH_word + random.choices(
            ['can [%s](%s) be [%s](attribute)', 'is [%s](%s) [%s](attribute)', 'are [%s](%s) [%s](attribute)', ],
            weights=(10, 45, 45), k=1)[0]
        question_template = temp % (c_or_i_label, type, p_label.strip())
    elif tag_chain == 'VB NN' or tag_chain == 'VBZ NN' or tag_chain == 'VBZ NNS' or tag_chain == 'VB NNS':
        # e.g. has color  what color does xxx have
        verb = text[0]
        noun = text[1]
        question_template = 'what [%s](attribute) does [%s](%s) %s' % (
            noun.strip(), c_or_i_label.strip(), type, verb.strip())

    elif tag_chain == 'MD VB':
        question_template = 'what does [%s](%s) [%s](attribute)' % (c_or_i_label, type, p_label)

    elif tag_chain == 'VBN IN NN' or tag_chain == 'VBN IN NNS' or tag_chain == 'VBN IN NNP':
        # e.g. described at webpage -> which webpage is xxx described at / at what webpage is xxx described ...
        verb = text[0]
        prep = text[1]
        noun = text[2]
        wh_word = random.choice(['which', 'what'])
        opt_1 = '%s [%s](attribute) is %s(%s) [%s %s](attribute)' % (wh_word, noun, c_or_i_label, type, verb, prep)
        opt_2 = '[%s](attribute) %s [%s](attribute) is %s(%s) [%s](attribute) ' % (
            prep, wh_word, noun, c_or_i_label, type, verb)
        question_template = random.choice([opt_1, opt_2])


    elif tag_chain == 'NN IN':
        # e.g. activator of -> xxx is the activator of what / what is xxx an activator of
        article = random.choice(['the', 'a'])
        opt_1 = '[%s](%s) is %s [%s](attribute) what' % (c_or_i_label, type, article, p_label)
        opt_2 = 'what is [%s](%s) %s [%s](attribute)' % (c_or_i_label, type, article, p_label)
        question_template = random.choice([opt_1, opt_2])



    # TODO: make type 4 question: xxx is the activator of which xxx , which xxx is yyy an activator of ?

    else:  # the general_case, we category anything else into this question type, what is the a(attribute) of b (class/instance)

        WH_word = random.choices(pool_2, weights=(12.5, 12.5, 12.5, 12.5, 12.5, 12.5, 12.5, 12.5,), k=1)[0] + ' ' + \
                  random.choices(['the', ''], weights=(90, 10))[0] + ' '
        question_template = WH_word + random.choices(
            ['[%s](attribute) of [%s](%s)' % (p_label.strip(), c_or_i_label, type),
             '[%s](%s)\'s [%s](attribute)' % (c_or_i_label, type, p_label.strip())], weights=(80, 20), k=1)[0]

    # if tag_chain in structure_dictionary:
    # structure_dictionary[tag_chain].append(question_template)
    # else:
    # structure_dictionary[tag_chain] = [question_template]
    # with open('question_set', 'w') as f:
    # f.write(json.dumps(structure_dictionary))
    # f.close()
    #
    #
    print(question_template)
    print('==============================\n')
    return question_template


def generate_type_2_questions():
    type_2_questions = []
    # print(random.choice(i_labels))
    # print(random.choice(p_labels))
    # print(random.choice(question_pool))
    # ## intent: item_attribute_query
    # - what is the [image](attribute) of [H2O2](entity)
    # for i in range(0, size_of_trainning):

    for i_label in i_labels:
        for p_label in p_labels:
        # p_label = random.choice(p_labels)
        # i_label = random.choice(i_labels)
        # def parse_attribute(p_label, c_or_i_label, type):
            question = parse_attribute(p_label, i_label, 'entity')
            if question not in type_2_questions:
                type_2_questions.append(question)

    with open('type_2_questions', 'wb') as f:
        content = '## intent: item_attribute_query\n'
        for q in type_2_questions:
            line = '- ' + q + '\n'
            content = content + line

        f.write(content.encode('utf-8'))
        f.close()


# to generate questions with intent batch_restriction_query e.g. What are the pka constants of all acids
def generate_type_1_questions():
    type_1_questions = []

    print(random.choice(c_labels))
    print(random.choice(p_labels))
    print(random.choice(question_pool))

    #for p in range(0, size_of_trainning):
    for p_label in p_labels:
        for c_label in c_labels:
            # p_label = random.choice(p_labels)
            # c_label = random.choice(c_labels)
            question = parse_attribute(p_label, c_label, 'class')
            if question not in type_1_questions:
                type_1_questions.append(question)

    with open('type_1_questions', 'wb') as f:
        content = '## intent: batch_restriction_query\n'
        for q in type_1_questions:
            line = '- ' + q + '\n'
            content = content + line

        f.write(content.encode('utf-8'))
        f.close()


superlative_words_pool = ['larger_than', 'smaller_than', 'under', 'over', 'higher than', 'lower than', 'more than',
                          'less than','above', 'beneath','fewer'
                          'smaller than', 'bigger than', 'broader']
mid_words_pool = ['with', 'that have', 'having', 'of']

def generate_random_numerical_value():
    numerical_value = random.choices([random.randint(-1000, 1000), random.random()], weights=(75, 25), k=1)[0]
    return numerical_value

def generate_type_3_questions():
    type_3_questions = []
    # e.g. find all the fatty acids with molecular weight more than 100
    template = '%s [%s](class) %s [%s](attribute) [%s](comparison) [%s](numerical_value)'  # wh_word, class, mid_word, property ,superlative_word, numerical_value

    # for i in range(0, size_of_trainning):
    for c_label in c_labels:
        for p_label in p_labels:
            # c_label = (random.choice(c_labels))
            # p_label = (random.choice(p_labels))
            wh_word = (random.choice(question_pool))
            mid_word = random.choice(mid_words_pool)
            superlative_word = random.choice(superlative_words_pool)
            # e.g. find all the alkenes with molecular weight more than 200
            question = template % (wh_word, c_label, mid_word, p_label, superlative_word, generate_random_numerical_value())
            type_3_questions.append(question)
            print(question)

    with open('type_3_questions', 'wb') as f:
        content = '## intent: batch_restriction_query_numerical\n'
        for q in type_3_questions:
            line = '- ' + q + '\n'
            content = content + line

        f.write(content.encode('utf-8'))
        f.close()


def generate_type_4_questions():
    type_4_questions = []
    # gold_question = 'what is the pka of all the acids with a molecular weight over 200'
    template = '%s [%s](attribute) of [%s](class) %s [%s](attribute) [%s](comparison) [%s](numerical_value)'
    # wh_word, attribute, class, mid_word, attribute, comparison, number
    # for i in range(0, size_of_trainning):
    #     c_label = (random.choice(c_labels))
    #     p_label_1 = (random.choice(p_labels))
    #     p_label_2 = (random.choice(p_labels))
    for p_label_1 in p_labels:
        for p_label_2 in p_labels:
            for c_label in c_labels:
                wh_word = (random.choice(question_pool))
                mid_word = random.choice(mid_words_pool)
                superlative_word = random.choice(superlative_words_pool)
                question = template % (wh_word, p_label_1, c_label, mid_word, p_label_2, superlative_word ,generate_random_numerical_value())
                type_4_questions.append(question)

    with open('type_4_questions', 'wb') as f:
        content = '## intent: batch_restriction_query_numerical_and_attribute\n'
        for q in type_4_questions:
            line = '- ' + q + '\n'
            content = content + line

        f.write(content.encode('utf-8'))
        f.close()



def merge_files():
    # Reading data from file1 
    with open('type_1_questions') as fp:
        data = fp.read()

        # Reading data from file2
    with open('type_2_questions') as fp:
        data2 = fp.read()

        # Reading data from file3
    with open('type_3_questions') as fp:
        data3 = fp.read()

    with open('type_4_questions') as fp:
        data4 = fp.read()
        # Merging 2 files
    # To add the data of file2 
    # from next line 
    data += "\n"
    data += data2
    data += data3
    data += data4

    with open('nlu_%s.md' % str(date.today()), 'w') as fp:
        fp.write(data)


generate_type_1_questions()
generate_type_2_questions()
generate_type_3_questions()
generate_type_4_questions()
merge_files()
