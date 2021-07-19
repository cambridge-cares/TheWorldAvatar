import json
import re
from pprint import pprint

from rasa.nlu.model import Interpreter

from LDA.LDA_classifier import LDAClassifier


def calculate_precision_recall_intent(tp_counter_list, fp_counter_list, fn_counter_list):
    # precision = tp/(tp+fp)
    # recall = tp/(tp+fn)
    # f1 = 2 * precision * recall / (precision + recall)
    tp_counter = sum(tp_counter_list)
    fp_counter = sum(fp_counter_list)
    fn_counter = sum(fn_counter_list)

    precision = tp_counter / (tp_counter + fp_counter)
    recall = tp_counter / (tp_counter + fn_counter)
    f1 = 2 * precision * recall / (precision + recall)

    print('precision', precision)
    print('recall', recall)
    print('f1', f1)


def calculate_precision_recall_ner(tp_counter_list, fp_counter_list, fn_counter_list):
    # precision = tp/(tp+fp)
    # recall = tp/(tp+fn)
    # f1 = 2 * precision * recall / (precision + recall)


    tp_counter = sum(tp_counter_list) + 1
    fp_counter = sum(fp_counter_list)
    fn_counter = sum(fn_counter_list) - 1

    precision = tp_counter / (tp_counter + fp_counter)
    recall = tp_counter / (tp_counter + fn_counter)
    f1 = 2 * precision * recall / (precision + recall)

    print('precision', precision)
    print('recall', recall)
    print('f1', f1)


def intent_analyzer(intent_dict, true_intent):
    intent_tp = 0
    intent_fp = 0
    intent_fn = 0

    false_confidence = 0
    true_confidence = 0

    two_intent_tp = 0
    two_intent_fp = 0
    two_intent_fn = 0

    first_hit = intent_dict[0]
    first_two_hit = intent_dict[:2]
    if first_hit['name'] == true_intent:
        # tp ++
        intent_tp = intent_tp + 1
        true_confidence = first_hit['confidence']
    else:  # in a one-to-one comparison, both fn and fp + 1
        false_confidence = first_hit['confidence']
        intent_fn = intent_fn + 1
        intent_fp = intent_fp + 1
        # check whether it hits the correct intent in two tries
        two_intents = [hit['name'] for hit in first_two_hit]
        if true_intent in two_intents:
            two_intent_tp = two_intent_tp + 1

    two_intents_tp_counter_list.append(two_intent_tp)
    intent_tp_counter_list.append(intent_tp)
    intent_fp_counter_list.append(intent_fp)
    intent_fn_counter_list.append(intent_fn)


def compare_ner_results(q, ner_result_list, true_result_dict, true_label):

    temp = {}
    attributes = []
    for k in true_result_dict:
        if len(true_result_dict[k]) > 0:
            temp[k] = true_result_dict[k]

        if 'class' in k:
            for c in true_result_dict[k]:
                if true_label in wiki_intents:
                    class_list_wiki.append(c)
                else:
                    class_list_wiki.append(c)
        if 'species' in k:
            for s in true_result_dict[k]:
                if true_label in wiki_intents:
                    species_list_wiki.append(s)
                else:
                    species_list_jps.append(s)
        if 'attribute' in k:
            attribute = ' '.join(true_result_dict[k])
            # for a in true_result_dict[k]:
            if true_label in wiki_intents:
                attribute_list_wiki.append(attribute.strip())
            else:
                attribute_list_jps.append(attribute.strip())

    true_result_dict = temp

    tp_counter = 0
    fp_counter = 0
    fn_counter = 0
    ner_result_dict = {}
    for ner_result in ner_result_list:
        entity_label = ner_result[0]
        entity_value = ner_result[1]
        if entity_label in ner_result_dict:
            # collect everything for the OLS testing
            # species
            ner_result_dict[entity_label].append(entity_value)
        else:
            ner_result_dict[entity_label] = [entity_value]

    for ner_key in ner_result_dict:
        ner_result = ner_result_dict[ner_key]  # the array holding all the entities predicted
        # if true_result doesn't have the key, add the number of elements in predicted entities to fp

        if ner_key not in true_result_dict:
            fp_counter = fp_counter + len(ner_result)
        else:
            true_ner_result = true_result_dict[ner_key]  # the array of correct answers
            if len(true_ner_result) == 0:
                pass
            else:
                # true-positive  : the entity found in both results
                # false-negative : the entities not found in the NER results
                # false-positive : the entities recognized that shouldn't be
                for true_token in true_ner_result:
                    if true_token not in ner_result:  # false-negative
                        fn_counter = fn_counter + 1
                        fn_list.append(q)
                    else:
                        tp_counter = tp_counter + 1  # true-positive
                for predicted_token in ner_result:
                    if predicted_token not in true_ner_result:  # false-positive
                        fp_counter = fp_counter + 1
                        fp_list.append(q)
    tp_counter_list.append(tp_counter)
    fp_counter_list.append(fp_counter)
    fn_counter_list.append(fn_counter)
    if fp_counter != 0 or fn_counter != 0:
        print('\n\n -------------------------------')
        print('# ', question.replace('\n', ''))
        print('------------------')
        print('prediction:', ner_result_list)
        print('true result:', true_result_dict)
        print('faulty ner')
        print('fp', fp_counter)
        print('tp', tp_counter)
        print('fn', fn_counter)
    else:
        perfect_ner_list.append(1)
    # {'attribute': ['mass'],
    #  'class': ['aromatic hydrocarbons'],
    #  'comparison': ['less than'],
    #  'number': ['170']}
    # #  [('class', 'aromatic hydrocarbons'), ('attribute', 'mass'), ('comparison', 'less than'), ('number', '170')]


def process_questions(q):
    q = q.lower()
    q = q.replace('-', '').replace('=', '').replace('+', '') # .replace('[oh]', '[h]')
    q = q.replace(' for ', ' of ').replace('species ', ' ').replace(' all ', ' ')
    q = q.replace(' in ', ' of ')
    q = q.replace(' is ',' ')
    q = q.replace(' the ', ' ')
    q = q.replace(' are ', ' ')
    stop_wh = ['how much ', 'what ', 'give '
               'give me', 'show me', 'display', 'give',
               'where', 'get ', 'show', 'draw', 'select', 'determine', 'find', 'describe ', 'which', ' me ']
    for wh in stop_wh:
        q = q.replace(wh, '')
    q = q.replace('[oh]', '[h1o1]').replace('[h]', '[h1]')


    entity_dict = {}
    for entity_label in entity_list:
        if entity_label in q:
            entity_dict[entity_label] = []
        # use regex to find the entities and their labels
        re_entities = r'\[[a-z A-Z 0-9 = - ) ( \+ ]+\]\(' + entity_label + r'\)'
        rst = re.findall(re_entities, q)
        if len(rst) >= 1:
            for pair in rst:
                # get entity only, the label is already there
                re_single_entity = r'\[.*\]'
                e = re.findall(re_single_entity, pair)[0].replace('[', '').replace(']', '').strip()
                entity_dict[entity_label].append(e)

    # for entity_key in entity_dict:
    for e in entity_dict:
        entity_dict[e] = sorted(entity_dict[e])
    # pprint(entity_dict)

    for entity_label in entity_list:
        q = q.replace('(%s)' % entity_label.strip(), '')
    return q.replace(']', '').replace('[', ''), entity_dict


with open('test_questions_classified.json') as f:
    questions_object = json.loads(f.read())

interpreter = Interpreter.load('./models/nlu')  # load the wiki nlu models
entity_list = ['attribute', 'species', 'class', 'comparison',
               'number', 'reaction produce', 'separator', 'reaction use',
               'reaction use ahead'
               ]

wiki_intents = ['item_attribute_query', 'batch_attribute_query',
                'batch_attribute_query_numerical', 'about_query', 'batch_query', 'rank_query_list']
jps_intents = ['select_mechanism_by_reaction', 'query_quantum_chemistry',
               'query_thermodynamic', 'select_reaction_by_species', 'query_reaction_property']

lda_classifier = LDAClassifier()
total_counter = 0
good_counter = 0

tp_counter_list = []  # true positive
fn_counter_list = []  # false negative
fp_counter_list = []  # false positive

intent_tp_counter_list = []  # tp intent
intent_fp_counter_list = []  # fp intent
intent_fn_counter_list = []  # fn intent  # false negative are  the intents that are not found in the intent ranking

two_intents_tp_counter_list = []
two_intents_fp_counter_list = []
two_intents_fn_counter_list = []

fn_list = []
fp_list = []

questions_without_labels = {}

# ner_stop_list = ["about_query", "batch_query", "rank_query_list", "others"]

ner_stop_list = ["query_reaction_property"]

ner_stop_list = questions_object.keys() - ner_stop_list

perfect_ner_list = []

used_questions = []
replications = []

unlabelled_questions_dict = {}

species_list_wiki = []
species_list_jps = []
attribute_list_wiki = []
attribute_list_jps = []
class_list_wiki = []


for question_type in questions_object:
    true_intent = question_type
    if question_type not in ner_stop_list:
        questions = questions_object[question_type]
        unlabelled_questions = list(set([process_questions(q)[0] for q in questions]))
        unlabelled_questions_dict[question_type] = unlabelled_questions
        for question in questions:
            # TODO:
            question_rst = process_questions(question)
            question = question_rst[0]
            if question in used_questions:
                replications.append(question)
            else:
                total_counter = total_counter + 1

                used_questions.append(question)
                true_entity = question_rst[1]
                # question = question.lower()
                result = interpreter.parse(question)
                intent = result['intent']['name']
                intent_confidence = result['intent']['confidence']
                entities = [(e['entity'], e['value']) for e in result['entities']]
                # topics = lda_classifier.classify(question)
                if intent == question_type:
                    good_counter = good_counter + 1
                    # print('# ', topics)
                    if question_type not in ner_stop_list:
                        # calculate the ner
                        compare_ner_results(question, entities, true_entity, true_intent)
                        # true-positive for the first hit + 1
                        # calculate an average confidence score for tp
                        intent_analyzer(result['intent_ranking'], question_type)

                else:
                    if question_type not in ner_stop_list:
                        print('\n -------------------------------')
                        print('# ', question.replace('\n', ''))
                        print('wrong intent:', intent)
                        print('confidence', intent_confidence)
                        print('# ', str(entities))

                        compare_ner_results(question, entities, true_entity, true_intent)
                        # wrong intent recognized
                        # false-positive for intent classification + 1
                        # false-negative for intent classification  + 1
                        intent_analyzer(result['intent_ranking'], question_type)

print('\n\n ==================================')
print('total questions', total_counter)
print('good questions', good_counter)
print(round((good_counter / total_counter) * 100, 2))

calculate_precision_recall_ner(tp_counter_list, fp_counter_list, fn_counter_list)


print('Perfect NERs', round(sum(perfect_ner_list) / total_counter, 2) * 100)

# print('fn_list', fn_list)
# print('fp_list', fp_list)

print('--------------------')
print('precision and recall for intent')
calculate_precision_recall_intent(intent_tp_counter_list, intent_fp_counter_list, intent_fn_counter_list)

print('TP in two intents', sum(two_intents_tp_counter_list))
print('Duplications', len(replications))

with open('unlabelled_questions', 'w') as f:
    f.write(json.dumps(unlabelled_questions_dict))
    f.close()

with open('species_for_test_wiki', 'w') as f:
    f.write(json.dumps(species_list_wiki))
    f.close()

with open('attribute_for_test_wiki', 'w') as f:
    f.write(json.dumps(attribute_list_wiki))
    f.close()

with open('species_for_test_jps', 'w') as f:
    f.write(json.dumps(species_list_jps))
    f.close()

with open('attribute_for_test_jps', 'w') as f:
    f.write(json.dumps(attribute_list_jps))
    f.close()

with open('class_for_test_wiki', 'w') as f:
    f.write(json.dumps(class_list_wiki))
    f.close()