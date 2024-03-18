from pprint import pprint
import itertools


def generate_combinations(results):
    # TODO: to generate a list of combinations with a score ...
    temp = []


    for uris in results:
        if type(uris) is str:
            uris = [uris]
            temp.append(uris)
        else:
            temp.append(uris)
    list_of_combination = list(itertools.product(*temp))
    # TODO: calculate a score for the results
    list_of_combination_with_score = []
    for combination in list_of_combination:
        combination_score = 1
        for item in combination:
            if type(item) is tuple:
                uri = item[0]
                score = item[1]
                if score != 0:
                    combination_score = combination_score * score

        list_of_combination_with_score.append({'uris': combination, 'score': combination_score})

    sorted_list = [t for t in sorted(list_of_combination_with_score, key=lambda item: item['score'], reverse=True)]
    return sorted_list


def retrieve_uris_from_entities(entities, order):
    # get the object by label, the remove the object

    temp_list = entities
    results = []
    for o_label in order:
        for e in entities:
            key = ''
            uris = []
            for k, u in e.items():
                key = k
                uris = u
            if key == o_label:
                # return the result, remove it from the list
                results.append(uris)

    return results
    # TODO: put the comparison and the number in, apply filter over them


def fill_sparql_query_for_one_intent(intent, template, order, entities, index_order):
    list_of_sparqls = []
    # this function takes the template, entities, the order, returns a list of sparql queries
    r = retrieve_uris_from_entities(entities, order=order)
    combinations = generate_combinations(r)
    # x = input()
    # formula of fatty acid with density more than 40
    # aromatic hydrocarbon with molecular weight more than 200
    for comb in combinations:
        elements = []
        uris = comb['uris']
        for u in uris:
            if type(u) is tuple:
                elements.append(u[0])
            else:
                elements.append(u)

        try:
            candidates = []
            for i in index_order:
                candidates.append(elements[i])
            elements = tuple(candidates)
            # sparql_query = template % (elements[2], elements[1], elements[0], elements[0], elements[3], elements[4])
            sparql_query = template % elements
            list_of_sparqls.append(sparql_query)
        except:
            return None
    return list_of_sparqls


def rename_keys(es):
    counter = 0
    for e in es:
        counter = counter + 1
        k = list(e.keys())[0]
        if k == 'attribute':
            new_k = 'attribute_' + str(counter)
            e[new_k] = e.pop(k)
        print(e)
    return es

entities = [{'attribute': [('P117', 100), ('P556', 74)]}, {'attribute': [('P8224', 71), ('P2067', 67)]},
            {'class': [('Q230731', 100), ('Q904441', 80), ('Q407212', 78)]}, {'comparison': '>'},
            {'numerical_value': '200'}]
order = ['attribute_1', 'attribute_2', 'class', 'comparison', 'numerical_value']
index_order = [2, 1, 0, 0, 3, 4]

entities = rename_keys(entities)

r = retrieve_uris_from_entities(entities, order=order)
pprint(r)
print('==============')
combinations = generate_combinations(r)

pprint(combinations)