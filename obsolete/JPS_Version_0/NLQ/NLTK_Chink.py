from urllib.error import URLError

import nltk
from nltk import word_tokenize
from nltk.corpus import stopwords
import urllib.request
from urllib.parse import quote
import json
import multiprocessing
import NLTK_SPARQL
import NLTK_formatter
import NLTK_log
import NLTK_CompareString


def join_chunk(words, simple_join):
    simple_join = True
    # TODO apply the stop-word filter at this point
    stop = set(stopwords.words('english'))
    stop.update(['did'])
    raw_words = []
    for word in words:
        if word[0].lower() not in stop:
            raw_words.append(word[0])

    word_list = []
    word_list2 = []
    result_list = []
    origin_string = ''
    for word in raw_words:
        word_list.append(word)
        origin_string += ' ' + word
        result_list.append(' '.join(word_list))

    for word in raw_words[::-1][:-1]:
        word_list2.append(word)
        result_list.append(' '.join(word_list2[::-1]))
    if simple_join:
        # return origin_string
        # logger.write(words)
        NLTK_log.logger(raw_words)
        if raw_words:
            return {'list': [' '.join(raw_words)], 'origin': origin_string}
        else:
            # NLTK_log.logger.write('the shit is empty')
            return {'list': [], 'origin': origin_string}

    else:
        # NLTK_log.logger.write('list:', result_list)
        return {'list': result_list, 'origin': origin_string}


def lookup(term_obj):
    term_list = term_obj['list']
    original_term = term_obj['origin']
    jobs = []
    manager = multiprocessing.Manager()
    return_dict = manager.dict()
    return_dict['result'] = ''
    return_dict['list_of_candidates'] = []
    for term in term_list:
        p = multiprocessing.Process(target=single_request, args=(term, original_term, return_dict))
        jobs.append(p)
        p.start()
    for j in jobs:
        j.join()

    sorted_list_of_candidate = sorted(return_dict['list_of_candidates'], key=lambda score: score[0], reverse=True)

    # NLTK_log.logger.write('Search result for term ', original_term)
    # NLTK_log.logger.write(sorted_list_of_candidate)
    # NLTK_log.logger.write('========================= end of a row ===============================')
    if len(sorted_list_of_candidate) >= 5:
        return sorted_list_of_candidate[:5]
    else:
        return sorted_list_of_candidate


def single_request(term, original_term, return_dict):
    url = 'http://lookup.dbpedia.org/api/search/PrefixSearch?QueryClass=&MaxHits=10&QueryString=' + quote(term)
    headers = {}
    headers['Accept'] = 'application/json'
    request = urllib.request.Request(url, headers=headers)
    response = urllib.request.urlopen(request)
    json_response = json.loads(response.read().decode('utf-8', 'replace'))

    #   send a single request to dbpedia's lookup service, an json obj will be returned
    #   the obj contains multiple candidates in 'result' array, this part will iterate through all the candidates
    # and calculate a score regarding its similarity with the original phrase.
    # e.g. 'the Grand Prix' and '2011 Abu Dhabi Grand Prix'

    list_of_candidates = []
    for result in json_response['results']:
        uri = result['uri']
        label = result['label']
        score = round(NLTK_CompareString.phraseSimilarity(original_term, label), 2)
        NLTK_log.logger(original_term, label, score)
        new_tuple = (score, uri, label)
        list_of_candidates.append(new_tuple)

    # append list_of_candidates to s
    return_dict['list_of_candidates'] += list_of_candidates


# similarity_score = NLTK_CompareString.phraseSimilarity(original_term,json_response[])
# report_generator = NLTK_formatter.ReportGenerator(return_dict, json_response, term, original_term)
# report_generator.generate_html_report()
# NLTK_log.logger.write('###', return_dict)


s1 = '''who served in Apollo 11 mission'''
s2 = '''who was the doctoral supervisor of Albert Einstein'''
s3 = '''how many pages are there in war and peace'''
s4 = '''when was PRC established'''
s5 = '''what is in a chocolate chip cookie'''
s6 = '''Which electronics companies were founded in Beijing'''
s7 = '''How many movies did Park Chan-wook direct'''


def test(index, sentence):
    # if __name__ == "__main__":
    tokens = nltk.word_tokenize(sentence)
    tagged = nltk.pos_tag(tokens)
    grammar = '''
        Sub_Obj:
            {<.*>+}
            }<VBD|IN|WP|VBP|RB|WRB|VBZ|VBN|WP|WDT|VB|RBS|JJS|DT>+{
        Predicate:
            {<JJ>*<VBN|VBD|VB|IN>}	
        Question:
            {<WP|WRB><JJ>}
        JJS:	
            {<JJS>}
        RBS:
            {<RBS>}
        JJNN:
            {<JJ><NN|NNP|NNS*>}
        '''

    cp = nltk.RegexpParser(grammar)
    result = cp.parse(tagged)
    # ne = nltk.ne_chunk(tagged)

    with open('test1.html', 'a') as file:
        file.write('<h2>SENTENCE: ' + str(index) + ':' + sentence + '</h2><br/>')

        # for tree in ne:
        # if type(tree) == nltk.Tree:
        # file.write('<p>' + str(tree.label()) + str(tree.leaves()) + '/<p><hr/>')

        #
        global_selected_predicate_candidates = []
        for tree in result:
            if type(tree) == nltk.Tree:
                if tree.label() == 'Predicate':
                    NLTK_log.logger(tree.leaves())
                    global_selected_predicate_candidates.append(join_chunk(tree.leaves(), True)['list'])
                if tree.label() == 'Sub_Obj':
                    NLTK_log.logger(tree.leaves())
                    global_selected_predicate_candidates += join_chunk(tree.leaves(), False)['list']

        # The first iteration of going through the tree, make a list of all candidates for predicates

        for tree in result:
            if type(tree) == nltk.Tree:
                file.write('<p>' + str(tree.label()) + str(tree.leaves()) + '/<p><hr/>')

                if tree.label() == 'Sub_Obj':
                    # the list below is only used for one specific chunk
                    _selected_sub_obj_candidates_for_one_chunk = []
                    candidates = (lookup(join_chunk(tree.leaves(), False)))
                    # in each sub_obj chunk, returns a list of candidates of uri

                    for i, candidate in enumerate(candidates):
                        file.write(str(i) + '--' + str(candidate) + '<br/>')
                        _selected_sub_obj_candidates_for_one_chunk.append(candidate)
                        #if i >= 3:
                        #    break

                    # select out x candidates of highest scores

                    # ready to fire a SPARQL query
                    print('Selected sub obj candidates', _selected_sub_obj_candidates_for_one_chunk)
                    for selected in _selected_sub_obj_candidates_for_one_chunk:
                        sparql_engine = NLTK_SPARQL.SPARQLEngine()

                        # FIXME Implement an error handling mechanism
                        try:

                            predicate_list_from_query = sparql_engine.fire_query(
                                sparql_engine.construct_pre_query_as_sub(selected[1]), selected[1], 'sub')

                            print('first list', predicate_list_from_query)

                            predicate_list_from_query += sparql_engine.fire_query(
                                sparql_engine.construct_pre_query_as_obj(selected[1]), selected[1], 'obj')

                            print('second list', predicate_list_from_query)
                            # stop = input('stop')

                        except URLError:
                            NLTK_log.logger('Something wrong with the URL')
                        except TimeoutError:
                            NLTK_log.logger('There is a timeout error')

                        # TODO Add both subjects and predicates into a candidate list for predicates
                        # TODO Let sparql_engine.fire_query return a list of predicates from the query
                        score_board_for_predicate_candidates = []
                        NLTK_log.logger('From term:', selected[1])
                        for predicate_from_query in predicate_list_from_query:
                            for predicate_candidate in global_selected_predicate_candidates:
                                if predicate_candidate and (predicate_candidate not in selected) and len(
                                        predicate_candidate) >= 1:
                                    NLTK_log.logger('line 207: ', predicate_candidate, predicate_from_query['label'])
                                    if type(predicate_candidate) is str:
                                        similarity = NLTK_CompareString.phraseSimilarity(predicate_candidate,
                                                                                         predicate_from_query['label'])
                                    else:
                                        similarity = NLTK_CompareString.phraseSimilarity(predicate_candidate[0],
                                                                                         predicate_from_query['label'])

                                    score_board_for_predicate_candidates.append((similarity, predicate_candidate,
                                                                                 predicate_from_query))

                        sorted_score_board_for_predicate_candidates = sorted(score_board_for_predicate_candidates,
                                                                             key=lambda score: score[0], reverse=True)
                        for x in sorted_score_board_for_predicate_candidates:
                            if x[0] >= 0.1:
                                NLTK_log.logger(x)

                        NLTK_log.logger(
                            '=====================================================================================')
