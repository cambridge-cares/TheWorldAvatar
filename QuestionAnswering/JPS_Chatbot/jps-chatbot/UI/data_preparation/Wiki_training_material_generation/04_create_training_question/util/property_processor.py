import json
import random
# from pattern.en import pluralize, singularize

import nltk

connects = ['containing', 'that contains', 'with', 'having', 'that has', 'of']
# heads = ['what is ', 'show ', 'show me ', 'give ', 'give me', 'provide ', 'find ', 'find me ', 'get ', '']
# whs = ['who', 'where', 'when', 'what', 'at what time', 'at what place']
heads = ['']
whs = ['']

skip_tags = ['NN : NN', 'WRB', 'VBZ ( NN )', 'NN ( JJ NNS )', 'RB JJ TO']
DTs = ['a', 'an', '']
the = ['the', '']
# ALLs = ['all', '', 'all the']
ALLs = ['']
smaller_than = ['smaller than', 'less', 'less than', 'under', 'smaller', 'beneath', 'lower', 'lower than', 'fewer']
larger_than = ['bigger', 'bigger than', 'larger than', 'larger', 'over', 'above', 'beyond', 'broader', 'broader ',
               'than', 'more than', 'more']

simple_properties = ['RBR JJ NN', 'JJ NN IN NN', 'JJ JJ NN', 'JJ JJ NN NN', 'JJ NN NN', 'JJ NNP', 'JJ NN', 'VBG NN'] + [
    'NN NN', 'NN NN NN']
PROPERTY_TAG_DICT = {}
with open('../files/FULL_PROPERTY_TAG_DICT') as f:
    _FULL_PROPERTY_TAG_DICT = json.loads(f.read())
    f.close()


def join_tags(tags_words):
    tmp = {}
    if len(tags_words) == 2 or len(tags_words) == 3:
        for i in range(len(tags_words)):
            tmp.update(tags_words[i])
        return tmp


def get_pos_tags(p_label):
    if p_label in _FULL_PROPERTY_TAG_DICT:
        return _FULL_PROPERTY_TAG_DICT[p_label]
    else:
        stop_tags = ['DT']
        text = nltk.word_tokenize(p_label)
        # text = [ ps.stem(w.lower()) for w in text]
        word_tag_pairs = nltk.pos_tag(text)
        tags = [pair[1] for pair in word_tag_pairs if pair[1] not in stop_tags]
        tags_words = [{pair[1]: pair[0].strip()} for pair in word_tag_pairs if pair[1] not in stop_tags]
        tag_chain = ' '.join(tags)
        tag_dict = join_tags(tags_words)
        PROPERTY_TAG_DICT[p_label] = (tag_chain, tag_dict)
        return tag_chain, tag_dict
    # if tag_chain.endswith(' IN'):
    #     # if it starts with VB, then it is a different story
    #     if tag_chain == 'VBN IN' or tag_chain == 'NN IN':
    #         # split the two element attribute into 'VBN': created, 'IN': in, 'VB':
    #         tag_dict = join_tags(tags_words)
    #         return tag_chain, tag_dict
    #     else
    #
    # else:
    #     return tag_chain, {}


def generate_simple_questions(tag_chain, property_tags_dict, i, p):
    questions = []
    labelled_p = '[%s](attribute)' % (p.strip())
    if 'class' in i:
        of = random.choice(['of all', 'of', 'all', ''])
    else:
        of = random.choice(['of', ''])

    # if 'treat' in p:
    #     print('----------')
    #     print(p)
    #     print(tag_chain)

    if tag_chain.endswith(' IN'):
        if tag_chain == 'VBN IN':  # e.g. used for, developed from
            IN = property_tags_dict['IN']
            VBN = property_tags_dict['VBN']
            q = ' [%s](attribute) ' % IN + ' what is ' + i + ' ' + ' [%s](attribute) ' % VBN
            q2 = 'what is ' + i + ' [%s %s](attribute)' % (VBN, IN)
            questions = questions + [q, q2]
        elif tag_chain == 'NN IN':  # standard attribute 'mass of', 'density of'
            NN = property_tags_dict['NN']
            IN = property_tags_dict['IN']
            q = 'what is %s %s [%s %s](attribute)' % (i, random.choice(DTs), NN, IN)
            # q = 'what is %s ' % i + ' ' + random.choice(DTs) + ' [%s %s](attribute)' % (NN, IN)
            # q2 = i + ' is ' + random.choice(DTs) + '[%s %s](attribute)' % (NN, IN) + ' what'
            q2 = '%s is %s [%s %s](attribute) what' % (i, random.choice(DTs), NN, IN)
            questions = questions + [q, q2]

        elif tag_chain == 'VBZ NN IN':  # 'is a class of', 'is an example of '
            NN = property_tags_dict['NN']
            IN = property_tags_dict['IN']
            p_label = '[%s %s](attribute)' % (NN, IN)
            # xxx is a type of what
            # what is xxx a type of
            q = ' %s is %s %s what' % (i, random.choice(DTs), p_label)
            q2 = 'what is %s %s %s' % (i, random.choice(DTs), p_label)
            questions = questions + [q, q2]

        elif tag_chain == 'JJ IN':
            JJ = property_tags_dict['JJ']
            IN = property_tags_dict['IN']
            p_label = '[%s %s](attribute)' % (JJ, IN)
            q = '%s is %s %s' % (random.choice(heads), i, p_label)
            q2 = '%s is %s what' % (i, p_label)
            questions = questions + [q, q2]

        elif tag_chain == 'NNS IN':
            NNS = property_tags_dict['NNS']

            IN = property_tags_dict['IN']
            p_label = '[%s %s](attribute)' % (NNS, IN)
            q = '%s does %s %s' % (random.choice(heads), i, p_label)
            q2 = '%s %s what' % (i, p_label)
            questions = questions + [q, q2]

        # elif tag_chain in ['VBN IN IN', 'JJ NN IN']:
        #     q = 'what is %s %s' % (i, labelled_p)
        #     questions.append(q)

        elif tag_chain.startswith('VBZ'):
            p_label = '[%s](attribute)' % (p.replace('is', '').strip())
            q = 'what is %s %s' % (i, p_label)
            questions.append(q)

        else:  # handle all other properties that end with IN
            q = 'what is %s %s' % (i, labelled_p)
            questions.append(q)

    elif tag_chain in simple_properties:  # standard attribute 'molecule weight'
        q = '%s %s %s %s' % (random.choice(heads), labelled_p, of, i)
        q2 = '%s %s quote %s' % (random.choice(heads), i, labelled_p)
        questions = questions + [q, q2]

    elif tag_chain == ['NN IN NN', 'VBN']:
        wh = random.choice(whs)
        q = '%s is %s %s %s %s' % (wh, random.choice(the), labelled_p, of, i)
        questions.append(q)

    elif tag_chain == 'NNS':
        sp = singularize(p)
        labelled_p = '[%s](attribute)' % random.choice([sp, p])
        q = 'what does %s %s' % (i, labelled_p)
        q2 = '%s %s what' % (i, labelled_p)
        questions = questions + [q, q2]

    elif tag_chain == 'NN':
        pp = pluralize(p)
        labelled_p = '[%s](attribute)' % random.choice([pp, p])
        q = 'what does %s %s' % (i, labelled_p)
        q2 = '%s %s what' % (i, labelled_p)
        questions = questions + [q, q2]

    elif tag_chain == 'NNS VBP':  # treats disease
        NNS = property_tags_dict['NNS']  # treats
        VBP = property_tags_dict['VBP']  # disease
        VBP = random.choice([VBP, pluralize(VBP)])
        NNS = random.choice([NNS, singularize(NNS)])
        # what disease (VBP) does xxx treat (NNS)
        q = 'what [%s](attribute) does %s [%s](attribute)' % (VBP, i, NNS)
        # xxx treat(s)(NNS) what disease (VBP)
        q2 = '%s [%s](attribute) what [%s](attribute)' % (i, NNS, VBP)
        q3 = 'what is the [%s](attribute) of %s' % (labelled_p, i)
        questions = questions + [q, q2, q3]

    elif tag_chain == 'NNS JJ NN':  # treats medical condition
        NNS = property_tags_dict['NNS']  # treats
        NNS = random.choice([NNS, singularize(NNS)])  # treats/treat
        JJ = property_tags_dict['JJ']  # medical
        NN = property_tags_dict['NN']  # condition
        NN = random.choice([NN, pluralize(NN)])

        # what medical condition does xx treat(s)
        q = 'what [%s %s](attribute) does %s [%s](attribute)' % (JJ, NN, i, NNS)
        q2 = '%s [%s](attribute) what [%s %s](attribute)' % (i, NNS, JJ, NN)
        q3 = 'what is the [%s](attribute) of %s' % (labelled_p, i)
        questions = questions + [q, q2, q3]
        # if 'condition' in NN:
        #     print('GOT NN')
        #     print('pluralized', pluralize(NN))
        #     print(questions)



    elif tag_chain == 'VBZ NN':
        VBZ = property_tags_dict['VBZ']
        NN = property_tags_dict['NN']
        if 'has' in p:
            verb = 'have'
            q = 'what [%s](attribute) does %s %s' % (NN, i, verb)

        else:
            verb = VBZ[:-1]
            q = 'what [%s](attribute) does %s [%s](attribute)' % (NN, i, verb)
        # 'show the role of xxx '
        q2 = '%s %s [%s](attribute) %s %s' % (random.choice(heads), random.choice(the), NN, of, i)
        questions = questions + [q, q2]
    elif tag_chain == 'VBN IN NN':
        VBN = property_tags_dict['VBN']
        IN = property_tags_dict['IN']
        NN = property_tags_dict['NN']
        q = '%s what [%s](attribute) is %s [%s](attribute)' % (IN, NN, i, VBN)
        q2 = '%s is [%s](attribute) %s what [%s](attribute)' % (i, VBN, IN, NN)
        questions = questions + [q, q2]
    elif tag_chain in ['VBZ', 'NNS TO']:
        q = 'what does %s %s' % (i, labelled_p)
        questions.append(q)
        # IN what NN is I VBN
        # I is VBN IN what NN

    elif tag_chain in ['RB JJ TO NN CC NN', 'NN IN JJ', 'NN IN NN NN', 'NN NN CC NN', 'NN POS JJ NN', 'NNP NN NN',
                       'NNP NNP NNP NNP NNP NNP']:
        q = '%s %s %s %s' % (random.choice(heads), labelled_p, of, i)
        questions.append(q)
    elif tag_chain in ['VBZ NNS', 'VBN NN', 'VBZ NN NN', 'VBZ NNS']:
        labelled_p = labelled_p.replace('has ', '')
        q = 'what %s does %s [have](attribute)' % (labelled_p, i)  # what xxx does xxx has
        questions.append(q)

    elif tag_chain in ['JJ NN VBD', 'NN VBD']:
        be_word = random.choice(['does', 'can', 'do'])
        VBD = property_tags_dict['VBD']
        if 'JJ' in property_tags_dict:
            if VBD.endswith('ed'):
                VBD = random.choice([VBD, VBD.replace('ed', '')])
            JJ = property_tags_dict['JJ']
            NN = property_tags_dict['NN']
            p1 = '[%s %s](attribute)' % (JJ, NN)
            p2 = '[%s](attribute)' % VBD
            q = 'what %s %s %s %s' % (p1, be_word, i, p2)
            questions.append(q)
        else:
            NN = property_tags_dict['NN']
            p1 = '[%s](attribute)' % NN
            p2 = '[%s](attribute)' % VBD
            q = 'what %s %s %s %s' % (p1, be_word, i, p2)
            questions.append(q)

    else:
        if tag_chain in skip_tags:
            pass
        else:
            q = '%s %s %s %s' % (random.choice(heads), labelled_p, of, i)
            q2 = '%s %s quote %s' % (random.choice(heads), i, labelled_p)
            questions = questions + [q, q2]

    return questions, PROPERTY_TAG_DICT
