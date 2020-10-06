import nltk



class toolbox:

    @staticmethod
    def remove_qi(tree):
        for sub_tree in tree:
            if toolbox.is_tree(sub_tree):
                if sub_tree.label() == 'QI':
                    tree.remove(sub_tree)

        return tree

    @staticmethod
    def np_processor(np):
        temp = []
        funcs = []
        for word in np.leaves():
            if not word[1].startswith('FUNC_'):
                temp.append(word[0])
            else:
                funcs.append(word[1])
        return {'term': ' '.join(temp).strip(), 'funcs': funcs}

    @staticmethod
    def update_element_within_tree(old_element, new_element, bigram):
        for i, element in enumerate(bigram):
            if element == old_element:
                bigram[i] = new_element
        return bigram

    @staticmethod
    def get_the_only_sub_tree(tree):
        if tree.subtrees():
            if len([subtree for subtree in tree.subtrees()]) > 1:
                return [subtree for subtree in tree.subtrees()][1]
            else:
                return tree

        else:
            return tree
    @staticmethod
    def is_tree(element):
        return type(element) == nltk.Tree

    @staticmethod
    def lower_strip(string):
        return string.lower().strip()

    @staticmethod
    def parse(grammar, tree):
        """Return a parsed tree basing on the given grammar"""
        cp = nltk.RegexpParser(grammar)
        return cp.parse(tree)


    @ staticmethod
    def separate_noun_in_tree(tree):
        separate_noun_grammar = '''NP: {<NN|NNS|NNP>+}
                                   FUNC_TARGET: {<WP|WDT>}
                                   FUNC_CLASS:{<ALL>}'''
        if tree.label() == 'NP':
            tree = toolbox.get_the_only_sub_tree(tree)
        return toolbox.parse(separate_noun_grammar, tree.leaves())

    @staticmethod
    def question_identifier(tree):
        if toolbox.is_tree(tree):
            question_identifier_grammar = '''WP: {<WP|WDT|WRB>}'''
            result = toolbox.parse(question_identifier_grammar, tree)
            labels = [subtree.leaves()[0][0] for subtree in result if toolbox.is_tree(subtree)]
            is_question = (1 == len(labels))
        else:
            labels = [tree[0]]
            is_question = (tree[1] in ['WP','WRB','WDT'])

        expectation = ''
        if is_question:
            label = labels[0]
            if toolbox.lower_strip(label) == 'when':
                expectation = 'date'
            elif toolbox.lower_strip(label) in ['who','whom']:
                expectation = 'person'
            elif toolbox.lower_strip(label) == 'where':
                expectation = 'place'
            elif toolbox.lower_strip(label) == 'what':
                expectation = 'thing'
            elif toolbox.lower_strip(label) == '_value':
                expectation = 'value'

        return [is_question,expectation]

    @staticmethod
    def get_root_sub_and_search_for_uri(tree):
        root_np = toolbox.get_root_sub(tree)
        tree = toolbox.get_the_only_sub_tree(tree)
        tree.remove(root_np)
        tree = NLP_Engine.nin_pattern_recognizer(tree)
        np_result = toolbox.np_processor(root_np)
        term = np_result['term']
        funcs = np_result['funcs']
        print('funcs', funcs)
        if 'FUNC_ALL' in funcs:
            uri = LookUpService.get_sub_class_uri(term)
            print('uri', uri)
            type = 'class'
        else:
            uri = LookUpService.get_sub_instance_uri(term)
            print('uri', uri)
            type = 'instance'
        return {'uri': uri, 'type': type, 'tree': tree}

    @staticmethod
    def get_root_sub(tree):
        NPs = [sub_tree for sub_tree in toolbox.get_the_only_sub_tree(tree) if toolbox.is_tree(sub_tree)]
        if NPs:
            root_np = NPs.pop()
            return root_np
        else:
            return toolbox.np_processor(tree)

    @staticmethod
    def general_get_root_sub(tree):
        NPs = [sub_tree for sub_tree in tree if toolbox.is_tree(sub_tree)]
        if NPs:
            root_np = NPs.pop()
            return {'term': toolbox.np_processor(root_np), 'tree': root_np}
        else:
            return {'term': toolbox.np_processor(tree), 'tree': tree}