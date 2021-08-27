import json

def add_to_class(name, iri):
    with open('wiki_dictionary_new') as f:
        wiki_dict = json.loads(f.read())
        class_list = wiki_dict['class']['list']

        if name not in class_list:
            # add the name
            class_list.append(name)
            # attr_dict.append({name: [str(iri)]})

            wiki_dict['class']['dict'][name] = [str(iri)]
            wiki_dict['class']['list'] = class_list

    with open('wiki_dictionary_new', 'w') as f:
        f.write(json.dumps(wiki_dict, indent=4))
        f.close()


def add_to_attr(name, iri):
    with open('wiki_dictionary_new') as f:
        wiki_dict = json.loads(f.read())

        # class_dict = wiki_dict['class']['dict']
        # class_list = wiki_dict['class']['list']

        # print(class_dict)
        # print(class_list)

        attr_dict = wiki_dict['attribute']['dict']
        attr_list = wiki_dict['attribute']['list']

        if name not in attr_list:
            # add the name
            attr_list.append(name)
            # attr_dict.append({name: [str(iri)]})

            wiki_dict['attribute']['dict'][name] = [str(iri)]
            wiki_dict['attribute']['list'] = attr_list

    with open('wiki_dictionary_new', 'w') as f:
        f.write(json.dumps(wiki_dict, indent=4))
        f.close()


# add_to_attr("antiparticle", "http://www.wikidata.org/entity/P2152")
# add_to_class("fuel gas", "http://www.wikidata.org/entity/Q909363")
add_to_class("fossil fuel", "http://www.wikidata.org/entity/Q12748")