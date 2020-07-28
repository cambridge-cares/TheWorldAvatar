import json, re, time, random
import nltk
from nltk.stem import PorterStemmer
ps = PorterStemmer()

size_of_trainning = 5000

labels = json.loads(open('corpus_for_trainning').read())
c_labels = labels[0]
i_labels = labels[1]
p_labels = labels[2]
 
# generate different types of questions ... 

# batch_restriction_query e.g. give me the [flash point](attribute) of all the [fossil fuels](class)
# item_attribute_query e.g. what is the freezing point of benzene
question_pool = ['what is', 'show me the', 'show the', 'list the', 'whats the', 'what\'s the', '', 'what are the', 'find the', 'the']


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
    WH_word = random.choices(pool_1, weights = (5, 70, 10 , 5, 5, 5), k = 1)[0] + ' '
    print('------------------')
    #if 'VBN' in tags or 'V' in tags: # e.g. what is xxx used for 
    if tag_chain == 'VBN IN' or tag_chain ==  'VBN IN IN':
        temp = WH_word + random.choices(['can [%s](%s) be [%s](attribute)', 'is [%s](%s) [%s](attribute)', 'are [%s](%s) [%s](attribute)'], weights = (10, 45, 45), k = 1)[0] 
        print(text)
        print(tag_chain)
        question_template = temp % (c_or_i_label, type, p_label.strip())
        print(question_template)
    elif tag_chain == 'VBN' or  tag_chain == 'VBN IN NN IN': # e.g. coined, invented, awarded , VBN IN NN IN named in honor of
        temp = WH_word + random.choices(['can [%s](%s) be [%s](attribute)', 'is [%s](%s) [%s](attribute)', 'are [%s](%s) [%s](attribute)',],weights = (10, 45, 45), k = 1 )[0]
        print(text)
        print(tag_chain)
        question_template = temp % (c_or_i_label, type, p_label.strip())
        print(question_template)                
    else: # the general_case, we category anything else into this question type, what is the a(attribute) of b (class/instance) 
        print(text)
        print(tag_chain)
        WH_word = random.choices(pool_2, weights = (12.5,12.5,12.5,12.5,12.5,12.5,12.5,12.5,), k = 1)[0] + ' ' + random.choices(['the', ''], weights = (90,10))[0] + ' '
        question_template = WH_word + random.choices(['[%s](attribute) of [%s](%s)'%(p_label.strip(), c_or_i_label, type), '[%s](%s)\'s [%s](attribute)'% (c_or_i_label, type, p_label.strip())], weights = (80,20), k = 1)[0]
        print(question_template)
         
    # if tag_chain in structure_dictionary:
        # structure_dictionary[tag_chain].append(question_template)
    # else:
        # structure_dictionary[tag_chain] = [question_template]
    # with open('question_set', 'w') as f:
        # f.write(json.dumps(structure_dictionary))
        # f.close()
        # 
        # 
            
    return question_template        

def generate_type_2_questions():
    type_2_questions = []
    # print(random.choice(i_labels))
    # print(random.choice(p_labels))
    # print(random.choice(question_pool))
    # ## intent: item_attribute_query
    # - what is the [image](attribute) of [H2O2](entity)
    for i in range(0, size_of_trainning):
        p_label = random.choice(p_labels)
        i_label = random.choice(i_labels)
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
     
    for i in range(0, size_of_trainning):
        p_label = random.choice(p_labels)
        c_label = random.choice(c_labels)
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
     

def merge_files():
    # Reading data from file1 
    with open('type_1_questions') as fp: 
        data = fp.read() 
      
    # Reading data from file2 
    with open('type_2_questions') as fp: 
        data2 = fp.read() 
      
    # Merging 2 files 
    # To add the data of file2 
    # from next line 
    data += "\n"
    data += data2 
      
    with open ('nlu.md', 'w') as fp: 
        fp.write(data) 

generate_type_1_questions()
generate_type_2_questions()
merge_files()