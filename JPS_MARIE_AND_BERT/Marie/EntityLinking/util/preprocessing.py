import re
from nltk import word_tokenize


def expand_abbreviation(text, ab3p_abbr_dict={}, additional_abbr_dict={}):
    '''
    to expand abbreviations in a text
    :param text:
    :param ab3p_abbr_dict: abbreviations fund by Ab3p tool, this dictionary is more reliable
    :param additional_abbr_dict: pre-defined abbreviations stored in 'input/abbreviation.dict'.
    Be cautious to use it because this method can cause errors.
    :return:
    '''
    expanded_text = text
    for k, v in ab3p_abbr_dict.items():
        expanded_text = expanded_text.replace(k, v) if k in text else text

    # Note that this step can cause errors
    expanded_text = word_tokenize(expanded_text)

    for k, v in additional_abbr_dict.items():
        for index, token in enumerate(expanded_text):
            if k == token:
                expanded_text[index] = v
    expanded_text = ' '.join(expanded_text)
    return expanded_text


def remove_punctuation(text):
    '''
    :param text: a natural word sequence (a single word is acceptable)
    :return: a new sentence after removing punctuations
    '''

    # feel free to add new punctuations if needed
    punctuations = '[’!"#$%&\'()*+,-./:;<=>?@，。?★、…【】《》？“”‘’！[\\]^_`{|}~]+'
    new_sentence = re.sub(punctuations, ' ', text)
    new_sentence = ' '.join(new_sentence.split())
    return new_sentence


def load_number_mapping(number_mapping_file):
    '''
    read different types of numeral values
    :param number_mapping_file: mapping file
    :return: a dict that stores the mapping relationships
    '''
    number_mappings = {}
    for line in open(number_mapping_file, encoding='utf8'):
        row = line.strip('\n').split('@')
        cardinal_num = row[0]
        number_mappings[cardinal_num] = []
        number_mappings[cardinal_num].append(cardinal_num)
        for diff_type_nums in row[1].split('|'):
            diff_type_nums = diff_type_nums.split('__')
            for num in diff_type_nums:
                num = str.lower(num)
                number_mappings[cardinal_num].append(num)
        if len(row) > 2:
            number_mappings[cardinal_num].append(row[2])
    return number_mappings


def get_number(text):
    '''
    extract arabic numbers from a natural text
    :param text:
    :return:
    '''
    result = re.findall(r"\d+\.?\d*", text)
    return result


def replace_arabic_numeral(text, number_mappings):
    '''
    find arabic numerals and replace by spelt-out english words
    :param text:
    :return:
    '''
    numbers = get_number(text)

    if len(numbers) == 0:return text

    def get_mapping(x):
        for k, v in number_mappings.items():
            if x in v:
                return k
        return None

    for num in numbers:
        mapping = get_mapping(num)
        if not mapping:continue
        text = text.replace(num, ' ' + mapping + ' ')

    text = ' '.join(text.split())

    return text


def replace_numeral(text, number_dict):
    '''
    find arabicm, roman, ordinal numerals and replace them
    :param text: must be lower words
    :return:
    '''
    text = str.lower(text)
    tokens = word_tokenize(text)
    number_tokens = tokens[:]
    for k, numbers in number_dict.items():
        for num in numbers:
            if num in number_tokens:
                index = number_tokens.index(num)
                number_tokens[index] = k

    replaced_number_tokens = []
    for token in number_tokens:
        rep_token = replace_arabic_numeral(token, number_dict)
        replaced_number_tokens.append(rep_token)
    return ' '.join(replaced_number_tokens).strip()


def _preprocess(text, ab3p_abbr_dict={}, additional_abbr_dict={}, number_dict={}):
    '''
    the preprocessing step described in our paper
    :param text:
    :param ab3p_abbr_dict:
    :param additional_abbr_dict:
    :return:
    '''

    # Abbreviation Expansion
    expanded_text = expand_abbreviation(text, ab3p_abbr_dict, additional_abbr_dict)

    # Puctuation Removal
    punc_text = remove_punctuation(expanded_text)

    # Numeral Replacement
    numeral_text = replace_numeral(punc_text, number_dict)

    return numeral_text


class Preprocessor():
    def __init__(self, ab3p_dict ="./abbrmap/abbreviation.dict", num_mapping = './abbrmap/number_mapping.txt'):
        self.ab3p_dict = self.load_abbr_dict(ab3p_dict)
        self.number_mapping =  load_number_mapping(num_mapping)

    def load_abbr_dict(self, dict_path):
        with open(dict_path,'r') as f:
            lines = f.readlines()
            abbr_dict = {}
            for line in lines:
                k, v = line.strip().split('||')
                abbr_dict[k] = v
            return abbr_dict


    def preprocess(self, text):
        return _preprocess(text, self.ab3p_dict, {}, self.number_mapping)
