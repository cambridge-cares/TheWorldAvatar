import PyPDF2
import re
import pandas as pd
import sys
import os
from nltk.corpus import words

# Download the words corpus
import nltk
nltk.download('words')

# List of valid English words
valid_words = set(words.words())

# Function to check if a word is valid
def is_valid_word(word):
    return word in valid_words

# Function to extract text from the PDF
def extract_text_from_pdf(pdf_path):
    with open(pdf_path, 'rb') as file:
        pdf_reader = PyPDF2.PdfReader(file)
        text = ""
        for page_num in range(len(pdf_reader.pages)):
            page = pdf_reader.pages[page_num]
            page_text = page.extract_text()
            # Remove common header and footer patterns (customise these patterns as needed)
            page_text = remove_headers_footers(page_text)
            text += page_text
    
    # Clean the extracted text
    # cleaned_text = clean_extracted_text(text)
    return text

# Function to remove headers and footers
def remove_headers_footers(page_text):
    # Example patterns to remove (customise as needed)
    header_footer_patterns = [
        r'POINTS OF INTEREST –  CLASSIFICATION SCHEME',  # header
        r'POINTS OF INTEREST – CLASSIFICATION SCHEME',  # header
        r'December 2022',  # header
        r'OFFICIAL', # footer
        r'© Or dnance Survey Ltd 2022', # footer
        r'Page \d+ of \d+',  # page numbers
    ]
    
    for pattern in header_footer_patterns:
        page_text = re.sub(pattern, '', page_text, flags=re.IGNORECASE)
    
    cleaned_lines = page_text.split('\n')
    return '\n'.join(cleaned_lines)

# Function to remove digits followed by one or more spaces
def remove_digits_followed_by_spaces(text):
    text = text.strip()
    # Use regular expression to remove digits followed by one or more spaces
    return re.sub(r'\d+\s+', '', text)

# Function to extract digits from a string and return as an integer
def extract_digits(text):
    # Use regular expression to find all digits in the string
    digits = re.findall(r'\d+(?=\s+)', text)
    
    # Join the digits into a single string and convert to integer
    if digits:
        return ''.join(digits)
    else:
        return None  # In case no digits are found

def is_category(line):
    # Check if the line matches the pattern
    if re.match(r'^\d{2} ', line):
        return True
    else:
        return False

# Function to clean extracted text
def clean_extracted_text(text):
    # Refine regex to avoid merging valid separate words like 'Eating and'
    def smart_merge(match):
        first_word, second_word = match.groups()
        merged_word = first_word + second_word

        # Only merge if the merged word is valid and both words aren't valid individually
        if is_valid_word(merged_word) and not (is_valid_word(first_word) and is_valid_word(second_word)):
            return merged_word
        else:
            return match.group(0)  # Return original match if merge isn't appropriate

    # Apply smart merging to the text using the updated smart_merge function
    text = re.sub(r'(\w+)\s+(\w+)', smart_merge, text)

    # Merge separated single characters except 'a'
    text = re.sub(r'\b([b-zB-Z])\s+(\w+)\b', r'\1\2', text)

    # Remove spaces before hyphens in compound words
    text = re.sub(r'(\w)\s+-(\w)', r'\1-\2', text)
    
    # Remove spaces between a prefix and a hyphen (e.g., "pre -" -> "pre-")
    text = re.sub(r'(\b\w+)\s+-', r'\1-', text)

    return text

# Function to clean extracted class
def clean_ocr_errors(text):
    # Correct common OCR errors
    corrections = {
        'Plastere rs': 'Plasterers',
        'Electri cal': 'Electrical',
        'Aircra ft': 'Aircraft',
        'Conce rt/': 'Concert or ',
        'publ ishers': 'publishers',
        '  ': ' ',
        'pal mists': 'palmists',
        'Servi ces': 'Services',
        'servi ces': 'services',
        'clot hing': 'clothing',
        'Tes ting': 'Testing',
        'hi re': 'hire',
        'Aquari a': 'Aquaria',
        'Farm- based': 'Farm-based',
        'obse rvatories': 'observatories',
        'lochan s': 'lochans',
        'Children\'sactivity': 'Children\'s activity',
        'children’sclothes': 'children\'s clothes',
        'Ho spices': 'Hospices',
        'surge ries': 'surgeries',
        'Coast al': 'Coastal',
        'organ isations': 'organisations',
        'Or emining': 'Ore mining',
        'Mixe dor': 'Mixed or',
        'supplie s': 'supplies',
        'telecomm unications': 'telecommunications',
        'Stati onery': 'Stationery',
        'Stat ionery': 'Stationery',
        'inclu ding': 'including',
        'st ores': 'stores'
    }

    for error, correction in corrections.items():
        text = text.replace(error, correction)
    return text

# Improved function to extract categories and classes
def extract_categories_classes(text):
    # Split text into lines
    lines = text.split('\n')

    category_dict = {}
    current_category = None
    current_class = None
    previous_line = None
    top_level_category = None

    for line in lines:
        if line.strip() == "":
            continue
        line = line.strip()
        # Check if the previous line is a top level category
        if is_category(str(line)) and is_category(str(previous_line)):
            if top_level_category:
                category_dict[top_level_category].remove(previous_line)
            top_level_category = previous_line
            current_category = line
            category_dict[top_level_category].append(current_category)
            category_dict[line] = []
            current_class = None
        # Check if the line matches a category pattern
        elif re.match(r'^\d{2} ', line):
            current_category = line
            if current_category in category_dict:
                continue
            category_dict[current_category] = []
            current_class = None
            if top_level_category:
                category_dict[top_level_category].append(current_category)
        # Check if the line matches a class pattern
        elif re.match(r'^\d{4} ', line):
            current_class = line
            if current_category:
                current_class = re.sub(r'\s+\d{2}\s+[A-Z].*$', '', current_class)
                parts = re.split(r'(?<!^)(?=\d{4} )', current_class)
                for part in parts:
                    current_class = part.strip()
                    category_dict[current_category].append(current_class)
        # Handle continuation lines (for split categories or classes)
        else:
            if current_class:
                parts = re.split(r'(?=\d{4} )', line)
                count = 0
                part_size = len(parts)
                for part in parts:
                    count += 1
                    current_class += ' ' + part.strip()
                    if count <= 1:
                        # Update the last class in the list
                        category_dict[current_category][-1] = current_class
                    else:
                        category_dict[current_category].append(current_class)
                    if count < part_size:
                        current_class = ''
            elif current_category:
                current_category += ' ' + line.strip()
                # Update the category in the dict
                category_dict[current_category] = category_dict.pop(list(category_dict.keys())[-1])
        previous_line = line
    return category_dict

def is_pdf_file(pdf_path):
    # Check if the file has a .pdf extension
    if not pdf_path.lower().endswith('.pdf'):
        return False
    
    # Check if the file exists and is a file
    if not os.path.isfile(pdf_path):
        return False
    
    # Check the file signature (magic number)
    with open(pdf_path, 'rb') as file:
        # Read the first few bytes
        file_signature = file.read(4)
        
        # Check if the file starts with the '%PDF-' signature
        if file_signature == b'%PDF':
            return True
        else:
            return False

def form_complete_code(cls, class_category_map, class_code_map):
    # Initialise a list to store the complete code
    complete_code = []
    
    # Start with the given class and include its code
    current_class = cls
    if current_class in class_code_map:
        complete_code.append(class_code_map[current_class])
    
    # Traverse the parent hierarchy until no more parents are found
    while current_class in class_category_map:
        parent = class_category_map[current_class]

        # Retrieve the code for the parent from class_code_map
        if parent in class_code_map:
            complete_code.append(class_code_map[parent])

        # Move to the parent for the next iteration
        current_class = parent
    
    # Reverse the order of codes to get them in the correct order (root code to child code)
    complete_code.reverse()

    # Combine the codes to form the complete concatenated code
    final_code = ''.join(complete_code)

    return final_code

def replace_comma_and_apostrophe(text):
    """
    Replaces each comma with " and" and removes occurrences of "'s" from the text.

    Parameters:
    text (str): The input string to be modified.

    Returns:
    str: The modified string with commas replaced by " and" and occurrences of "'s" removed.
    """
    # Remove occurrences of "'s"
    text = text.replace("'s", "")
    # Replace commas with " and"
    text = text.replace(",", " and")
    return text

def extract_and_write_classes_in_tbox_csv_template(input_pdf_path, output_csv_path):
    """
    If you are calling from another Python script, call this function.
    
    Extracts class and category information from a PDF, processes it to generate an ontology-based structure,
    and writes the resulting data to CSV files.

    Parameters:
    input_pdf_path (str): The path to the input PDF file containing class information.
    output_csv_path (str): The path where the main extracted classes and categories will be saved in CSV format.

    Returns:
    None: This function does not return a value but generates two CSV files:
          1. A CSV with extracted classes and categories based on ontology structure.
          2. A CSV containing class-to-complete code mappings for 8-digit codes derived from the classes.

    """
    # Check if the file exists and is a file
    if not os.path.isfile(input_pdf_path):
        raise FileNotFoundError(f"The file '{input_pdf_path}' does not exist or is not a valid file path.")
    if not is_pdf_file(input_pdf_path):
        raise FileNotFoundError(f"The file '{input_pdf_path}' is not a valid PDF file.")
    # Extract text from the provided PDF
    pdf_path = input_pdf_path
    pdf_text = extract_text_from_pdf(pdf_path)

    # Extract categories and classes
    category_dict = extract_categories_classes(pdf_text)

    # Convert the dictionary to a DataFrame for better visualization
    data = []
    
    # Pre-defined ontology entries
    ontology_entries = [
        ["ontopoi", "TBox", "https://www.theworldavatar.com/kg/ontopoi/", "https://www.w3.org/2007/05/powder-s#hasIRI", "", "", "", "", "", ""],
        ["ontopoi", "TBox", 1, "http://www.w3.org/2002/07/owl#versionInfo", "", "", "", "", "", ""],
        ["ontopoi", "TBox", "OntoPOI is an ontology developed for representing points of interest.", "http://www.w3.org/2000/01/rdf-schema#comment", "", "", "", "", "", ""],
        ["ontopoi", "TBox", "", "http://www.w3.org/2002/07/owl#imports", "", "", "", "", "", ""]
    ]

    # Add pre-defined ontology entries to the data list
    data.extend(ontology_entries)

    class_code_map = {}
    class_category_map = {}
    for category, classes in category_dict.items():
        for cls in classes:
            cls = clean_extracted_text(cls)
            cls_code = extract_digits(cls)
            cls = remove_digits_followed_by_spaces(cls)
            cls = clean_ocr_errors(cls)
            category = clean_extracted_text(category)
            category_code = extract_digits (category)
            category = remove_digits_followed_by_spaces(category)
            category = clean_ocr_errors(category)
            comment = cls + " is a subcategory of " + category
            comment = comment.capitalize()
            data.append([
                replace_comma_and_apostrophe(cls).title(),     # Source
                "Class",      # Type
                replace_comma_and_apostrophe(category).title(),          # Target
                "IS-A",       # Relation
                "",           # Domain (empty)
                "",           # Range (empty)
                "",           # Quantifier (empty)
                comment, # Comment
                "https://www.theworldavatar.com/kg/ontopoi",           # Defined By (empty)
                cls            # Label (empty)
            ])
            if cls_code is not None:
                class_code_map[cls] = cls_code
            if category_code is not None:
                class_code_map[category] = category_code
            class_category_map[cls] = category
    
    class_complete_code_map = {}
    
    # Create the class versus complete code map
    for cls in class_category_map:
        # Form the complete code for the current class
        complete_code = form_complete_code(cls, class_category_map, class_code_map)
        
        # Check if the complete code is exactly 8 digits long
        if len(complete_code) == 8:
            # Strip the leading zeros from the complete code
            complete_code = complete_code.lstrip('0')
            
            # Store the result in the class_complete_code_map
            class_complete_code_map[cls] = complete_code

    columns = ["Source", "Type", "Target", "Relation", "Domain", "Range", "Quantifier", "Comment", "Defined By", "Label"]
    df = pd.DataFrame(data, columns=columns)
    # Display the DataFrame
    print(df)

    # Save the DataFrame to a CSV file
    df.to_csv(output_csv_path, index=False)

    # Convert the class_complete_code_map into a DataFrame
    df = pd.DataFrame(list(class_complete_code_map.items()), columns=["Class", "Code"])

    # Save the DataFrame to a CSV file
    df.to_csv('class_complete_code_map.csv', index=False)

    print("class_code_map saved to 'class_complete_code_map.csv'")

# Main execution block
if __name__ == "__main__":
    """
    Script entry point for extracting classes from a PDF and saving them to a CSV file.

    Usage:
        python class_extractor.py <input_pdf_path> <output_csv_path>
    
    This script expects exactly two command-line arguments:
    1. input_pdf_path (str): Path to the PDF file containing class definitions.
    2. output_csv_path (str): Path where the extracted classes will be saved in CSV format.

    Example:
        python class_extractor.py my_classes.pdf output_classes.csv
    """
    # Ensure correct number of command-line arguments
    if len(sys.argv) != 3:
        print("Usage: python class_extractor.py <input_pdf_path> <output_csv_path>")
    else:
        # Assign arguments to variables
        input_pdf_path = sys.argv[1]
        output_csv_path = sys.argv[2]
        
        # Call function to perform extraction and save results
        extract_and_write_classes_in_tbox_csv_template(input_pdf_path, output_csv_path)