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

def is_category(line):
    # Check if the line matches the pattern
    if re.match(r'^\d{2} ', line):
        return True
    else:
        return False

# Function to clean extracted text
def clean_extracted_text(text):
    # Correct common OCR errors
    # corrections = {
    #     'dri nking': 'drinking',
    #     'service s': 'services',
    #     't oiletries':'toiletries',
    #     'Or e':'Ore',
    #     'Mixe d': 'Mixed',
    # }

    # for error, correction in corrections.items():
    #     text = text.replace(error, correction)

    # Refine regex to avoid merging valid separate words like 'Eating and'
    def smart_merge(match):
        first_word, second_word = match.groups()
        merged_word = first_word + second_word

        # Use specific corrections from the dictionary if they exist
        # if match.group(0) in corrections:
        #     return corrections[match.group(0)]

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

# If you are calling from another Python script, call this function
def write_extracted_classes_in_tbox_csv_template(input_pdf_path, output_csv_path):
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
    for category, classes in category_dict.items():
        for cls in classes:
            cls = clean_extracted_text(cls)
            cls = remove_digits_followed_by_spaces(cls)
            category = clean_extracted_text(category)
            category = remove_digits_followed_by_spaces(category)
            data.append([
                cls,     # Source
                "Class",      # Type
                category,          # Target
                "IS-A",       # Relation
                "",           # Domain (empty)
                "",           # Range (empty)
                "",           # Quantifier (empty)
                cls + " is a subcategory of " + category, # Comment
                "https://www.theworldavatar.com/kg/ontopoi",           # Defined By (empty)
                cls            # Label (empty)
            ])

    columns = ["Source", "Type", "Target", "Relation", "Domain", "Range", "Quantifier", "Comment", "Defined By", "Label"]
    df = pd.DataFrame(data, columns=columns)
    # Display the DataFrame
    print(df)

    # Save the DataFrame to a CSV file
    df.to_csv(output_csv_path, index=False)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python class_extractor.py <input_pdf_path> <output_csv_path>")
    else:
        input_pdf_path = sys.argv[1]
        output_csv_path = sys.argv[2]
        write_extracted_classes_in_tbox_csv_template(input_pdf_path, output_csv_path)
