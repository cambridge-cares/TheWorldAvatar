import re
def extract_numbers_and_units(text, pattern_type, multiplier_flag=2):
    """
    Extracts numbers and units based on pattern type ("add" or "temp").
    If "add" is matched, it also processes the range pattern (number1:number2).
    If "temp" is matched, range processing is skipped.

    Parameters:
        - text: The input text containing numbers and units.
        - pattern_type: "add" for regular units, "temp" for temperature or rates.
        - multiplier_flag: 0 to multiply by number1 from range, 1 to multiply by number2.
    """
    
    # Define patterns
    patterns = {
        'add': r'(\d*\.?\d+)\s*([a-zA-Z]+)',   # Pattern for numbers with units
        'temp': r'(\d*\.?\d+)\s*([^\d\s]+)',   # Pattern for temperature or rates
        'range': r'\((\d+):(\d+)\)'            # Pattern for (number1:number2)
    }

    # Determine the pattern based on pattern_type
    if pattern_type == "add":
        pattern = patterns['add']
    elif pattern_type == "temp":
        pattern = patterns['temp']
    else:
        return [0], ["N/A"], [1]  # Return default values if no valid pattern_type

    # Find all matches based on the selected pattern
    matches = re.findall(pattern, text)
    
    # Separate numbers and units
    numbers = [float(match[0]) for match in matches]
    units = [match[1] for match in matches]
    
    if not numbers:
        numbers = [0] 
    if not units:
        units = ["N/A"] 
    
    # If the pattern is "add", we process the range pattern
    if pattern_type == "add" and matches:
        # Find 'range' pattern matches (number1:number2)
        range_matches = re.findall(patterns['range'], text)
        
        if range_matches:
            print("range matched: ", range_matches)
            # If 'range' pattern is found, extract number1 and number2
            number1_list = [int(match[0]) for match in range_matches]
            number2_list = [int(match[1]) for match in range_matches]
            
            # Calculate the sum of number1 and number2 for each match
            divisors = [n1 + n2 for n1, n2 in zip(number1_list, number2_list)]
        else:
            # Default divisor if no 'range' is found but 'add' is present
            divisors = [1] * len(numbers)
            number1_list, number2_list = [1] * len(numbers), [1] * len(numbers)
    
        # Now divide each number by the corresponding sum of range values
        divisor = divisors[0]  # Using the first divisor for all numbers for simplicity
        numbers_divided = [num / divisor for num in numbers]
    
        # Multiply the result of division by number1 or number2 depending on multiplier_flag
        if multiplier_flag == 0:
            # Multiply by number1
            numbers_final = [num_div * number1_list[0] for num_div in numbers_divided]
        else:
            # Multiply by number2
            numbers_final = [num_div * number2_list[0] for num_div in numbers_divided]
    
    # If the pattern is "temp", skip range processing
    elif pattern_type == "temp":
        # Return numbers directly without dividing or multiplying by range values
        numbers_final = numbers
        divisors = [1] * len(numbers)
    else: 
        numbers_final = numbers
    
    return numbers_final, units
