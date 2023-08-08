import random

def add_space_and_lower(string):
    new_string = ""
    for char in string:
        if char.isupper():
            new_string += " " + char.lower()
        else:
            new_string += char
    return new_string

def get_random_number(a, b, n):
    start_range = a
    end_range = b 
    random_numbers = []
    for _ in range(n):
        random_number = random.randint(start_range, end_range)
        random_numbers.append(random_number)
    return random_numbers