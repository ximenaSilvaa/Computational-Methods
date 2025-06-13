# Authors: Ximena Silva Bárcena
# Description: This program performs lexical analysis on arithmetic expressions from a text file.

# Checks whether a token is a valid integer using a DFA
def is_integer(token):
    states = {'q0', 'q1', 'q2', 'q3'}
    alphabet = {'digit', 'sign'}
    transition_table = {
        'q0': {'digit': 'q1', 'sign': 'q2'},
        'q1': {'digit': 'q1', 'sign': 'q3'},
        'q2': {'digit': 'q1', 'sign': 'q3'},
        'q3': {'digit': 'q3', 'sign': 'q3'}
    }
    current_state = 'q0'
    final_states = {'q1'}
    for char in token:
        if char.isdigit():
            input_type = 'digit'
        elif char in '+-':
            input_type = 'sign'
        else:
            return False  
        current_state = transition_table[current_state].get(input_type, 'q3')

    return current_state in final_states

# Checks whether a token is a valid float using a DFA
def is_float(token):
    states = {'q0', 'q1', 'q2', 'q3', 'q4', 'q5'}
    alphabet = {'digit', 'sign', 'decimal_point'}
    transition_table = {
        'q0': {'digit': 'q1', 'sign': 'q2', 'decimal_point': 'q3' },
        'q1': {'digit': 'q1', 'sign': 'q5', 'decimal_point': 'q3' },
        'q2': {'digit': 'q1', 'sign': 'q5', 'decimal_point': 'q3' },
        'q3': {'digit': 'q4', 'sign': 'q5', 'decimal_point': 'q5' },
        'q4': {'digit': 'q4', 'sign': 'q5', 'decimal_point': 'q5' },
        'q5': {'digit': 'q5', 'sign': 'q5', 'decimal_point': 'q5' }
    }

    current_state = 'q0'
    final_states = {'q4'}
    for char in token:
        if char.isdigit():
            input_type = 'digit'
        elif char in '+-':
            input_type = 'sign'
        elif char == '.':
            input_type = 'decimal_point'
        else:
            return False  
        current_state = transition_table[current_state].get(input_type, 'q5')

    return current_state in final_states

# Checks whether a token is in scientific notation using a DFA
def is_scientific_notation(token):
    states = {'q0', 'q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7', 'q8'}
    alphabet = {'digit', 'sign', 'decimal_point', 'exponent'}
    transition_table = {
        'q0': {'digit': 'q1', 'sign': 'q2', 'decimal_point': 'q3', 'exponent': 'q5'},
        'q1': {'digit': 'q1', 'sign': 'q5', 'decimal_point': 'q3', 'exponent': 'q6'},
        'q2': {'digit': 'q1', 'sign': 'q5', 'decimal_point': 'q3', 'exponent': 'q5'},
        'q3': {'digit': 'q4', 'sign': 'q5', 'decimal_point': 'q5', 'exponent': 'q6'},
        'q4': {'digit': 'q4', 'sign': 'q5', 'decimal_point': 'q5', 'exponent': 'q6'},
        'q5': {'digit': 'q5', 'sign': 'q5', 'decimal_point': 'q5', 'exponent': 'q5'},
        'q6': {'digit': 'q8', 'sign': 'q7', 'decimal_point': 'q5', 'exponent': 'q5'},
        'q7': {'digit': 'q8', 'sign': 'q5', 'decimal_point': 'q5', 'exponent': 'q5'},
        'q8': {'digit': 'q8', 'sign': 'q5', 'decimal_point': 'q5', 'exponent': 'q5'}
    }

    current_state = 'q0'
    final_states = {'q8'}
    for char in token:
        if char.isdigit():
            input_type = 'digit'
        elif char in '+-':
            input_type = 'sign'
        elif char == '.':
            input_type = 'decimal_point'
        elif char == 'E':
            input_type = 'exponent'
        else:
            return False  
        current_state = transition_table[current_state].get(input_type, 'q5')

    return current_state in final_states

# Checks whether a token is an arithmetic operator using a DFA
def is_operator(token):
    alphabet = {'operator'}
    transition_table = {
        'q0': {'operator': 'q1'},
        'q1': {'operator': 'q2'},
        'q2': {'operator': 'q2'}
    }

    current_state = 'q0'
    final_states = {'q1'}
    for char in token:
        if char in '=+-*/^':
            input_type = 'operator'
        else:
            return False  
        current_state = transition_table[current_state].get(input_type, 'q2')

    return current_state in final_states

# Checks whether a token is a valid variable/identifier using a DFA
def is_identifier(token):
    alphabet = {'letter','underscore', 'digit'}
    transition_table = {
        'q0': {'letter': 'q1', 'underscore': 'q2', 'digit': 'q2'},
        'q1': {'letter': 'q1', 'underscore': 'q1', 'digit': 'q1'},
        'q2': {'letter': 'q2', 'underscore': 'q2', 'digit': 'q2'},
    }
    current_state = 'q0'
    final_states = {'q1'}
    for char in token:
        if char.isalpha():
            input_type = 'letter'
        elif char.isdigit():
            input_type = 'digit'
        elif char == '_':
            input_type = 'underscore'
        else:
            return False  
        current_state = transition_table[current_state].get(input_type, 'q2')

    return current_state in final_states


# Checks whether a token is a parenthesis using a DFA
def is_special_symbol(token):
    alphabet = {'parentheses'}
    transition_table = {
        'q0': {'parentheses': 'q1'},
        'q1': {'parentheses': 'q1'},
    }
    current_state = 'q0'
    final_states = {'q1'}
    for char in token:
        if char in '()':
            input_type = 'parentheses'
        else:
            return False  
        current_state = transition_table[current_state].get(input_type)
    return current_state in final_states

# Checks whether a line segment is a comment using a DFA
def is_comment(text):
    transition_table = {
        'q0': {'/': 'q1', 'other': 'q3'},
        'q1': {'/': 'q2', 'other': 'q3'},
        'q2': {'/': 'q2', 'other': 'q2'},
        'q3': {'/': 'q3', 'other': 'q3'}
    }
    current_state = 'q0'
    final_states = {'q2'}
    for char in text:
        if char == '/':
            input_type = '/'
        else:
            input_type = 'other'
        current_state = transition_table.get(current_state, {}).get(input_type, 'q3')

    return current_state in final_states

# Tokenizes a line of code into individual lexemes
def tokenize(line):
    tokens = []
    current = ""

    special_chars = "/^()=*"

    i = 0
    while i < len(line):
        if is_comment(line[i:]):
            if current:
                tokens.append(current)
            tokens.append('//')
            comment_part = line[i+2:].strip()
            if comment_part:
                tokens.append(comment_part)
            break 

        char = line[i]

        if char.isspace():
            if current:
                tokens.append(current)
                current = ""
            i += 1
            continue

        if char in special_chars:
            if current:
                tokens.append(current)
                current = ""
            tokens.append(char)
            i += 1
            continue

        current += char
        i += 1

    if current:
        tokens.append(current)

    return tokens

# Classifies and prints the type of each token
def logic(words):
    for word in words:
        if is_integer(word):
            print(f"{word}\tInteger")
        elif is_float(word):
            print(f"{word}\tFloat")
        elif is_scientific_notation(word):
            print(f"{word}\tScientific notation")
        elif is_operator(word):
            operators = {'=': "Assignment", '+': "Addition", '-': "Subtraction", '*': "Multiplication", '/': "Division", '^': "Power"}
            print(f"{word}\t{operators.get(word, 'Operator')}")
        elif is_identifier(word):
            print(f"{word}\tVariable")
        elif is_special_symbol(word):
            print(f"{word}\t{'Opening parenthesis' if word == '(' else 'Closing parenthesis'}")
        else:
            print(f"{word}\tUnidentified")


# Reads an input file and performs lexical analysis line by line
def arithmetic_lexer(file_name):
    with open(file_name, "r") as file:
        print("Token\tType")
        for line in file:
            line = line.strip()
            tokens = tokenize(line)

            comment_mode = False
            for token in tokens:
                if token == '//':
                    print("//\tComment")
                    comment_mode = True
                elif comment_mode:
                    for word in token.split():
                        print(f"{word}\tComment")
                    break
                else:
                    logic([token])

# Start the lexer with the input file
arithmetic_lexer("expressions.txt")
