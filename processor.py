from nltk import word_tokenize, sent_tokenize

def strip_punctuation(text):
    table = str.maketrans(dict.fromkeys(':,.?\n'))
    s = text.translate(table)
    return s