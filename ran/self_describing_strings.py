number_names = {
    "One": 1, "Two": 2, "Three": 3, "Four": 4,
    "Five": 5, "Six": 6, "Seven": 7, "Eight": 8,
    "Nine": 9, "Ten": 10, "Eleven": 11, "Twelve": 12,
    "Thirteen": 13, "Fourteen": 14, "Fifteen": 15,
    "Sixteen": 16, "Seventeen": 17, "Eighteen": 18,
    "Nineteen": 19, "Twenty": 20, "Twenty-One": 21,
    "Twenty-Two": 22, "Twenty-Three": 23, "Twenty-Four": 24,
    "Twenty-Five": 25, "Twenty-Six": 26, "Twenty-Seven": 27,
    "Twenty-Eight": 28, "Twenty-Nine": 29, "Thirty": 30
}

def generate_self_describing_strings():
    # We will compare the value of the string with length of the string.
    # One might debate which of these words includes spaces.
    suffix_list = ["chars", "letters","glyphs","elements","characters", "letters"]

    suffix_list = suffix_list + [x + " exist" for x in suffix_list] + [x + " are in this string " for x in suffix_list]

    # We want to be able to look up a suffix with given length.
    suffixes = {len(suffix): suffix for suffix in suffix_list}

    self_describing_strings = []

    for number_name, number_value in number_names.items():
        # Now, the required suffix length is the number value minus the length of the number name plus a space.
        suffix_len = number_value - len(number_name) - 1
        if suffix_len in suffixes:
            self_describing_strings.append(number_name + " " + suffixes[suffix_len])

    return self_describing_strings

# this is probably the best code in this whole program
def longest_string(string_list):
    return max(string_list, key=len, default=None)

self_describing_strings = generate_self_describing_strings()

longest = longest_string(self_describing_strings)

longest_spelled_out = {val: key for key, val in number_names.items()}[len(longest)]

formatted_output = f"There are {longest_spelled_out} letters in the longest string.\nThe longest string is \"{longest}\"."

print(formatted_output)
