LESS_THAN = "<"
GREATER_THAN = ">"
LESS_THAN_EQUAL = "<="
GREATER_THAN_EQUAL = ">="
EQUAL = "="
AROUND = "around"
INSIDE = "in"

COMPARATIVES = [
    LESS_THAN,
    GREATER_THAN,
    LESS_THAN_EQUAL,
    GREATER_THAN_EQUAL,
    EQUAL,
    AROUND,
    INSIDE,
]

COMPARATIVE_LABELS = {
    LESS_THAN: ["<", "less than", "lower than", "smaller than"],
    GREATER_THAN: [">", "greater than", "higher than", "bigger than"],
    LESS_THAN_EQUAL: ["<=", "less than or equal to", "not greater than"],
    GREATER_THAN_EQUAL: [">=", "greater than or equal to", "not less than"],
    EQUAL: ["=", "equal to"],
    AROUND: ["around", "approximately"],
    INSIDE: ["inside the range"],
}
