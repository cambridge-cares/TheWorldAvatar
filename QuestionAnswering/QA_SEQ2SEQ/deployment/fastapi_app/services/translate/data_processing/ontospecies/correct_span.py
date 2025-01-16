from typing import Optional
import Levenshtein


def remove_punctuation_or_contraction(text: str):
    terminal_chars = [".", ",", ";", "?", "!", "'s", "'ll"]
    for c in terminal_chars:
        if text.endswith(c):
            return text[:-len(c)]
    return text


class OSSpanCorrector:
    def correct(self, nlq: Optional[str], span: str):
        if nlq is None or span in nlq:
            return span
        
        nlq_words = [remove_punctuation_or_contraction(x) for x in nlq.split()]
        span_words = span.split()
        span = " ".join(span_words)

        if len(span_words) > len(nlq_words):
            return span
        
        candidates = [
            " ".join(nlq_words[i : i + len(span_words)])
            for i in range(0, len(nlq_words) - len(span_words) + 1)
        ]
        distances = [Levenshtein.distance(span, x) for x in candidates]
        idx_min = min(range(len(distances)), key=lambda i: distances[i])

        return candidates[idx_min]