from unit_parse import parser
from pint import Quantity

from services.preprocess import PreprocessedText
from services.preprocess import IPreprocessor


class ChemistryPreprocessor(IPreprocessor):
    def _startswith_magnitude(self, text: str, start: int = 0):
        return (
            text[start].isdigit()
            or (
                text[start] == "-"
                and start < len(text) - 1
                and text[start + 1].isdigit()
            )
            or text[start] == "."
        )

    def _is_magnitude_char(self, char: str):
        return char in ["-", ".", "*", "+", "e", "E", "x"] or char.isdigit()

    def _advance_ptr_thru_space(self, text: str, ptr: int = 0):
        while ptr < len(text) and text[ptr].isspace():
            ptr += 1
        return ptr

    def _advance_past_magnitude(self, text: str, ptr: int = 0):
        """Advances the pointer past the magnitude substring.

        If the input text starts with a magnitude, advance the pointer past the magnitude substring.
        Otherwise, return the current pointer.

        Args:
            text: The input string.
            ptr: The current pointer position.

        Returns:
            The new pointer position that marks the end of the magnitude substring.
        """
        if not self._startswith_magnitude(text, ptr):
            return ptr

        while ptr < len(text):
            if ptr < len(text) and self._is_magnitude_char(text[ptr]):
                ptr += 1

            new_ptr = self._advance_ptr_thru_space(text, ptr)
            if new_ptr >= len(text):
                break

            if self._is_magnitude_char(text[new_ptr]):
                ptr = new_ptr
            else:
                break

        while ptr >= 1 and text[ptr - 1].isspace():
            ptr -= 1

        return ptr

    def _advance_past_unit(self, text: str, ptr: int = 0):
        """Advances the pointer past the unit substring.

        Args:
            text: The input string.
            ptr: The current pointer position.

        Returns:
            The new pointer position that marks the end of the unit substring.
        """
        ptr = self._advance_ptr_thru_space(text, ptr)

        while ptr < len(text):
            while ptr < len(text) and text[ptr].isalpha():
                ptr += 1

            new_ptr = self._advance_ptr_thru_space(text, ptr)
            if new_ptr >= len(text):
                break

            flag = False
            for c in ["*", "**", "/", "°"]:
                if text.startswith(c, new_ptr):
                    flag = True
                    ptr = new_ptr + len(c)
                    break
            if not flag:
                break

        while ptr >= 1 and text[ptr - 1].isspace():
            ptr -= 1

        return ptr

    def _advance_to_magnitude(self, text: str, ptr: int = 0):
        """Advances the pointer to the magnitude substring.

        Args:
            text: The input string.
            ptr: The current pointer position.

        Returns:
            The new pointer position that marks the start of a magnitude substring.
        """
        while ptr < len(text) and (
            (not self._startswith_magnitude(text, ptr))
            or (ptr > 0 and not text[ptr - 1].isspace())
        ):
            ptr += 1

        return ptr

    def preprocess(self, text: str):
        """Converts any physical quantities to SI units.

        Args:
            text: The input string.

        Returns:
            A `PreprocessedText` object whose fields are
                `for_user`: both magnitude and SI units are included,
                `for_trans`: only magnitude is included.
        """
        ptr = self._advance_to_magnitude(text)
        if ptr >= len(text):
            preprocessed_text_for_user = str(text)
            preprocessed_text_for_trans = str(text)
        else:
            text_segment = text[:ptr]
            text_segments_for_user = [text_segment]
            text_segments_for_trans = [text_segment]

            while ptr < len(text):
                ptr_after_magnitude = self._advance_past_magnitude(text, ptr)
                if ptr_after_magnitude >= len(text):
                    text_segment = text[ptr:]
                    text_segments_for_user.append(text_segment)
                    text_segments_for_trans.append(text_segment)
                    break

                ptr_at_unit = self._advance_ptr_thru_space(text, ptr_after_magnitude)
                ptr_after_unit = self._advance_past_unit(text, ptr_at_unit)

                text_segment = text[ptr:ptr_after_unit]
                parsed = parser(text_segment)

                if parsed is None:
                    text_for_user = text_segment
                    text_for_trans = text_segment
                elif isinstance(parsed, Quantity):
                    if parsed.unitless:
                        text_for_user = text_segment
                        text_for_trans = text_segment
                    else:
                        quantity_converted = parsed.to_base_units()  # meter/kg/second
                        if str(quantity_converted.units) == "kg / mol":
                            quantity_converted = (
                                parsed.to_root_units()
                            )  # meter/gram/second

                        text_for_user = str(quantity_converted)
                        text_for_trans = str(quantity_converted.magnitude)
                else:  # [[quantity, condition], [quantity, condition], ...]
                    # This is not expected to happen because we parse a text segment with a single quantity
                    # raise Exception("Unexpected parsed results")
                    return PreprocessedText("", "")

                text_segments_for_user.append(text_for_user)
                text_segments_for_trans.append(text_for_trans)

                ptr = self._advance_to_magnitude(text, ptr_after_unit)
                text_segment = text[ptr_after_unit:ptr]
                text_segments_for_user.append(text_segment)
                text_segments_for_trans.append(text_segment)

            preprocessed_text_for_user = "".join(text_segments_for_user)
            preprocessed_text_for_trans = "".join(text_segments_for_trans)

        return PreprocessedText(
            for_user=preprocessed_text_for_user,
            for_trans=preprocessed_text_for_trans,
        )
