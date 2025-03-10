class StringUtils:    
    @staticmethod
    def replace_with_dict(input_string: str, replacement_dict: dict) -> str:
        for old, new in replacement_dict.items():
            input_string = input_string.replace(f"%{old}%", new)
        return input_string