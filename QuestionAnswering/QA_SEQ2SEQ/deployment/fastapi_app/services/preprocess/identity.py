from services.preprocess import PreprocessedText, Preprocessor


class IdentityPreprocessor(Preprocessor):
    def preprocess(self, text: str):
        return PreprocessedText(for_user=text, for_trans=text)
