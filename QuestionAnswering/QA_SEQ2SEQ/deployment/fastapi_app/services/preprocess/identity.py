from services.preprocess import PreprocessedText, IPreprocessor


class IdentityPreprocessor(IPreprocessor):
    def preprocess(self, text: str):
        return PreprocessedText(for_user=text, for_trans=text)
