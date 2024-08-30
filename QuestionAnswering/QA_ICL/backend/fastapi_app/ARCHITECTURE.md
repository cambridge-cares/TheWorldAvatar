# Pipeline

## Step 1: Rewrite the query

    rewritten_query = self.nlq_rewriter.rewrite(question=query)
    
The `query` method of `DataSupporter` calls the `rewrite` method of the `nlq_rewriter` object

This takes the original user query and attempts to standardise it by normalising any physical quantities in the text. It combines a GPT model (see `normalise` method of the `QtNormaliser` class) to detect physical quantities and a unit conversion system (the `normalise_qt` method of the `QtNormaliser` class) to convert these quantities into standardised units. This process helps ensure that the system can understand and process them consistently in the subsequent pipeline steps regardless of how a user expresses physical quantities in their query (e.g., using different units or formats). If no physical quantities are detected or an error occurs during this process, the original query is passed through unchanged.