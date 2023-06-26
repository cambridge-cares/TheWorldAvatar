The BLINK library is included in the source code of this project due to changes on the 
library source code:
In `MARIE_AND_BERT\Marie\EntityLinking\elq\biencoder\biencoder.py`, replace `BertTokenizer` to `BertTokenizerFast`
as the `BertTokenizerFast` is able to provide the original position information of tokens. 
