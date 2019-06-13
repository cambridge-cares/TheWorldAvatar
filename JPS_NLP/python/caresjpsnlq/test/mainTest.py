import os, sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from NLQ_Chunker import NLQ_Chunker

chunker = NLQ_Chunker()
chunker.chunk_a_sentence('Who is the president of China')