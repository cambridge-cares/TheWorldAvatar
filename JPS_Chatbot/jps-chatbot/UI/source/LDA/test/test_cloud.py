# Import the wordcloud library
from wordcloud import WordCloud
import json
with open('../test_corpus/corpus') as f:
    words = json.loads(f.read())
    print(words)


# Join the different processed titles together.
long_string = ','.join(words[1])
# Create a WordCloud object
wordcloud = WordCloud(background_color="white", max_words=5000, contour_width=3, contour_color='steelblue')
# Generate a word cloud
wordcloud.generate(long_string)
# Visualize the word cloud
img = wordcloud.to_image()
img.show()