import openai
import sys
import os
sys.path.append("")
from Marie.Util.location import DATA_DIR

class ChatGPTQAEngine:
    def __init__(self):
        self.model = "gpt-3.5-turbo"
        with open(os.path.join(DATA_DIR, 'chatgpt_api_key.txt'), "r") as file:
            self.api_key = file.read().strip()
        openai.api_key = self.api_key

    def ask_question(self, question):
        try:
            response = openai.ChatCompletion.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": "You are a helpful assistant to answer questions in the Chemistry domain."},
                    {"role": "user", "content": question},
                ],
                temperature=0,
            )
        except Exception as e:
            return f"An error occurred: {str(e)}"
        
        answer = response.choices[0].message.content
        return answer
    
if __name__=="__main__":
    chatbot = ChatGPTQAEngine()

    while True:
        user_input = input("User: ")
        answer = chatbot.ask_question(user_input)
        print("ChatGPT:", answer)