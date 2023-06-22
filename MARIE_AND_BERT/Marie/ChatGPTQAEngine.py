import openai

class ChatGPTQAEngine:
    def __init__(self, api_key):
        # self.api_key = api_key
        self.model = "gpt-3.5-turbo"
        openai.api_key = api_key

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
    api_key = "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"
    chatbot = ChatGPTQAEngine(api_key)

    while True:
        user_input = input("User: ")
        answer = chatbot.ask_question(user_input)
        print("ChatGPT:", answer)