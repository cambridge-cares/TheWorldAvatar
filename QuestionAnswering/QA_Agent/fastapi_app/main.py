from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles

from api import qa, html, chat

app = FastAPI()


app.mount("/static", StaticFiles(directory="static"), name="static")

app.include_router(html.router)
app.include_router(chat.router, prefix="/chat")
app.include_router(qa.router, prefix="/qa")