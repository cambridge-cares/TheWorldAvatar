from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles

from api import translate, sparql, chat, html

app = FastAPI()

app.mount("/static", StaticFiles(directory="static"), name="static")

app.include_router(html.router)
app.include_router(translate.router, prefix="/translate")
app.include_router(sparql.router, prefix="/sparql")
app.include_router(chat.router, prefix="/chat")