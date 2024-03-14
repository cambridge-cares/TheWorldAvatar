from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles

from api import qa, html, chat
from config import QA_SUPERDOMAIN

app = FastAPI()


app.mount("/static", StaticFiles(directory="static"), name="static")

app.include_router(html.router)
app.include_router(chat.router, prefix="/chat")

if QA_SUPERDOMAIN == "chemistry":
    app.include_router(qa.chemistry_router, prefix="/qa")
elif QA_SUPERDOMAIN == "cities":
    app.include_router(qa.cities_router, prefix="/qa")
else:
    raise ValueError("`QA_SUPERDOMAIN` must be either `chemistry` or `cities`")
