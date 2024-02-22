from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles
from api import translate, sparql, chat, html
from api.dash_app import create_dash_app
from fastapi.middleware.wsgi import WSGIMiddleware
from importlib.resources import files
import json

app = FastAPI()

app.mount("/static", StaticFiles(directory="static"), name="static")

app.include_router(html.router)
app.include_router(translate.router, prefix="/translate")
app.include_router(sparql.router, prefix="/sparql")
app.include_router(chat.router, prefix="/chat")

EXPLORE_OPTIONS = json.loads( files("resources.chemistry").joinpath("explore_options.json").read_text() )
for explore_group in EXPLORE_OPTIONS:
    dash_app = create_dash_app(explore_group["pathname_prefix"], explore_group["dropdown_options"])
    app.mount("/dash_" + explore_group["pathname_prefix"] + "/" , WSGIMiddleware(dash_app.server))