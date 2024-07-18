from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles

from routers import qa, html, chat, ontospecies, ontozeolite

app = FastAPI()

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_methods=["*"],
    allow_headers=["*"],
)

app.mount("/static", StaticFiles(directory="static"), name="static")

app.include_router(html.router)
app.include_router(ontospecies.router, prefix="/ontospecies")
app.include_router(ontozeolite.router, prefix="/ontozeolite")
app.include_router(chat.router, prefix="/chat")
app.include_router(qa.router, prefix="/qa")
