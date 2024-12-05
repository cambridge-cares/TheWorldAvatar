import logging
from fastapi import FastAPI, Request, Response
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles

from routers import qa, html, chat, ontospecies, ontozeolite

logger = logging.getLogger(__name__)

app = FastAPI()


async def catch_exceptions_middleware(request: Request, call_next):
    """
    Middleware to catch and log exceptions
    """
    try:
        return await call_next(request)
    except Exception:
        logging.exception("Unhandled exception")
        return Response("Internal server error", status_code=500)

# Add exception handling middleware
app.middleware("http")(catch_exceptions_middleware)

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_methods=["*"],
    allow_headers=["*"],
)

# Serve static files
app.mount("/static", StaticFiles(directory="static"), name="static")

# Include routers for the application
app.include_router(html.router)
app.include_router(ontospecies.router, prefix="/ontospecies", tags=["ontospecies"])
app.include_router(ontozeolite.router, prefix="/ontozeolite", tags=["ontozeolite"])
app.include_router(chat.router, prefix="/chat")
app.include_router(qa.router, prefix="/qa")