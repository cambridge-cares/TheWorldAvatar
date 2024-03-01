from fastapi import FastAPI

from api import qa

app = FastAPI()


app.include_router(qa.router, prefix="/qa")
