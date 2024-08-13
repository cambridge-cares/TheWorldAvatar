# Question-Answering for The World Avatar using In-Context Learning

## Introduction

Question-answering for The World Avatar involves retrieving data from RDF graphs and other data sources such as HTTP endpoints. To do so, input questions need to be converted into SPARQL queries and HTTP requests, whose execution would yield the desired data. The conversion of natural language queries to data requests is facilitated by in-context learning, which entails engineering a text prompt for LLMs to automatically perform the transformation. The prompt may include context information such as parsing examples and the structure of target predictions.

## Project Structure

- [`data_generation`](./data_generation): encompases Python scripts to prepare data for entity linking and in-context learning.
- [`backend`](./backend): Marie backend
- [`frontend`](./frontend): Next app for Marie frontend and a minimal mock backend server
