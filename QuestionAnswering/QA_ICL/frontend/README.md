# Marie Frontend

## Table of Contents
- [Marie Frontend](#marie-frontend)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Project Structure](#project-structure)
  - [Backend Integration](#backend-integration)


## Overview

The frontend of Marie serves as the user interface for interacting with the backend services that handle natural language queries related to chemistry data. Built using Next.js, the frontend communicates with the backend to perform operations such as querying chemical species and filtering zeolites, and it displays the results to users in a user-friendly manner.

## Project Structure

The frontend is organised into the following main directories:

- [`mock_backend`](./mock_backend/): A minimal Express server used for simulating backend API responses during development.
- [`next_app_marie`](./next_app_marie/): The Next.js application containing the main code for the user interface and client-side logic.

## Backend Integration
The frontend communicates with the Marie backend to execute natural language queries and retrieve relevant chemistry data. The backend provides RESTful APIs, documented in the API Documentation, which the frontend consumes to display data in a user-friendly interface.

To ensure smooth communication between the frontend and backend:
 - Verify the backend endpoint in the .env file.
 - Ensure the backend is running (check the Backend README for details).

