# Use an official Python runtime as the base image
FROM python:3.8-slim

# Set the working directory in the container
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Install any needed packages specified in requirements.txt
RUN pip install --no-cache-dir -r /app/requirements.txt

# Make port 9047 available to the world outside this container
EXPOSE 9047

# Define environment variable
ENV FLASK_APP=entry_point.py

# Run the app
CMD ["python", "-m", "smellagent.entry_point"]