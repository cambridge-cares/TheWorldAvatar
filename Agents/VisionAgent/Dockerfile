# Python runtime
FROM python:3.10-slim

# Install required libraries for cv2
RUN apt-get update && apt-get install -y libgl1-mesa-glx libglib2.0-0

# Container working dir
WORKDIR /app

# Copy the requirements.txt into the container at /app
COPY requirements.txt /app/

# Install the required libraries
RUN pip install --no-cache-dir -r requirements.txt

# copy the current dir into container
COPY . /app/

# Explose to port number
EXPOSE 9048

# Define the working environmental variable
ENV FLASK_ENV=production

# Launch the app
CMD ["python", "-m", "visionagent.entry_point"]
