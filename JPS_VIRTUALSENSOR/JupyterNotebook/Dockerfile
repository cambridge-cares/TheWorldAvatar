# Use the official Jupyter Notebook image as a base
FROM jupyter/base-notebook

# Install Voilà
RUN pip install voila

RUN pip install ipywidgets

# Expose the Voilà port
EXPOSE 8866

# Start Voilà with the notebook directory as the root
CMD ["voila", "--port=8866", "--no-browser", "--VoilaConfiguration.file_allowlist=['.*']","--Voila.ip=0.0.0.0"]