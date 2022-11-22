DATABASE='postgres'
NAMESPACE='kb'

# Hostname to use for communication with test containers
# For local agent test: 'localhost
# For dockerised agent test: 'host.docker.internal' ('localhost' gets replaced automatically in Dockerfile)
HOSTNAME='localhost'

# Boolean flag whether agent is deployed locally or as docker container
# Default: False (gets replaced automatically in Docker container for dockerised tests)
DOCKERISED_TEST=False
