#########################
#
# This docker file creates an Image with TippeCanoe
#
#########################

# Using Ubuntu (with Ruby) as the base 
FROM drecom/ubuntu-ruby:2.6.0

# Meta data
LABEL authors = "support@cmclinnovations.com"
LABEL version = "1.8.1"
LABEL description = "Tippecanoe"

# Install utilities
RUN apt update && apt install -y nano curl bash git gcc

# Create directories 
WORKDIR /usr/local/tippecanoe
RUN mkdir -p /usr/local/tippecanoe

# Install homebrew
RUN /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" < /dev/null
ENV PATH "$PATH:/home/linuxbrew/.linuxbrew/bin"

# Install tippecanoe
RUN brew install tippecanoe

# Keep container running on boot
CMD tail -f /dev/null