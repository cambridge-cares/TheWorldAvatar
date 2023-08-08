#
# This image houses an FTPS server and an Apache webserver. It is intended to be built into a
# container that will sit alongside a website and act as a CDN for publicly available JavaScript 
# libraries. As such, nothing added to this FTP server should be considered protected in any way.
# For more details, or to report issues, please contact CMCL Innovations. 
#

# Use pure ftpd base image
FROM stilliard/pure-ftpd

# Install utilities
RUN apt update && apt install -y nano

# Install and setup Apache web server
RUN apt update && apt install -y apache2  
COPY 000-default.conf /etc/apache2/sites-enabled/000-default.conf

RUN a2enmod headers

EXPOSE 80

# Command to run at start up
CMD /etc/init.d/apache2 start && /run.sh -c 30 -C 10 -l puredb:/etc/pure-ftpd/pureftpd.pdb -j -R -P $PUBLICHOST -p 30000:30009