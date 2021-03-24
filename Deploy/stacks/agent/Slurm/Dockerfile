#########################
#
# This docker file creates an Image for use as a
# Slurm compute node.
# 
# The "docker build" command used to build this file
# into a Image should be run from the docker directory.
# See the README for more details.
#
#########################

# Use the CentOS 7 as the base
FROM docker.cmclinnovations.com/centos7-systemd:latest

# Update package repository
RUN yum update -y

# Add the EPEL repository
RUN rpm -Uvh http://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm

# Install tools required to build Slurm
RUN yum install -y wget expect munge-devel munge-libs readline-devel perl-ExtUtils-MakeMaker openssl-devel pam-devel rpm-build perl-DBI perl-Switch munge 
RUN yum groupinstall -y "Development tools"

# Install MariaDB
RUN yum install -y mariadb-server mariadb-devel

# Install Python3
# Note: This will break yum (as it only works with Python2),
# 	    so should be done as the last yum installation.
RUN yum install -y python3
RUN alternatives --install /usr/bin/python python /usr/bin/python2 1
RUN alternatives --install /usr/bin/python python /usr/bin/python3 2

# Download slurm source
#RUN wget https://download.schedmd.com/slurm/slurm-20.11.5.tar.bz2

# Build the RPM packages
#RUN rpmbuild -ta ./slurm-20.11.5.tar.bz2

# Install the RPM packages
#RUN rpm -Uvh ~/rpmbuild/RPMS/x86_64/*.rpm

# Create a slurm user
RUN useradd slurm

# Place for slurm logs
RUN mkdir /var/log/slurm
RUN chown slurm. /var/log/slurm

# TODO: Run the MYSQL setup script
#WORKDIR /tmp
#COPY mysql-setup.sh mysql-setup.sh
#RUN chmod +x mysql-setup.sh

# TODO: Setup SSH access
#EXPOSE 22

# Entrypoint
CMD ["/usr/sbin/init"]