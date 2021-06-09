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
FROM centos:7

# Update package repository
RUN yum update -y

# Add the EPEL repository
RUN rpm -Uvh http://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm

# Install tools required to build Slurm
RUN yum install -y munge-devel munge-libs readline-devel perl-ExtUtils-MakeMaker openssl-devel pam-devel rpm-build perl-DBI perl-Switch munge 
RUN yum groupinstall -y "Development tools"
RUN yum install -y nano wget expect 

# Install MariaDB
RUN yum install -y mariadb-server mariadb-devel

# Install Python3
# This will break yum (only works with Python2), so should be done as the last yum call.
RUN yum install -y python3
RUN alternatives --install /usr/bin/python python /usr/bin/python2 1
RUN alternatives --install /usr/bin/python python /usr/bin/python3 2

# Download and build Slurm
RUN wget https://download.schedmd.com/slurm/slurm-20.11.5.tar.bz2
RUN rpmbuild -ta ./slurm-20.11.5.tar.bz2
RUN rpm -Uvh ~/rpmbuild/RPMS/x86_64/*.rpm

# Create a slurm user
RUN useradd slurm
RUN usermod --password m0delsrus slurm

# Create place for slurm logs
RUN mkdir /var/log/slurm
RUN chown slurm:slurm /var/log/slurm

# Copy in required files and scripts
COPY slurmdbd.conf /etc/slurm/slurmdbd.conf
COPY mysql_secure_installation.sh /tmp/mysql_secure_installation.sh
COPY startup.sh /tmp/startup.sh
RUN chmod -R 755 /tmp



#########


# TODO: Setup SSH access
#EXPOSE 22

# Entrypoint to keep the container running
ENTRYPOINT ["tail", "-f", "/dev/null"]