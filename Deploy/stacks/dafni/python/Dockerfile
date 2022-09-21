ARG PYTHON_VERSION=3.7.12
ARG PYTHON_SCRIPTS_DIR=/scripts

FROM python:${PYTHON_VERSION}-slim-bullseye


RUN apt update && apt install -y openssh-server python3-tk xauth

RUN ssh-keygen -A \
    && mkdir /root/.ssh \
    && chmod 0700 /root/.ssh \
    && echo "root:root" | chpasswd \
    && ln -s /etc/ssh/ssh_host_ed25519_key.pub /root/.ssh/authorized_keys

EXPOSE 22

# Modify some sshd options
COPY sshd_config_overrides.conf /etc/ssh/sshd_config.d/


# Create privilege separation directory
RUN mkdir -p /run/sshd

WORKDIR $PYTHON_SCRIPTS_DIR

ENTRYPOINT ["/usr/sbin/sshd", "-D", "-e"]