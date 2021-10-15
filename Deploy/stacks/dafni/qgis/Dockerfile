FROM qgis/qgis:release-3_20

RUN apt update && apt install -y openssh-server xauth

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

# Loading Qt5 lib fails without this when qgis is run
RUN strip --remove-section=.note.ABI-tag /usr/lib/x86_64-linux-gnu/libQt5Core.so.5

ENTRYPOINT ["/usr/sbin/sshd", "-D", "-e"]
