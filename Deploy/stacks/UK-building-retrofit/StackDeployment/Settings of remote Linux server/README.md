
# Remote Server Connection Guide

This guide provides a approach to connecting to remote Linux server using SSH, including steps for generating SSH keys, configuring WSL (if needed), and using VS Code for remote access.

## 1. Generate SSH Keys

To establish a secure connection, first generate a public and private SSH key pair:

```bash
ssh-keygen -t rsa
```

- **Private Key (id_rsa)**: This is your private key. Keep it secure and do not share it.
- **Public Key (id_rsa.pub)**: This is your public key. Share it with the remote server manager to enable authentication.

The keys will be stored in the specified location, typically in `~/.ssh/`.

## 2. Copy SSH Keys (For WSL Users)

If you are using Windows Subsystem for Linux (WSL), copy your SSH keys from Windows to WSL:

```bash
mkdir -p ~/.ssh
cp /mnt/c/Users/your-username/.ssh/id_rsa ~/.ssh/
cp /mnt/c/Users/your-username/.ssh/id_rsa.pub ~/.ssh/
chmod 600 ~/.ssh/id_rsa
chmod 644 ~/.ssh/id_rsa.pub
```

> **Note**: Replace `your-username` with your actual Windows username.

## 3. Add Public Key to Remote Server

Copy your public key (`id_rsa.pub`) to the remote server to allow SSH access. Use the following command:

```bash
ssh-copy-id -i ~/.ssh/id_rsa.pub user@remote-server-address
```

Alternatively, you can manually add the public key to the `~/.ssh/authorized_keys` file on the remote server.

## 4. Connect to Remote Server

Use the SSH command to connect to your remote server:

```bash
ssh -i ~/.ssh/id_rsa user@remote-server-address
```

Replace `user` with your username on the remote server and `remote-server-address` with the server's IP or domain name.

## 5. VS Code Remote Setup (Optional)

1. Install the **Remote - SSH** extension in VS Code.
2. Open the Command Palette (F1) and choose **Remote-SSH: Connect to Host**.
3. Enter your SSH details (e.g., `user@remote-server-address`).
4. Start coding on the remote server directly from VS Code.

## 6. Common Issues

- **Permission Denied**: Ensure your private key file (`id_rsa`) has the correct permissions:

  ```bash
  chmod 600 ~/.ssh/id_rsa
  ```

- **Host Key Verification Failed**: Remove the old host key if the server address has changed:

  ```bash
  ssh-keygen -R remote-server-address
  ```

## 7. Important Reminder on Firewall Configuration

For users working on cloud servers like Digital Ocean or similar platforms, **do not enable firewall rules directly on individual droplets** using tools like `ufw` or `iptables`. Most cloud providers, including Digital Ocean, manage firewall settings at the account or network level through their web console. If you need to open specific ports for services, request the change through your server administrator.
---

## References

- [SSH Key Generation Guide](https://www.ssh.com/academy/ssh/keygen)
- [Using SSH with VS Code](https://code.visualstudio.com/docs/remote/ssh)
- [Troubleshooting SSH Issues](https://www.ssh.com/academy/ssh/troubleshooting)
