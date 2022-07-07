# HPLC Agent

# Deployment
```cmd
cd C:\Users\usr
docker run -v "C:\CHEM32:/app/CHEM32" --env-file agent.hplc.env --add-host=localhost:host-gateway --name hplc_agent ghcr.io/cambridge-cares/hplc_agent:1.0.0-SNAPSHOT
```

# Author
Jiaru Bai (jb2197@cam.ac.uk)
