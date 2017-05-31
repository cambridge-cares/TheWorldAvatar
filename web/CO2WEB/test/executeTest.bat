cd CO2WEB
nssm stop indiHostCO2WEB
npm test
ping 127.0.0.1 -n 6 > nul
nssm start indiHostCO2WEB
