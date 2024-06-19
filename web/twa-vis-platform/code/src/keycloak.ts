import Keycloak from 'keycloak-js'

const keycloak = new Keycloak({
    url: "https://idm-credo.hartree.app",
    realm: "MFATest",
    clientId: "credo-app",
});

export default keycloak