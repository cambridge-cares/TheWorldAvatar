import Keycloak from "keycloak-js";
const keycloak = new Keycloak({
 url: "/https://idm-credo.hartree.app/",
 realm: "MFATest",
 clientId: "credo",
});

export default keycloak;

try {
    const authenticated = await keycloak.init();
    console.log(`User is ${authenticated ? 'authenticated' : 'not authenticated'}`);
} catch (error) {
    console.error('Failed to initialize adapter:', error);
}