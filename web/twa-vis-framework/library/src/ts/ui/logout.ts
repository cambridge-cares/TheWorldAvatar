//     <script src="./keycloak.min.js"></script>

declare const Keycloak: any;

interface Profile {
    firstName: string;
    lastName: string;
}

function initKeycloak(): void {
    const kc = new Keycloak({
        url: "https://idm-credo.hartree.app",
        realm: "MFATest",
        clientId: "credo-app",
    });

    kc.init({ onLoad: 'login-required' })
        .then(function (authenticated: boolean) {
            console.log(authenticated ? "user is KeyCloak authenticated" : "user is not KeyCloak authenticated");

            if (authenticated) {
                kc.loadUserProfile().then(function (profile) {
                    document.getElementById('userName').innerHTML = profile.firstName + ' ' + profile.lastName;
                });
            }
        }).catch(function () {
            console.log('CReDo KeyCloak failed to initialize');
        });
}