class KeycloakLogoutButton {
    constructor(domainName, realmName) {
        this.domainName = domainName;
        this.realmName = realmName;
        this.logoutURL = `http://${domainName}/realms/${realmName}/protocol/openid-connect/logout`;
        this.createButton();
    }
    createButton() {
        let logoutLink = document.createElement('a');
        logoutLink.href = this.logoutURL;
        let logoutButton = document.createElement('button');
        logoutButton.textContent = "Log Out";
        logoutLink.appendChild(logoutButton);
        logoutButton.classList.add('logout-button');
    }
}
const logoutButtonInstance = new KeycloakLogoutButton('idm-credo.hartree.app', 'MFATest');
