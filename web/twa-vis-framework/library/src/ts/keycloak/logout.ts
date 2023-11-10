class keycloakLogoutButton {

  private domainName: string;
  private realmName: string;
  private logoutURL: string; // There is a URL interface but i'm not sure what that means

  constructor(domainName: string, realmName: string) {
    this.domainName = domainName; //auth-server-url in the keycloac documentation
    this.realmName =realmName;
    this.logoutURL = `http://${domainName}/realms/${realmName}/protocol/openid-connect/logout`; // template string
  
  this.createButton();
  }

  private createButton() {
    let logoutLink = document.createElement('a');
    logoutLink.href=this.logoutURL;
    
    let logoutButton = document.createElement('button');
    logoutButton.textContent = "Log Out";

    logoutLink.appendChild(logoutButton);

    logoutButton.classList.add('logout-button') // for styling
  }
}
  

const logoutButtonInstance = new keycloakLogoutButton('idm-credo.hartree.app', 'MFATest');


// Below should to be in the keycloak main.js. This keeps refreshing the token. 
  
// Token if exposed will give us username etc


//  setInterval() => {
//     keycloak.updateToken().then(refreshed) => {
//       if (store.state.user.isAuthenticated != false ) {
//         if (refreshed) {       
//           let payloadRefreshedTokens = {
//             idToken: keycloak.idToken,
//             accessToken: keycloak.token
//           }
//           if ((keycloak.token && keycloak.idToken) != '') {
//             store.commit("login", payloadRefreshedTokens);
//           }
//           else {
//             payloadRefreshedTokens = {
//               idToken: "",
//               accessToken: ""
//             }
//             store.commit("login", payloadRefreshedTokens);
//             store.commit("logout");
//           }
//         }
//       }