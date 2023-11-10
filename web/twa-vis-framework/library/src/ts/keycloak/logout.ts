let domainName: string = 'idm-credo.hartree.app'; //auth-server-url
let realmName: string = 'MFATest';
let logoutUrl: string = `http://${domainName}/realms/${realmName}/protocol/openid-connect/logout`; // template string


let logoutLink = document.createElement('a');
logoutLink.href=logoutUrl;
let logoutButton = document.createElement('button');
logoutButton.textContent = "Log Out";
logoutLink.appendChild(logoutButton);

// Clicking the above button 


// needs to be in the keycloak main.js. This keeps refreshing the token

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