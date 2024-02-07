var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var KeycloakLogoutButton = function () {
    function KeycloakLogoutButton(domainName, realmName) {
        _classCallCheck(this, KeycloakLogoutButton);

        this.domainName = domainName;
        this.realmName = realmName;
        this.logoutURL = 'http://' + domainName + '/realms/' + realmName + '/protocol/openid-connect/logout';
        this.createButton();
    }

    _createClass(KeycloakLogoutButton, [{
        key: 'createButton',
        value: function createButton() {
            var logoutLink = document.createElement('a');
            logoutLink.href = this.logoutURL;
            var logoutButton = document.createElement('button');
            logoutButton.textContent = "Log Out";
            logoutLink.appendChild(logoutButton);
            logoutButton.classList.add('logout-button');
        }
    }]);

    return KeycloakLogoutButton;
}();

var logoutButtonInstance = new KeycloakLogoutButton('idm-credo.hartree.app', 'MFATest');