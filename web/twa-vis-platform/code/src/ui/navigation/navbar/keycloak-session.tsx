"use client";

import styles from './navbar.module.css';

import React, { useEffect, useState } from 'react';
import keycloak from 'keycloak';

export interface KeycloakSessionProps {
    name: string,
    icon: string,
    url: string,
    active?: boolean,
    callback?: (name: string) => void
}


export default function KeycloakSession() {
    // const [userProfile, setUserProfile] = useState(null);

    // useEffect(() => {
        
    // }, []);

    return (
        <div id="keycloakSession" className={`${styles.keycloak_session} ${styles.dropdown}`}>
            <span id="userName" className="dropbtn">{`${keycloak.userInfo}`}</span>
            <div className="dropdown-content">
                <a href="https://idm-credo.hartree.app/realms/master/protocol/openid-connect/logout">Log Out</a>
                <a href="https://credo.stfc.ac.uk/">Home</a>
            </div>
        </div>
    );
}