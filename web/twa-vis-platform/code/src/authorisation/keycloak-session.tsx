"use client";

import Link from 'next/link';
import styles from './keycloak-session.module.css';
import React, { useEffect, useState } from 'react';

const KeycloakSession = () => {
    const [username, setUsername] = useState(null);

    useEffect(() => {
        // Fetch the username from your backend
        const fetchUsername = async () => {
            try {
                const response = await fetch('/api/userinfo');
                const userInfo = await response.json();
                setUsername(userInfo.firstName);
                console.log(userInfo.firstName);
            } catch (error) {
                console.error('Error fetching user Info', error);
            }
        };

        fetchUsername();
    }, []);

    return (
        username && (
            <div id="keycloakSession" className={`${styles.keycloakSession} ${styles.dropdown}`}>
                <span id="userName" className={styles.dropbtn}>{username}</span>
                <div className={styles.dropdownContent}>
                    <Link href="/logout">Log Out</Link>
                    <Link href="./protected">API</Link>
                </div>
            </div>)
    );
};

export default KeycloakSession;