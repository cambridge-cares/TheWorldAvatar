"use client";

import Link from 'next/link';
import styles from './keycloak-session.module.css';
import React, { useEffect, useState } from 'react';

const KeycloakSession = () => {
    const [displayName, setDisplayName] = useState(null);

    useEffect(() => {
        const fetchUsername = async () => {
            try {
                const response = await fetch('/api/userinfo');
                if (response.ok) {
                    const userInfo = await response.json();
                    setDisplayName(userInfo.fullName);
                } else if (response.status != 404) {
                    // Log all other connection errors, but 404 should be ignored
                    // 404 will likely occur only when keycloak is not configured and does not exist
                    console.error("Failed to complete request: ", response);
                }
            } catch (error) {
                console.error('Error fetching user Info', error);
            }
        };

        fetchUsername();
    }, []);

    return (
        displayName && (
            <div id="keycloakSession" className={styles.keycloakSession}>
                <span id="userName" className={styles.dropbtn}>{displayName}</span>
                <div className={styles.dropdownContent}>
                    <Link prefetch={false} href="/logout">Log Out</Link>
                </div>
            </div>)
    );
};

export default KeycloakSession;