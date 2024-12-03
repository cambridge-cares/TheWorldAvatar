"use client";

import Link from 'next/link';
import styles from './keycloak-session.module.css';
import React, { useEffect, useState } from 'react';

const KeycloakSession = () => {
    const [displayName, setDisplayName] = useState(null);

    useEffect(() => {
        const fetchUsername = async () => {
            try {
                const response: Response = await fetch('/api/userinfo');
                if (response.ok) {
                    const userInfo = await response.json();
                    setDisplayName(userInfo.fullName);
                }
            } catch (error) {
                console.warn('Error fetching user info:', error);
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