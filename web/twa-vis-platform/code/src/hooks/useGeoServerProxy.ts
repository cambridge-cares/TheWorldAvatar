import { useEffect, useState } from 'react';

// a fudge because process.env does not work on browser
export function useGeoServerProxy() {
    const [useProxy, setUseProxy] = useState<boolean>(false);

    useEffect(() => {
        const fetchProxyValue = async () => {
            const response = await fetch('/env/use-geoserver-proxy');
            const data = await response.json();
            setUseProxy(data.useGeoServerProxy);
        };

        fetchProxyValue();
    }, []);

    return { useProxy };
}