import { useState, useCallback } from 'react';

// Custom hook: useRefresh
const useRefresh = (): [boolean, () => void]  => {
  const [refreshFlag, setRefreshFlag] = useState<boolean>(false);

  // Prevent unnecessary re-creations of the refresh function on every render
  const triggerRefresh = useCallback(() => {
    setRefreshFlag(true);
    setTimeout(() => setRefreshFlag(false), 500);
  }, []);

  return [refreshFlag, triggerRefresh];
};

export default useRefresh;