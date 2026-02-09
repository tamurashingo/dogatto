import { useEffect, useState } from 'react';

/**
 * Custom hook for debouncing a value.
 *
 * Returns a debounced version of the input value that only updates after
 * the specified delay has elapsed since the last change.
 *
 * @param value [T] Value to debounce
 * @param delay [number] Delay in milliseconds
 * @return [T] Debounced value
 */
export function useDebounce<T>(value: T, delay: number): T {
  const [debouncedValue, setDebouncedValue] = useState<T>(value);

  useEffect(() => {
    const handler = setTimeout(() => {
      setDebouncedValue(value);
    }, delay);

    return () => {
      clearTimeout(handler);
    };
  }, [value, delay]);

  return debouncedValue;
}
