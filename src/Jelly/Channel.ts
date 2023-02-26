type Channel<T> = Set<(value: T) => void>;

export const newChannel = <T>(): Channel<T> => new Set();
export const send =
  <T>(channel: Channel<T>) =>
  (value: T) =>
  () => {
    channel.forEach((f) => f(value));
  };

export const subscribe =
  <T>(channel: Channel<T>) =>
  (f: (value: T) => () => void) => {
    const listener = (value: T) => f(value)();
    channel.add(listener);
    return () => channel.delete(listener);
  };
