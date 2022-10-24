type Effect<T> = () => T;

type Channel<T> = Set<(value: T) => Effect<void>>;

export const newChannel = <T>(): Channel<T> => new Set();

export const sendChannel =
  <T>(channel: Channel<T>) =>
  (value: T): Effect<void> =>
  () => {
    channel.forEach((f) => f(value)());
  };

export const subscribeChannel =
  <T>(channel: Channel<T>) =>
  (handler: (value: T) => Effect<void>): Effect<Effect<void>> =>
  () => {
    const f = (value: T) => handler(value);
    channel.add(f);
    return () => {
      channel.delete(f);
    };
  };
