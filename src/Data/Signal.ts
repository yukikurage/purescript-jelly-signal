type Channel<T> = {
  subscriptions: Set<() => void>;
  value: T;
};

type Signal<T> = {
  subscribe: (callback: () => void) => void;
  unsubscribe: (callback: () => void) => void;
  getValue: () => T;
};

export const newChannelImpl =
  <T>(initialValue: T) =>
  (): Channel<T> => ({
    subscriptions: new Set(),
    value: initialValue,
  });

export const mutateImpl =
  <T>(fn: (value: T) => T) =>
  (channel: Channel<T>) =>
  () => {
    channel.value = fn(channel.value);
    channel.subscriptions.forEach((callback) => callback());
  };

export const sendImpl =
  <T>(value: T) =>
  (channel: Channel<T>) =>
  () => {
    channel.value = value;
    channel.subscriptions.forEach((callback) => callback());
  };

export const subscribe = <T>(channel: Channel<T>) => ({
  subscribe: (callback: () => void) => {
    channel.subscriptions.add(callback);
  },
  unsubscribe: (callback: () => void) => {
    channel.subscriptions.delete(callback);
  },
  getValue: () => channel.value,
});

export const runSignalImpl = (signal: Signal<() => () => void>) => () => {
  let cleaner = signal.getValue()();
  const callback = () => {
    cleaner();
    cleaner = signal.getValue()();
  };
  signal.subscribe(callback);
  return () => {
    signal.unsubscribe(callback);
    cleaner();
  };
};

export const pureImpl = <T>(value: T) => ({
  subscribe: () => {},
  unsubscribe: () => {},
  getValue: () => value,
});

export const mapImpl =
  <T, U>(fn: (value: T) => U) =>
  (signal: Signal<T>) => ({
    subscribe: (callback: () => void) => signal.subscribe(callback),
    unsubscribe: (callback: () => void) => signal.unsubscribe(callback),
    getValue: () => fn(signal.getValue()),
  });

export const applyImpl =
  <T, U>(signalFn: Signal<(value: T) => U>) =>
  (signal: Signal<T>) => ({
    subscribe: (callback: () => void) => {
      signal.subscribe(callback);
      signalFn.subscribe(callback);
    },
    unsubscribe: (callback: () => void) => {
      signal.unsubscribe(callback);
      signalFn.unsubscribe(callback);
    },
    getValue: () => signalFn.getValue()(signal.getValue()),
  });

export const readSignalImpl =
  <T>(signal: Signal<T>) =>
  () =>
    signal.getValue();
