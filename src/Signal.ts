type Channel<T> = {
  subscriptions: Set<Subscription<T>>;
  value: T;
};

type Subscription<T> = {
  callback: (value: T) => () => () => void;
  cleaner: () => void;
};

export const newChannelImpl =
  <T>(initialValue: T) =>
  (): Channel<T> => ({
    subscriptions: new Set(),
    value: initialValue,
  });

export const modifyChannelImpl =
  <T>(channel: Channel<T>) =>
  (fn: (value: T) => T) =>
  () => {
    channel.value = fn(channel.value);
    channel.subscriptions.forEach((subscription) => {
      subscription.cleaner();
      subscription.cleaner = subscription.callback(channel.value)();
    });
  };

export const readChannel =
  <T>(channel: Channel<T>) =>
  () =>
    channel.value;

export const subscribeChannel =
  <T>(channel: Channel<T>) =>
  (callback: (value: T) => () => () => void) =>
  (): (() => void) => {
    const subscription = {
      callback,
      cleaner: callback(channel.value)(),
    };
    channel.subscriptions.add(subscription);
    return () => {
      subscription.cleaner();
      channel.subscriptions.delete(subscription);
    };
  };
