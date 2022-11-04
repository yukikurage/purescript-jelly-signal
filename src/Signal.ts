type Channel<T> = {
  subscriptions: Set<Subscription<T>>;
  eq: (a: T) => (b: T) => boolean;
  value: T;
};

type Subscription<T> = {
  callback: (value: T) => () => () => void;
  cleaner: () => void;
};

export const newChannelEqImpl =
  <T>(eq: (a: T) => (b: T) => boolean) =>
  (value: T) =>
  (): Channel<T> => ({
    subscriptions: new Set(),
    eq,
    value,
  });

export const modifyChannelImpl =
  <T>(channel: Channel<T>) =>
  (fn: (value: T) => T) =>
  () => {
    if (channel.eq(fn(channel.value))(channel.value)) return;
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
