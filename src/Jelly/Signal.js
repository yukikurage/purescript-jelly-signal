export const newChannelImpl = (initialValue) => () => ({
    subscriptions: new Set(),
    value: initialValue,
});
export const modifyChannelImpl = (channel) => (fn) => () => {
    channel.value = fn(channel.value);
    channel.subscriptions.forEach((subscription) => {
        subscription.cleaner();
        subscription.cleaner = subscription.callback(channel.value)();
    });
};
export const readChannel = (channel) => () => channel.value;
export const subscribeChannel = (channel) => (callback) => () => {
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
