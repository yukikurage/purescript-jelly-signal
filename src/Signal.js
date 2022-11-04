export const newChannelEqImpl = (eq) => (value) => () => ({
    subscriptions: new Set(),
    eq,
    value,
});
export const modifyChannelImpl = (channel) => (fn) => () => {
    if (channel.eq(fn(channel.value))(channel.value))
        return;
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
