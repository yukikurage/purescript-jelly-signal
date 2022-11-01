export const newChannelImpl = (initialValue) => () => ({
    subscriptions: new Set(),
    value: initialValue,
});
export const mutateImpl = (fn) => (channel) => () => {
    channel.value = fn(channel.value);
    channel.subscriptions.forEach((callback) => callback());
};
export const sendImpl = (value) => (channel) => () => {
    channel.value = value;
    channel.subscriptions.forEach((callback) => callback());
};
export const subscribe = (channel) => ({
    subscribe: (callback) => {
        channel.subscriptions.add(callback);
    },
    unsubscribe: (callback) => {
        channel.subscriptions.delete(callback);
    },
    getValue: () => channel.value,
});
export const runSignalImpl = (signal) => () => {
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
export const pureImpl = (value) => ({
    subscribe: () => { },
    unsubscribe: () => { },
    getValue: () => value,
});
export const mapImpl = (fn) => (signal) => ({
    subscribe: (callback) => signal.subscribe(callback),
    unsubscribe: (callback) => signal.unsubscribe(callback),
    getValue: () => fn(signal.getValue()),
});
export const applyImpl = (signalFn) => (signal) => ({
    subscribe: (callback) => {
        signal.subscribe(callback);
        signalFn.subscribe(callback);
    },
    unsubscribe: (callback) => {
        signal.unsubscribe(callback);
        signalFn.unsubscribe(callback);
    },
    getValue: () => signalFn.getValue()(signal.getValue()),
});
export const readSignalImpl = (signal) => () => signal.getValue();
