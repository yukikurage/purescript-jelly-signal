const instantiate = (getInstance, addInstance, signal) => {
    const existInstance = getInstance(signal);
    if (existInstance) {
        return existInstance;
    }
    const callbacks = new Set();
    switch (signal.type) {
        case "subscription": {
            const { channel, getInitialValue } = signal;
            const instance = { callbacks, value: getInitialValue() };
            channel.add((mutate) => {
                instance.value = mutate(instance.value);
                callbacks.forEach((callback) => callback());
            });
            addInstance(signal, instance);
            return instance;
        }
        case "pure": {
            const instance = { callbacks, value: signal.value };
            addInstance(signal, instance);
            return instance;
        }
        case "map": {
            const { fn, signal: depSignal } = signal;
            const depInstance = instantiate(getInstance, addInstance, depSignal);
            const instance = {
                callbacks,
                value: fn(depInstance.value),
            };
            depInstance.callbacks.add(() => {
                instance.value = fn(depInstance.value);
                callbacks.forEach((callback) => callback());
            });
            addInstance(signal, instance);
            return instance;
        }
        case "apply": {
            const { signalFn, signal: depSignal } = signal;
            const depInstance = instantiate(getInstance, addInstance, depSignal);
            const fnInstance = instantiate(getInstance, addInstance, signalFn);
            const instance = {
                callbacks,
                value: fnInstance.value(depInstance.value),
            };
            depInstance.callbacks.add(() => {
                instance.value = fnInstance.value(depInstance.value);
                callbacks.forEach((callback) => callback());
            });
            fnInstance.callbacks.add(() => {
                instance.value = fnInstance.value(depInstance.value);
                callbacks.forEach((callback) => callback());
            });
            addInstance(signal, instance);
            return instance;
        }
        case "merge": {
            const { signalA, signalB, fn } = signal;
            const depInstanceA = instantiate(getInstance, addInstance, signalA);
            const depInstanceB = instantiate(getInstance, addInstance, signalB);
            const instance = {
                callbacks,
                value: fn(depInstanceA.value, depInstanceB.value),
            };
            depInstanceA.callbacks.add(() => {
                instance.value = depInstanceA.value;
                callbacks.forEach((callback) => callback());
            });
            depInstanceB.callbacks.add(() => {
                instance.value = depInstanceB.value;
                callbacks.forEach((callback) => callback());
            });
            addInstance(signal, instance);
            return instance;
        }
        case "join": {
            const { signal: depSignal } = signal;
            const depInstance = instantiate(getInstance, addInstance, depSignal);
            const initialInstance = instantiate(getInstance, addInstance, depInstance.value);
            const instance = {
                callbacks,
                value: initialInstance.value,
            };
            depInstance.callbacks.add(() => {
                const nextInstance = instantiate(getInstance, addInstance, depInstance.value);
                instance.value = nextInstance.value;
                nextInstance.callbacks.add(() => {
                    instance.value = nextInstance.value;
                });
                callbacks.forEach((callback) => callback());
            });
            initialInstance.callbacks.add(() => {
                instance.value = initialInstance.value;
                callbacks.forEach((callback) => callback());
            });
            addInstance(signal, instance);
            return instance;
        }
        case "foldp": {
            const { signal: depSignal, fn, initialValue } = signal;
            const depInstance = instantiate(getInstance, addInstance, depSignal);
            const instance = {
                callbacks,
                value: initialValue,
            };
            depInstance.callbacks.add(() => {
                instance.value = fn(depInstance.value)(instance.value);
                callbacks.forEach((callback) => callback());
            });
            addInstance(signal, instance);
            return instance;
        }
    }
};
export const runSignal = (signal) => () => {
    const instances = new Map();
    const getInstance = (signal) => instances.get(signal);
    const addInstance = (signal, instance) => instances.set(signal, instance);
    const instance = instantiate(getInstance, addInstance, signal);
    instance.value();
    instance.callbacks.add(() => instance.value());
};
export const newChannel = () => new Set();
export const send = (channel) => (value) => () => {
    channel.forEach((callback) => callback(() => value));
};
export const mutate = (channel) => (mutate) => () => {
    channel.forEach((callback) => callback(mutate));
};
export const subscribe = (channel) => (getInitialValue) => ({
    type: "subscription",
    channel,
    getInitialValue,
});
export const pureImpl = (value) => ({
    type: "pure",
    value,
});
export const mapImpl = (fn) => (signal) => ({
    type: "map",
    fn,
    signal,
});
export const applyImpl = (signalFn) => (signal) => ({
    type: "apply",
    signalFn,
    signal,
});
export const joinImpl = (signal) => ({
    type: "join",
    signal,
});
export const mergeWith = (signalA) => (signalB) => (fn) => ({
    type: "merge",
    signalA,
    signalB,
    fn,
});
export const foldp = (fn) => (initialValue) => (signal) => ({
    type: "foldp",
    fn,
    initialValue,
    signal,
});
