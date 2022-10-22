export const newSignal = (f) => () => ({
    depends: [],
    instantiate: () => {
        const callbacks = new Set();
        const instance = {
            callbacks,
            value: f((value) => () => {
                callbacks.forEach((callback) => callback(value));
                instance.value = value;
            })(),
        };
        return instance;
    },
});
export const pureImpl = (value) => ({
    depends: [],
    instantiate: () => ({ value, callbacks: new Set() }),
});
export const mapImpl = (f) => (signal) => ({
    depends: [signal],
    instantiate: ([depInstance]) => {
        const callbacks = new Set();
        const instance = {
            callbacks,
            value: f(depInstance.value),
        };
        depInstance.callbacks.add((value) => {
            const newValue = f(value);
            callbacks.forEach((callback) => callback(newValue));
            instance.value = newValue;
        });
        return instance;
    },
});
export const applyImpl = (f) => (signal) => ({
    depends: [f, signal],
    instantiate: ([fInstance, signalInstance]) => {
        const callbacks = new Set();
        const instance = {
            callbacks,
            value: fInstance.value(signalInstance.value),
        };
        fInstance.callbacks.add((f) => {
            const newValue = f(signalInstance.value);
            callbacks.forEach((callback) => callback(newValue));
            instance.value = newValue;
        });
        signalInstance.callbacks.add((value) => {
            const newValue = fInstance.value(value);
            callbacks.forEach((callback) => callback(newValue));
            instance.value = newValue;
        });
        return instance;
    },
});
export const mergeWith = (f) => (signal1) => (signal2) => ({
    depends: [signal1, signal2],
    instantiate: ([signal1Instance, signal2Instance]) => {
        const callbacks = new Set();
        const instance = {
            callbacks,
            value: f(signal1Instance.value)(signal2Instance.value),
        };
        signal1Instance.callbacks.add((value) => {
            callbacks.forEach((callback) => callback(value));
            instance.value = value;
        });
        signal2Instance.callbacks.add((value) => {
            callbacks.forEach((callback) => callback(value));
            instance.value = value;
        });
        return instance;
    },
});
export const runSignal = (signal) => () => {
    const instances = new Map();
    const typedGet = (signal) => instances.get(signal);
    const go = (signal) => {
        const check = typedGet(signal);
        if (check !== undefined)
            return check;
        const instance = signal.instantiate(signal.depends.map(go));
        instances.set(signal, instance);
        return instance;
    };
    const instance = go(signal);
    instance.value();
    instance.callbacks.add((value) => value());
};
