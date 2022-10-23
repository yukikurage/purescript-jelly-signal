export const newSignal = (f) => () => ({
    depends: [],
    instantiate: () => {
        const callbacks = new Set();
        const instance = {
            callbacks,
            value: f((value) => () => {
                instance.value = value;
                callbacks.forEach((callback) => callback());
            })(),
        };
        return instance;
    },
});
export const mapMany = (f) => (signals) => ({
    depends: signals,
    instantiate: (depInstances) => {
        const callbacks = new Set();
        const instance = {
            callbacks,
            value: f(depInstances.map((depInstance) => depInstance.value)),
        };
        depInstances.forEach((depInstance, index) => {
            depInstance.callbacks.add(() => {
                const newValue = f(depInstances.map((depInstance) => depInstance.value));
                instance.value = newValue;
                callbacks.forEach((callback) => callback());
            });
        });
        return instance;
    },
});
export const map0 = (v) => mapMany(() => v)([]);
export const map1 = (f) => (signal) => mapMany((deps) => f(deps[0]))([signal]);
export const map2 = (f) => (signal1) => (signal2) => mapMany((deps) => f(deps[0])(deps[1]))([signal1, signal2]);
export const map3 = (f) => (signal1) => (signal2) => (signal3) => mapMany((deps) => f(deps[0])(deps[1])(deps[2]))([
    signal1,
    signal2,
    signal3,
]);
export const map4 = (f) => (signal1) => (signal2) => (signal3) => (signal4) => mapMany((deps) => f(deps[0])(deps[1])(deps[2])(deps[3]))([
    signal1,
    signal2,
    signal3,
    signal4,
]);
export const map5 = (f) => (signal1) => (signal2) => (signal3) => (signal4) => (signal5) => mapMany((deps) => f(deps[0])(deps[1])(deps[2])(deps[3])(deps[4]))([signal1, signal2, signal3, signal4, signal5]);
export const mergeManyWith = (f) => (signals) => ({
    depends: signals,
    instantiate: (depInstances) => {
        const callbacks = new Set();
        const instance = {
            callbacks,
            value: f(depInstances.map((depInstance) => depInstance.value)),
        };
        depInstances.forEach((depInstance) => {
            depInstance.callbacks.add(() => {
                const newValue = depInstance.value;
                instance.value = newValue;
                callbacks.forEach((callback) => callback());
            });
        });
        return instance;
    },
});
export const mergeManyImpl = (just) => (nothing) => (signals) => {
    const head = signals[0];
    if (head === undefined) {
        return nothing;
    }
    return just({
        depends: signals,
        instantiate: (depInstances) => {
            const callbacks = new Set();
            const instance = {
                callbacks,
                value: depInstances[0].value,
            };
            depInstances.forEach((depInstance) => {
                depInstance.callbacks.add(() => {
                    const newValue = depInstance.value;
                    instance.value = newValue;
                    callbacks.forEach((callback) => callback());
                });
            });
            return instance;
        },
    });
};
export const mergeWith = (f) => (signal1) => (signal2) => mergeManyWith((deps) => f(deps[0])(deps[1]))([signal1, signal2]);
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
    instance.callbacks.add(() => instance.value());
};
export const foldp = (f) => (init) => (signal) => ({
    depends: [signal],
    instantiate: ([depInstance]) => {
        const callbacks = new Set();
        const instance = {
            callbacks,
            value: init,
        };
        depInstance.callbacks.add(() => {
            const newValue = f(depInstance.value)(instance.value);
            instance.value = newValue;
            callbacks.forEach((callback) => callback());
        });
        return instance;
    },
});
