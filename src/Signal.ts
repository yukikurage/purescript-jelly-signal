type Effect<T> = () => T;

type Instance<T> = { callbacks: Set<(value: T) => void>; value: T };

type Signal<T, U extends unknown[] = unknown[]> = {
  depends: { [K in keyof U]: Signal<U[K]> };
  instantiate: (dependsInstances: {
    [K in keyof U]: Instance<U[K]>;
  }) => Instance<T>;
};

export const newSignal =
  <T>(
    f: (callback: (value: T) => Effect<void>) => Effect<T>
  ): Effect<Signal<T>> =>
  () => ({
    depends: [],
    instantiate: () => {
      const callbacks = new Set<(value: T) => Effect<void>>();
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

export const pureImpl = <T>(value: T): Signal<T> => ({
  depends: [],
  instantiate: () => ({ value, callbacks: new Set() }),
});

export const mapImpl =
  <T, U>(f: (value: T) => U) =>
  (signal: Signal<T>): Signal<U, [T]> => ({
    depends: [signal],
    instantiate: ([depInstance]) => {
      const callbacks = new Set<(value: U) => Effect<void>>();
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

export const applyImpl =
  <T, U>(f: Signal<(value: T) => U>) =>
  (signal: Signal<T>): Signal<U, [(value: T) => U, T]> => ({
    depends: [f, signal],
    instantiate: ([fInstance, signalInstance]) => {
      const callbacks = new Set<(value: U) => Effect<void>>();
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

export const mergeWith =
  <T>(f: (init1: T) => (init2: T) => T) =>
  (signal1: Signal<T>) =>
  (signal2: Signal<T>): Signal<T, [T, T]> => ({
    depends: [signal1, signal2],
    instantiate: ([signal1Instance, signal2Instance]) => {
      const callbacks = new Set<(value: T) => Effect<void>>();
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

export const runSignal =
  (signal: Signal<Effect<void>>): Effect<void> =>
  () => {
    const instances = new Map<Signal<any>, Instance<any>>();
    const typedGet = <T>(signal: Signal<T>) =>
      instances.get(signal) as Instance<T> | undefined;
    const go = <T>(signal: Signal<T>): Instance<T> => {
      const check = typedGet(signal);
      if (check !== undefined) return check;
      const instance = signal.instantiate(signal.depends.map(go));
      instances.set(signal, instance);
      return instance;
    };
    const instance = go(signal);
    instance.value();
    instance.callbacks.add((value) => value());
    return typedGet;
  };

export const getImpl =
  <U, T>(just: (value: T) => U) =>
  (nothing: U) =>
  (typedGet: (signal: Signal<T, unknown[]>) => Instance<T> | undefined) =>
  (signal: Signal<T>) =>
  () => {
    const instance = typedGet(signal);
    if (instance === undefined) return nothing;
    return just(instance.value);
  };
