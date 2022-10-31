type Channel<T> = Set<(mutate: (prevValue: T) => T) => void>;

type Signal<T, U = unknown> =
  | {
      type: "subscription";
      channel: Channel<T>;
      getInitialValue: () => T;
    }
  | {
      type: "pure";
      value: T;
    }
  | {
      type: "map";
      fn: (value: U) => T;
      signal: Signal<U>;
    }
  | {
      type: "apply";
      signalFn: Signal<(value: U) => T>;
      signal: Signal<U>;
    }
  | {
      type: "merge";
      signalA: Signal<T>;
      signalB: Signal<T>;
      fn: (a: T, b: T) => T;
    }
  | {
      type: "join";
      signal: Signal<Signal<T>>;
    }
  | {
      type: "foldp";
      signal: Signal<U>;
      fn: (value: U) => (acc: T) => T;
      initialValue: T;
    };

type Instance<T> = { callbacks: Set<() => void>; value: T };

const instantiate = <T>(
  getInstance: <U>(signal: Signal<U>) => Instance<U> | undefined,
  addInstance: <U>(signal: Signal<U>, instance: Instance<U>) => void,
  signal: Signal<T>
): Instance<T> => {
  const existInstance = getInstance(signal);
  if (existInstance) {
    return existInstance;
  }
  const callbacks = new Set<() => void>();
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
      const initialInstance = instantiate(
        getInstance,
        addInstance,
        depInstance.value
      );
      const instance = {
        callbacks,
        value: initialInstance.value,
      };
      depInstance.callbacks.add(() => {
        const nextInstance = instantiate(
          getInstance,
          addInstance,
          depInstance.value
        );
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

export const runSignal = (signal: Signal<() => void>) => () => {
  const instances = new Map<Signal<unknown>, Instance<unknown>>();
  const getInstance = <T>(signal: Signal<T>) =>
    instances.get(signal as any) as Instance<T> | undefined;
  const addInstance = <T>(signal: Signal<T>, instance: Instance<T>) =>
    instances.set(signal as any, instance);
  const instance = instantiate(getInstance, addInstance, signal);
  instance.value();
  instance.callbacks.add(() => instance.value());
};

export const newChannel = <T>(): Channel<T> => new Set();

export const send =
  <T>(channel: Channel<T>) =>
  (value: T) =>
  () => {
    channel.forEach((callback) => callback(() => value));
  };

export const mutate =
  <T>(channel: Channel<T>) =>
  (mutate: (prevValue: T) => T) =>
  () => {
    channel.forEach((callback) => callback(mutate));
  };

export const subscribe =
  <T>(channel: Channel<T>) =>
  (getInitialValue: () => T): Signal<T> => ({
    type: "subscription",
    channel,
    getInitialValue,
  });

export const pureImpl = <T>(value: T): Signal<T> => ({
  type: "pure",
  value,
});

export const mapImpl =
  <T, U>(fn: (value: U) => T) =>
  (signal: Signal<U>): Signal<T, U> => ({
    type: "map",
    fn,
    signal,
  });

export const applyImpl =
  <T, U>(signalFn: Signal<(value: U) => T>) =>
  (signal: Signal<U>): Signal<T, U> => ({
    type: "apply",
    signalFn,
    signal,
  });

export const joinImpl = <T>(signal: Signal<Signal<T>>): Signal<T> => ({
  type: "join",
  signal,
});
export const mergeWith =
  <T>(signalA: Signal<T>) =>
  (signalB: Signal<T>) =>
  (fn: (a: T, b: T) => T): Signal<T> => ({
    type: "merge",
    signalA,
    signalB,
    fn,
  });

export const foldp =
  <T, U>(fn: (value: U) => (acc: T) => T) =>
  (initialValue: T) =>
  (signal: Signal<U>): Signal<T, U> => ({
    type: "foldp",
    fn,
    initialValue,
    signal,
  });
