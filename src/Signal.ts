type Effect<T> = () => T;

type Instance<T> = { callbacks: Set<() => void>; value: T };

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
      const callbacks = new Set<() => Effect<void>>();
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

export const mapMany =
  <U extends unknown[], V>(f: (deps: U) => V) =>
  (signals: { [K in keyof U]: Signal<U[K]> }): Signal<V, U> => ({
    depends: signals,
    instantiate: (depInstances) => {
      const callbacks = new Set<() => Effect<void>>();
      const instance = {
        callbacks,
        value: f(depInstances.map((depInstance) => depInstance.value) as U),
      };
      depInstances.forEach((depInstance) => {
        depInstance.callbacks.add(() => {
          const newValue = f(
            depInstances.map((depInstance) => depInstance.value) as U
          );
          instance.value = newValue;
          callbacks.forEach((callback) => callback());
        });
      });
      return instance;
    },
  });

export const map0 = <T>(v: T) => mapMany(() => v)([]);

export const map1 =
  <T, U>(f: (value: T) => U) =>
  (signal: Signal<T>) =>
    mapMany((deps: [T]) => f(deps[0]))([signal]);

export const map2 =
  <T1, T2, U>(f: (value1: T1) => (value2: T2) => U) =>
  (signal1: Signal<T1>) =>
  (signal2: Signal<T2>) =>
    mapMany((deps: [T1, T2]) => f(deps[0])(deps[1]))([signal1, signal2]);

export const map3 =
  <T1, T2, T3, U>(f: (value1: T1) => (value2: T2) => (value3: T3) => U) =>
  (signal1: Signal<T1>) =>
  (signal2: Signal<T2>) =>
  (signal3: Signal<T3>) =>
    mapMany((deps: [T1, T2, T3]) => f(deps[0])(deps[1])(deps[2]))([
      signal1,
      signal2,
      signal3,
    ]);

export const map4 =
  <T1, T2, T3, T4, U>(
    f: (value1: T1) => (value2: T2) => (value3: T3) => (value4: T4) => U
  ) =>
  (signal1: Signal<T1>) =>
  (signal2: Signal<T2>) =>
  (signal3: Signal<T3>) =>
  (signal4: Signal<T4>) =>
    mapMany((deps: [T1, T2, T3, T4]) => f(deps[0])(deps[1])(deps[2])(deps[3]))([
      signal1,
      signal2,
      signal3,
      signal4,
    ]);

export const map5 =
  <T1, T2, T3, T4, T5, U>(
    f: (
      value1: T1
    ) => (value2: T2) => (value3: T3) => (value4: T4) => (value5: T5) => U
  ) =>
  (signal1: Signal<T1>) =>
  (signal2: Signal<T2>) =>
  (signal3: Signal<T3>) =>
  (signal4: Signal<T4>) =>
  (signal5: Signal<T5>) =>
    mapMany((deps: [T1, T2, T3, T4, T5]) =>
      f(deps[0])(deps[1])(deps[2])(deps[3])(deps[4])
    )([signal1, signal2, signal3, signal4, signal5]);

export const mergeManyWith =
  <U>(f: (deps: U[]) => U) =>
  (signals: Signal<U>[]): Signal<U, U[]> => ({
    depends: signals,
    instantiate: (depInstances) => {
      const callbacks = new Set<() => Effect<void>>();
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

export const mergeManyImpl =
  <U, MaybeS>(just: (v: Signal<U, U[]>) => MaybeS) =>
  (nothing: MaybeS) =>
  (signals: Signal<U>[]): MaybeS => {
    const head = signals[0];
    if (head === undefined) {
      return nothing;
    }
    return just({
      depends: signals,
      instantiate: (depInstances) => {
        const callbacks = new Set<() => Effect<void>>();
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

export const mergeWith =
  <T>(f: (dep1: T) => (dep2: T) => T) =>
  (signal1: Signal<T>) =>
  (signal2: Signal<T>) =>
    mergeManyWith((deps: T[]) => f(deps[0])(deps[1]))([signal1, signal2]);

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
    instance.callbacks.add(() => instance.value());
  };

export const foldp =
  <T, U>(f: (value: T) => (acc: U) => U) =>
  (init: U) =>
  (signal: Signal<T>): Signal<U, [T]> => ({
    depends: [signal],
    instantiate: ([depInstance]) => {
      const callbacks = new Set<() => Effect<void>>();
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
