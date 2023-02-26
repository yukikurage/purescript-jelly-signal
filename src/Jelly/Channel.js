export const newChannel = () => new Set();
export const send = (channel) => (value) => () => {
    channel.forEach((f) => f(value));
};
export const subscribe = (channel) => (f) => {
    channel.add(f);
    return () => channel.delete(f);
};
