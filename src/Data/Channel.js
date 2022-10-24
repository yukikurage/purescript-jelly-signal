export const newChannel = () => new Set();
export const sendChannel = (channel) => (value) => () => {
    channel.forEach((f) => f(value)());
};
export const subscribeChannel = (channel) => (callback) => () => {
    const f = (value) => callback(value);
    channel.add(f);
    return () => {
        channel.delete(f);
    };
};
