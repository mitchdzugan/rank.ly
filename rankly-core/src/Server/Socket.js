"use strict";

// const IO = require('socket.io');

exports.attachSocketIO = (server) => () => {}; // IO(server);
exports.on = (io) => (type) => handler => () => {
	/*
    io.on(type, (socket) => {
        handler(socket)();
    });
		*/
};
