"use strict";

// const { createBrowserHistory } = require('history');

let history = { push: () => {} };

exports.init = makeRoute => () => {
	/*
	console.log('oh');
	history = createBrowserHistory();
	const initRoute = makeRoute(history.location.pathname);
	const historyEvent = frp.mkEvent();
	history.listen((location) => {
		const currRoute = makeRoute(location.pathname);
		frp.push(currRoute)(historyEvent)();
	});
	const res = frp.s_buildImpl(frp.s_fromImpl(historyEvent)(initRoute))().signal;
	console.log(res);
	return res;
	*/
};

exports.push = s => () => history.push(s);
