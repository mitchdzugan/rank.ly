"use strict";

const { createBrowserHistory } = require('history');

let history = { push: () => {} };

exports.init = eff_sigBuilder => s_from => eventPush => historyEvent => makeRoute => () => {
	history = createBrowserHistory();
	const initRoute = makeRoute(history.location.pathname);
	history.listen((location) => {
		const currRoute = makeRoute(location.pathname);
		eventPush(currRoute)(historyEvent)();
	});
	return eff_sigBuilder(s_from(historyEvent)(initRoute))().signal;
};

exports.push = s => () => history.push(s);
