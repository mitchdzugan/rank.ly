var Main = require('../rankly-core/output/Client.Main');

function main () {
  Main.main();
}

// HMR stuff
// For more info see: https://parceljs.org/hmr.html
/*
	if (module.hot) {
  module.hot.accept(function () {
  console.log('Reloaded, running main again');
  main();
  });
	}
*/

console.log('Starting app');

main();
