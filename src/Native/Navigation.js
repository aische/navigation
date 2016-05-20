var _elm_lang$navigation$Native_Navigation = function() {

function go(n)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		history.go(n);
		callback(_elm_lang$core$Native_Scheduler.succeed(getState()));
	});
}

function pushState(url)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		history.pushState({}, '', url);
		callback(_elm_lang$core$Native_Scheduler.succeed(getState()));
	});
}

function replaceState(url)
{
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
	{
		history.replaceState({}, '', url);
		callback(_elm_lang$core$Native_Scheduler.succeed(getState()));
	});
}

function getState()
{
	var location = document.location;

	return {
		length: history.length,
		location: {
			href: location.href,
  			host: location.host,
  			hostname: location.hostname,
  			protocol: location.protocol,
  			origin: location.origin,
  			port_: location.port,
  			pathname: location.pathname,
  			search: location.search,
  			hash: location.hash,
  			username: location.username,
  			password: location.password
		}
	};
}


return {
	go: go,
	pushState: pushState,
	replaceState: replaceState,
	getState: getState
};

}();