//require('./node_modules/@fortawesome/fontawesome-free/js/all.min.js');
var Elm = require('./Main.elm');

var options, notification;

/** Service worker registration */
if ('serviceWorker' in navigator) {
	window.addEventListener('load', function() {
	  navigator.serviceWorker
	  	.register('/sw.js').then(reg => {
			console.log("SW Registered!");
		  });
	  
	});
  }


/** Elm initialization */

var app = Elm.Elm.Main.init({
  node: document.getElementById('elm')
});

app.ports.outgoingPort.subscribe(function(value){
	console.log(value);
	if (value.hasOwnProperty('GetCurrentPosition'))
		navigator.geolocation.getCurrentPosition(success, error, options);

	if (value.hasOwnProperty('StartAlarm'))
		playAlarm();

	if (value.hasOwnProperty('StopAlarm'))
		stopAlarm();

	if (value.hasOwnProperty('SaveData')){
		saveData(value.SaveData[0], value.SaveData[1]);
	}

	if (value.hasOwnProperty('GetData')){
		const data = getData(value.GetData);
		console.log(data);
		app.ports.incomingPort.send({ReceiveData: [value.GetData, data]});
	}
	
});

/** Navigation */

function success(rawPosition) {
	var coords = rawPosition.coords;

var rawAltitude = coords.altitude;
var rawAccuracy = coords.altitudeAccuracy;
var altitude =
	(rawAltitude === null || rawAccuracy === null)
		? null
		: { value: rawAltitude, accuracy: rawAccuracy };

var heading = coords.heading;
var speed = coords.speed;
var movement =
	(heading === null || speed === null)
		? null
		: 
			speed === 0
				? { Static: null }
				: { Moving: {speed: speed, degreesFromNorth: heading } }
		

const ret = {
	latitude: coords.latitude,
	longitude: coords.longitude,
		accuracy: coords.accuracy,
	altitude: altitude,
	movement: movement,
	timestamp: rawPosition.timestamp
};

app.ports.incomingPort.send({LocationUpdate: ret});
}

function error(err) {
  console.warn('ERROR(' + err.code + '): ' + err.message);
  app.ports.incomingPort.send({LocationUpdateError: err.message});
}

options = {
  enableHighAccuracy: false,
  timeout: 5000,
  maximumAge: 0
};

id = navigator.geolocation.watchPosition(success, error, options);

/** Alarm and notifications */

function playAlarm(){
	console.log("playAlarm");
	const alarm = document.getElementById("alarm");
	alarm.play();

	if(Notification.permission === 'default'){
		Notification.requestPermission()
		.then(
			showNotification
		)
	}else if (Notification.permission === 'granted'){
		showNotification('granted')
	}
}

function showNotification(permission){
	if(permission !== 'granted')
		return;


	try {
		throw "e";
		notification = new Notification("Proximity alert!", {
			requireInteraction: true
		});
		
		notification.onclick = () => stopAlarm();
		notification.onclose = () => stopAlarm();
		notification.onerror = () => stopAlarm();
	}catch(e){
		//exception means we are on Android without support for direct Notification construction
		//https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration/showNotification
		//https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerRegistration/showNotification#Browser_compatibility
		navigator.serviceWorker.getRegistration()
		.then(registration => {
			registration.showNotification("Proximity alert!")
			.then(_ => {
				registration.getNotifications()
				.then(notifications => {
					notifications.map(notification => {
						notification.onclick = () => stopAlarm();
						notification.onclose = () => stopAlarm();
						notification.onerror = () => stopAlarm();
					});
				});
			});
		})
		
	}
	
}

function stopAlarm(){
	const alarm = document.getElementById("alarm");
	alarm.pause();
	if (notification) {
		notification.close();
	}
	else
	{
		//probably, we are on android, when notifications constructed through Service Workers
		navigator.serviceWorker.getRegistration()
		.then(registration => registration.getNotifications()
			.then(notifications => {
				notifications.map(notification => notification.close());
			})
		);
	}
	app.ports.incomingPort.send({AlarmWasStopped: null});
}

/** LocalStorage */
function saveData(key, value){
	localStorage.setItem(key, JSON.stringify(value));
}

function getData(key){
	return JSON.parse(localStorage.getItem(key));
}