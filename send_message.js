/** This file produces message for main.js whenever there was (ServiceWorker's) notification event */
self.onnotificationclick = swNotificationClickHandler;
self.onnotificationclose = swNotificationClickHandler;

//http://craig-russell.co.uk/2016/01/29/service-worker-messaging.html

function send_message_to_client(client, msg){
    return new Promise(function(resolve, reject){
        var msg_chan = new MessageChannel();

        msg_chan.port1.onmessage = function(event){
            if(event.data.error){
                reject(event.data.error);
            }else{
                resolve(event.data);
            }
        };

        client.postMessage("SW Says: '"+msg+"'", [msg_chan.port2]);
    });
}

function send_message_to_all_clients(msg){
    //https://stackoverflow.com/questions/35100759/serviceworkers-focus-tab-clients-is-empty-on-notificationclick
    clients.matchAll({includeUncontrolled: true, type: 'window'}).then(clients => {
        clients.forEach(client => {
            send_message_to_client(client, msg);
        })
    })
}

function swNotificationClickHandler(event){
    send_message_to_all_clients("stopAlarm");
}